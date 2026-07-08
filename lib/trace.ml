open Syntax
open Domain
open Abs_dom

module VisitSet = Set.Make (struct
  type t = string * ProgramPoint.t

  let compare = compare
end)

type role = Base | Index | HeapValue | Dependency

type trace_node = {
  pp : ProgramPoint.t;
  line : int option;
  expr : string;
  subject : string;
  role : role;
  value : Abs_Val.t;
  pps : PPSet.t;
  is_handler : bool;
  children : trace_node list;
}

type trace_chain = {
  error : Error.t;
  err_line : int option;
  err_expr : string;
  index_expr : string;
  base_trace : trace_node list;
  index_trace : trace_node list;
  handler_iids : int list;
}

let max_depth = 15

let is_handler_pp : ProgramPoint.t -> bool = function
  | ProgramPoint.Label (Exp.Lbl.Handler _) -> true
  | _ -> false

let handler_iid_of_pp : ProgramPoint.t -> int option = function
  | ProgramPoint.Label (Exp.Lbl.Handler (iid, _)) -> Some iid
  | _ -> None

let role_name = function
  | Base -> "base"
  | Index -> "index"
  | HeapValue -> "heap-value"
  | Dependency -> "dependency"

let rec fmt_exp : Exp.t -> string = function
  | Exp.Unit -> "unit"
  | Exp.Int n -> string_of_int n
  | Exp.Var x -> x
  | Exp.AddrOf x -> "&" ^ x
  | Exp.Enable -> "enable"
  | Exp.Disable -> "disable"
  | Exp.Bop (bop, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (fmt_exp e1.exp) (Exp.string_of_bop bop)
        (fmt_exp e2.exp)
  | Exp.Deref (base_e, idx_e) ->
      Printf.sprintf "*%s[%s]" (fmt_exp base_e.exp) (fmt_exp idx_e.exp)
  | Exp.Malloc (n_e, init_e) ->
      Printf.sprintf "malloc(%s, %s)" (fmt_exp n_e.exp) (fmt_exp init_e.exp)
  | Exp.Assign (lhs, rhs) ->
      Printf.sprintf "%s := %s" (fmt_exp lhs.exp) (fmt_exp rhs.exp)
  | Exp.Seq (e1, e2) ->
      Printf.sprintf "%s; %s" (fmt_exp e1.exp) (fmt_exp e2.exp)
  | Exp.If (c, t, f) ->
      Printf.sprintf "if %s then %s else %s" (fmt_exp c.exp) (fmt_exp t.exp)
        (fmt_exp f.exp)
  | Exp.While (_, c, b) ->
      Printf.sprintf "while %s do (%s)" (fmt_exp c.exp) (fmt_exp b.exp)

let build_lbl_table (pgm : Program.t) : Exp.lbl_t Exp.Lbl_map.t =
  let rec walk ({ lbl; exp; line } : Exp.lbl_t) tbl =
    let labeled = ({ lbl; exp; line } : Exp.lbl_t) in
    let tbl = Exp.Lbl_map.add (Either.Left lbl) labeled tbl in
    match exp with
    | Exp.Unit | Exp.Int _ | Exp.Var _ | Exp.AddrOf _ | Exp.Enable | Exp.Disable
      ->
        tbl
    | Exp.Bop (_, e1, e2)
    | Exp.Deref (e1, e2)
    | Exp.Malloc (e1, e2)
    | Exp.Assign (e1, e2)
    | Exp.Seq (e1, e2) ->
        tbl |> walk e1 |> walk e2
    | Exp.If (e1, e2, e3) -> tbl |> walk e1 |> walk e2 |> walk e3
    | Exp.While (glbl, e1, e2) ->
        let labeled = ({ lbl = glbl; exp; line } : Exp.lbl_t) in
        let tbl = Exp.Lbl_map.add (Either.Right glbl) labeled tbl in
        tbl |> walk e1 |> walk e2
  in
  let tbl = Exp.Lbl_map.empty |> walk pgm.Program.global |> walk pgm.main in
  List.fold_left
    (fun acc (h : Handler.t) -> walk h.body acc)
    tbl pgm.Program.handler

let lookup_lbl_t (tbl : Exp.lbl_t Exp.Lbl_map.t) (pp : ProgramPoint.t) :
    Exp.lbl_t option =
  match pp with
  | ProgramPoint.Unit -> None
  | ProgramPoint.Label lbl -> Exp.Lbl_map.find_opt (Either.Left lbl) tbl

let loc_candidates (x : string) : Abs_Loc.t list =
  [ Abs_Loc.get x; Abs_Loc.get (x ^ "#") ]

let find_scalar (snapshot : Abs_Mem.t) (x : string) : Abs_Val.t * PPSet.t =
  List.fold_left
    (fun (acc_v, acc_pps) loc ->
      let v, pps = Abs_Mem.find snapshot loc in
      (Abs_Val.join acc_v v, PPSet.union acc_pps pps))
    (Abs_Val.bot, PPSet.empty) (loc_candidates x)

let int_of_value ((itv, _) : Abs_Val.t) : Itv.t = itv
let loc_of_value ((_, loc) : Abs_Val.t) : Abs_LocSet.t = loc

let loc_overlap (l1 : Abs_Loc.t) (l2 : Abs_Loc.t) : bool =
  match (l1, l2) with
  | Abs_Loc.Bot, _ | _, Abs_Loc.Bot -> false
  | Abs_Loc.Top, _ | _, Abs_Loc.Top -> true
  | ( Abs_Loc.AVarLoc { id = id1; offset = off1 },
      Abs_Loc.AVarLoc { id = id2; offset = off2 } ) ->
      Var.compare id1 id2 = 0 && Itv.is_overlap off1 off2
  | ( Abs_Loc.AHeapLoc { lbl = lbl1; offset = off1 },
      Abs_Loc.AHeapLoc { lbl = lbl2; offset = off2 } ) ->
      Exp.Lbl.compare lbl1 lbl2 = 0 && Itv.is_overlap off1 off2
  | _ -> false

let join_cells (snapshot : Abs_Mem.t) (targets : Abs_LocSet.t) :
    Abs_Val.t * PPSet.t =
  Abs_LocSet.fold
    (fun target (acc_v, acc_pps) ->
      Abs_Mem.fold
        (fun loc (v, pps) (inner_v, inner_pps) ->
          if loc_overlap target loc then
            (Abs_Val.join inner_v v, PPSet.union inner_pps pps)
          else (inner_v, inner_pps))
        snapshot (acc_v, acc_pps))
    targets (Abs_Val.bot, PPSet.empty)

let rec eval_value (snapshot : Abs_Mem.t) (e : Exp.lbl_t) : Abs_Val.t * PPSet.t
    =
  match e.exp with
  | Exp.Unit -> ((Itv.alpha 0, Abs_LocSet.bot), PPSet.empty)
  | Exp.Int n -> ((Itv.alpha n, Abs_LocSet.bot), PPSet.empty)
  | Exp.Var x -> find_scalar snapshot x
  | Exp.AddrOf x ->
      ((Itv.bot, Abs_LocSet.singleton (Abs_Loc.get x)), PPSet.empty)
  | Exp.Bop (bop, e1, e2) ->
      let v1, pps1 = eval_value snapshot e1 in
      let v2, pps2 = eval_value snapshot e2 in
      let i1 = int_of_value v1 in
      let i2 = int_of_value v2 in
      let itv =
        match bop with
        | Exp.Plus -> Itv.add i1 i2
        | Exp.Minus -> Itv.add i1 (Itv.neg i2)
        | Exp.Times -> Itv.mul i1 i2
        | Exp.Lt -> Itv.lt i1 i2
        | Exp.Le -> Itv.le i1 i2
        | Exp.Gt -> Itv.gt i1 i2
        | Exp.Ge -> Itv.ge i1 i2
        | Exp.Ne -> Itv.ne i1 i2
        | Exp.And -> Itv.and_ i1 i2
        | Exp.Or -> Itv.or_ i1 i2
        | Exp.Eq -> Itv.top
      in
      ((itv, Abs_LocSet.bot), PPSet.union pps1 pps2)
  | Exp.Deref (base_e, idx_e) -> resolve_deref_cell snapshot base_e idx_e
  | Exp.Malloc _ | Exp.Assign _ | Exp.Seq _ | Exp.If _ | Exp.While _
  | Exp.Enable | Exp.Disable ->
      (Abs_Val.bot, PPSet.empty)

and eval_index (snapshot : Abs_Mem.t) (e : Exp.lbl_t) : Itv.t * PPSet.t =
  let v, pps = eval_value snapshot e in
  (int_of_value v, pps)

and eval_base_loc (snapshot : Abs_Mem.t) (e : Exp.lbl_t) :
    Abs_LocSet.t * PPSet.t =
  match e.exp with
  | Exp.AddrOf x -> (Abs_LocSet.singleton (Abs_Loc.get x), PPSet.empty)
  | _ ->
      let v, pps = eval_value snapshot e in
      (loc_of_value v, pps)

and resolve_deref_cell (snapshot : Abs_Mem.t) (base_e : Exp.lbl_t)
    (idx_e : Exp.lbl_t) : Abs_Val.t * PPSet.t =
  let base_loc, _ = eval_base_loc snapshot base_e in
  let idx_itv, _ = eval_index snapshot idx_e in
  let target = Abs_LocSet.offset_add base_loc idx_itv in
  join_cells snapshot target

let assignment_rhs_for_subject (subject : string) (lt : Exp.lbl_t) :
    Exp.lbl_t option =
  match lt.exp with
  | Exp.Assign ({ exp = Exp.Var lhs; _ }, rhs) when lhs = subject -> Some rhs
  | Exp.Assign ({ exp = Exp.Var lhs; _ }, rhs)
    when lhs ^ "#" = subject || lhs = subject ^ "#" ->
      Some rhs
  | _ -> None

let heap_write_parts (lt : Exp.lbl_t) :
    (Exp.lbl_t * Exp.lbl_t * Exp.lbl_t) option =
  match lt.exp with
  | Exp.Assign ({ exp = Exp.Deref (base_e, idx_e); _ }, rhs) ->
      Some (base_e, idx_e, rhs)
  | _ -> None

let subject_key role subject =
  match role with
  | HeapValue -> "heap:" ^ subject
  | Base | Index | Dependency -> "scalar:" ^ subject

let node_site tbl pp =
  let lbl_t_opt = lookup_lbl_t tbl pp in
  let line = Option.bind lbl_t_opt (fun lt -> lt.line) in
  let expr =
    match lbl_t_opt with Some lt -> fmt_exp lt.exp | None -> "<unknown>"
  in
  (lbl_t_opt, line, expr)

let rec trace_scalar (name : string) (pps : PPSet.t) (role : role)
    (asem : Abs_Sem.t) (tbl : Exp.lbl_t Exp.Lbl_map.t) (visited : VisitSet.t)
    (depth : int) : trace_node list =
  if depth = 0 || PPSet.is_empty pps then []
  else
    PPSet.fold
      (fun pp acc ->
        let key = (subject_key role name, pp) in
        if VisitSet.mem key visited then acc
        else
          let visited' = VisitSet.add key visited in
          let snapshot = Abs_Sem.find asem pp in
          let value, stored_pps = find_scalar snapshot name in
          let lbl_t_opt, line, expr = node_site tbl pp in
          let children =
            match lbl_t_opt with
            | Some lt -> (
                match assignment_rhs_for_subject name lt with
                | Some { exp = Exp.Malloc _; _ } -> []
                | Some rhs ->
                    trace_expr rhs Dependency snapshot asem tbl visited'
                      (depth - 1)
                | None ->
                    trace_expr lt Dependency snapshot asem tbl visited'
                      (depth - 1))
            | None -> []
          in
          {
            pp;
            line;
            expr;
            subject = name;
            role;
            value;
            pps = stored_pps;
            is_handler = is_handler_pp pp;
            children;
          }
          :: acc)
      pps []

and trace_cell (display : string) (value : Abs_Val.t) (pps : PPSet.t)
    (asem : Abs_Sem.t) (tbl : Exp.lbl_t Exp.Lbl_map.t) (visited : VisitSet.t)
    (depth : int) : trace_node list =
  if depth = 0 || PPSet.is_empty pps then []
  else
    PPSet.fold
      (fun pp acc ->
        let key = (subject_key HeapValue display, pp) in
        if VisitSet.mem key visited then acc
        else
          let visited' = VisitSet.add key visited in
          let snapshot = Abs_Sem.find asem pp in
          let lbl_t_opt, line, expr = node_site tbl pp in
          let site_value, site_pps, children =
            match lbl_t_opt with
            | Some lt -> (
                match heap_write_parts lt with
                | Some (base_e, idx_e, rhs) ->
                    let resolved_v, resolved_pps =
                      resolve_deref_cell snapshot base_e idx_e
                    in
                    let v =
                      if resolved_v = Abs_Val.bot then value else resolved_v
                    in
                    let deps =
                      trace_expr rhs Dependency snapshot asem tbl visited'
                        (depth - 1)
                    in
                    let base_deps =
                      trace_expr base_e Base snapshot asem tbl visited'
                        (depth - 1)
                    in
                    let idx_deps =
                      trace_expr idx_e Index snapshot asem tbl visited'
                        (depth - 1)
                    in
                    let pps' =
                      if PPSet.is_empty resolved_pps then pps else resolved_pps
                    in
                    (v, pps', deps @ base_deps @ idx_deps)
                | None -> (
                    match lt.exp with
                    | Exp.Malloc (_, init_e) ->
                        ( value,
                          pps,
                          trace_expr init_e Dependency snapshot asem tbl
                            visited' (depth - 1) )
                    | _ ->
                        ( value,
                          pps,
                          trace_expr lt Dependency snapshot asem tbl visited'
                            (depth - 1) )))
            | None -> (value, pps, [])
          in
          {
            pp;
            line;
            expr;
            subject = display;
            role = HeapValue;
            value = site_value;
            pps = site_pps;
            is_handler = is_handler_pp pp;
            children;
          }
          :: acc)
      pps []

and trace_expr (e : Exp.lbl_t) (role : role) (snapshot : Abs_Mem.t)
    (asem : Abs_Sem.t) (tbl : Exp.lbl_t Exp.Lbl_map.t) (visited : VisitSet.t)
    (depth : int) : trace_node list =
  if depth = 0 then []
  else
    match e.exp with
    | Exp.Unit | Exp.Int _ | Exp.AddrOf _ | Exp.Enable | Exp.Disable -> []
    | Exp.Var x ->
        let _, pps = find_scalar snapshot x in
        trace_scalar x pps role asem tbl visited depth
    | Exp.Deref (base_e, idx_e) ->
        let cell_v, cell_pps = resolve_deref_cell snapshot base_e idx_e in
        let cell_trace =
          trace_cell (fmt_exp e.exp) cell_v cell_pps asem tbl visited depth
        in
        let base_trace =
          trace_expr base_e Base snapshot asem tbl visited (depth - 1)
        in
        let idx_trace =
          trace_expr idx_e Index snapshot asem tbl visited (depth - 1)
        in
        cell_trace @ base_trace @ idx_trace
    | Exp.Bop (_, e1, e2) ->
        trace_expr e1 Dependency snapshot asem tbl visited (depth - 1)
        @ trace_expr e2 Dependency snapshot asem tbl visited (depth - 1)
    | Exp.Malloc _ -> []
    | Exp.Assign (_, rhs) ->
        trace_expr rhs Dependency snapshot asem tbl visited (depth - 1)
    | Exp.Seq (e1, e2) ->
        trace_expr e1 Dependency snapshot asem tbl visited (depth - 1)
        @ trace_expr e2 Dependency snapshot asem tbl visited (depth - 1)
    | Exp.If (_, t, f) ->
        (* Control-dependence tracing is intentionally out of scope here. *)
        trace_expr t Dependency snapshot asem tbl visited (depth - 1)
        @ trace_expr f Dependency snapshot asem tbl visited (depth - 1)
    | Exp.While (_, _, body) ->
        trace_expr body Dependency snapshot asem tbl visited (depth - 1)

let deref_parts_at_error (lt : Exp.lbl_t) : (Exp.lbl_t * Exp.lbl_t) option =
  match lt.exp with
  | Exp.Deref (base_e, idx_e) -> Some (base_e, idx_e)
  | Exp.Assign ({ exp = Exp.Deref (base_e, idx_e); _ }, _) ->
      Some (base_e, idx_e)
  | _ -> None

let base_alloc_key (b : Abs_Loc.t) : Abs_Loc.t =
  match b with
  | Abs_Loc.AHeapLoc { lbl; _ } -> Abs_Loc.AHeapLoc { lbl; offset = Itv.bot }
  | other -> other

let merge_errors (errors : ErrorSet.t) : ErrorSet.t =
  let tbl : (ProgramPoint.t * Error.access * Abs_Loc.t, Error.t) Hashtbl.t =
    Hashtbl.create 16
  in
  ErrorSet.iter
    (fun (e : Error.t) ->
      let key = (e.at, e.access, base_alloc_key e.base) in
      match Hashtbl.find_opt tbl key with
      | None -> Hashtbl.add tbl key e
      | Some old ->
          let merged =
            {
              old with
              in_itv = Itv.join old.in_itv e.in_itv;
              left_oob = Itv.join old.left_oob e.left_oob;
              right_oob = Itv.join old.right_oob e.right_oob;
              base_pp = PPSet.union old.base_pp e.base_pp;
              offset_pp = PPSet.union old.offset_pp e.offset_pp;
              handler_caused = old.handler_caused || e.handler_caused;
            }
          in
          Hashtbl.replace tbl key merged)
    errors;
  Hashtbl.fold (fun _ e acc -> ErrorSet.add e acc) tbl ErrorSet.empty

let rec handler_iids_in_node (n : trace_node) : int list =
  let own =
    match handler_iid_of_pp n.pp with Some iid -> [ iid ] | None -> []
  in
  own @ List.concat_map handler_iids_in_node n.children

let handler_iids_of_traces traces =
  List.concat_map handler_iids_in_node traces |> List.sort_uniq Int.compare

let trace_one (asem : Abs_Sem.t) (tbl : Exp.lbl_t Exp.Lbl_map.t) (e : Error.t) :
    trace_chain option =
  let err_lbl_opt = lookup_lbl_t tbl e.at in
  match Option.bind err_lbl_opt deref_parts_at_error with
  | None -> None
  | Some (base_e, idx_e) ->
      let err_snapshot = Abs_Sem.find asem e.at in
      let base_trace =
        match base_e.exp with
        | Exp.Var x ->
            trace_scalar x e.base_pp Base asem tbl VisitSet.empty max_depth
        | _ ->
            trace_expr base_e Base err_snapshot asem tbl VisitSet.empty
              max_depth
      in
      let index_trace =
        match idx_e.exp with
        | Exp.Var x ->
            trace_scalar x e.offset_pp Index asem tbl VisitSet.empty max_depth
        | Exp.Deref (inner_base_e, inner_idx_e) ->
            let cell_v, _ =
              resolve_deref_cell err_snapshot inner_base_e inner_idx_e
            in
            let cell_trace =
              trace_cell (fmt_exp idx_e.exp) cell_v e.offset_pp asem tbl
                VisitSet.empty max_depth
            in
            let structural_trace =
              trace_expr inner_base_e Base err_snapshot asem tbl VisitSet.empty
                (max_depth - 1)
              @ trace_expr inner_idx_e Index err_snapshot asem tbl
                  VisitSet.empty (max_depth - 1)
            in
            cell_trace @ structural_trace
        | _ ->
            trace_expr idx_e Index err_snapshot asem tbl VisitSet.empty
              max_depth
      in
      let handler_iids = handler_iids_of_traces (base_trace @ index_trace) in
      let err_line = Option.bind err_lbl_opt (fun lt -> lt.line) in
      let err_expr =
        match err_lbl_opt with Some lt -> fmt_exp lt.exp | None -> "<unknown>"
      in
      Some
        {
          error = e;
          err_line;
          err_expr;
          index_expr = fmt_exp idx_e.exp;
          base_trace;
          index_trace;
          handler_iids;
        }

let trace_errors (errors : ErrorSet.t) (asem : Abs_Sem.t) (pgm : Program.t) :
    trace_chain list =
  let tbl = build_lbl_table pgm in
  let errors = merge_errors errors in
  ErrorSet.fold
    (fun e acc ->
      match trace_one asem tbl e with Some c -> c :: acc | None -> acc)
    errors []

let trace_warnings (errors : ErrorSet.t) (asem : Abs_Sem.t) (pgm : Program.t) :
    trace_chain list =
  trace_errors errors asem pgm
  |> List.filter (fun c ->
      c.handler_iids <> []
      || PPSet.exists is_handler_pp c.error.base_pp
      || PPSet.exists is_handler_pp c.error.offset_pp
      || c.error.handler_caused)

let string_of_ppset (pps : PPSet.t) : string =
  if PPSet.is_empty pps then "{}"
  else
    PPSet.elements pps
    |> List.map ProgramPoint.string_of_t
    |> String.concat ", " |> Printf.sprintf "{%s}"

let string_of_oob_kind (e : Error.t) =
  match (e.left_oob, e.right_oob) with
  | Itv.Bot, Itv.Bot -> "in bounds"
  | Itv.Bot, _ -> "right OOB"
  | _, Itv.Bot -> "left OOB"
  | _, _ -> "left/right OOB"

let indent n = String.make (n * 2) ' '

let rec string_of_node depth n =
  let loc =
    match n.line with Some l -> Printf.sprintf " line %d" l | None -> ""
  in
  let handler = if n.is_handler then " [handler]" else "" in
  let head =
    Printf.sprintf "%s<- %s %s `%s` value=%s pps=%s%s%s" (indent depth)
      (role_name n.role) n.subject n.expr
      (Abs_Val.string_of_t n.value)
      (string_of_ppset n.pps) handler loc
  in
  match n.children with
  | [] -> head
  | children ->
      head ^ "\n"
      ^ (List.map (string_of_node (depth + 1)) children |> String.concat "\n")

let string_of_chain c =
  let loc =
    match c.err_line with
    | Some l -> Printf.sprintf "line %d" l
    | None -> ProgramPoint.string_of_t c.error.at
  in
  let handlers =
    match c.handler_iids with
    | [] -> "handler unknown"
    | ids -> ids |> List.map (Printf.sprintf "handler %d") |> String.concat ", "
  in
  let base =
    match c.base_trace with
    | [] -> "  (no base trace)"
    | nodes -> List.map (string_of_node 1) nodes |> String.concat "\n"
  in
  let index =
    match c.index_trace with
    | [] -> "  (no index trace)"
    | nodes -> List.map (string_of_node 1) nodes |> String.concat "\n"
  in
  Printf.sprintf
    "Warning at %s: `%s`\n\
    \  access: %s, kind: %s, interrupt influence: %s\n\
    \  safe=%s left=%s right=%s\n\
    \  index expression: %s\n\
    \  Base provenance:\n\
     %s\n\
    \  Index provenance:\n\
     %s"
    loc c.err_expr
    (Error.string_of_access c.error.access)
    (string_of_oob_kind c.error)
    handlers
    (Itv.string_of_t c.error.in_itv)
    (Itv.string_of_t c.error.left_oob)
    (Itv.string_of_t c.error.right_oob)
    c.index_expr base index

let string_of_report chains =
  match chains with
  | [] -> "No interrupt-influenced OOB warnings found."
  | _ ->
      let n = List.length chains in
      Printf.sprintf "=== Trace Warning Report: %d warning%s ===\n" n
        (if n = 1 then "" else "s")
      ^ (List.mapi
           (fun i c ->
             Printf.sprintf "--- Warning #%d ---\n%s" (i + 1)
               (string_of_chain c))
           chains
        |> String.concat "\n\n")
