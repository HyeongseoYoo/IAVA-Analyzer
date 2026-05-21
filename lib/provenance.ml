open Syntax
open Domain
open Abs_dom

(* ===== Types ===== *)

type bug_kind = LeftOOB | RightOOB | BothOOB

type source_site = {
  pp : ProgramPoint.t;
  line : int option;
  expr : string;
  value : Abs_Val.t;
  pps : PPSet.t;
}

type prov_node = { site : source_site; children : prov_node list }

type chain = {
  error : Error.t;
  err_site : source_site;
  index_var : string;
  base_prov : prov_node list;
  offset_prov : prov_node list;
}

(* ===== Helpers ===== *)

let bug_kind_of (e : Error.t) : bug_kind =
  match (e.left_oob, e.right_oob) with
  | Itv.Bot, _ -> RightOOB
  | _, Itv.Bot -> LeftOOB
  | _ -> BothOOB

let string_of_bug_kind = function
  | LeftOOB -> "index too small (left OOB)"
  | RightOOB -> "index too large (right OOB)"
  | BothOOB -> "index out of bounds (both directions)"

(* Format an expression without internal label prefixes, for human display. *)
let rec fmt_exp : Exp.t -> string = function
  | Exp.Unit -> "unit"
  | Exp.Int n -> string_of_int n
  | Exp.Var x -> x
  | Exp.Enable -> "enable"
  | Exp.Disable -> "disable"
  | Exp.Bop (bop, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (fmt_exp e1.exp)
        (Exp.string_of_bop bop)
        (fmt_exp e2.exp)
  | Exp.Deref (e1, e2) ->
      Printf.sprintf "*%s[%s]" (fmt_exp e1.exp) (fmt_exp e2.exp)
  | Exp.Malloc (e1, e2) ->
      Printf.sprintf "malloc(%s, %s)" (fmt_exp e1.exp) (fmt_exp e2.exp)
  | Exp.Assign (e1, e2) ->
      Printf.sprintf "%s := %s" (fmt_exp e1.exp) (fmt_exp e2.exp)
  | Exp.Seq (e1, e2) ->
      Printf.sprintf "%s; %s" (fmt_exp e1.exp) (fmt_exp e2.exp)
  | Exp.If (c, t, f) ->
      Printf.sprintf "if %s then %s else %s" (fmt_exp c.exp) (fmt_exp t.exp)
        (fmt_exp f.exp)
  | Exp.While (_, c, b) ->
      Printf.sprintf "while %s do (%s)" (fmt_exp c.exp) (fmt_exp b.exp)

(* Extract variable names referenced in an expression. For Assign, only the
   RHS is traversed so we follow the data that was written, not the target. *)
let rec vars_in_exp : Exp.t -> string list = function
  | Exp.Var x -> [ x ]
  | Exp.Int _ | Exp.Unit | Exp.Enable | Exp.Disable -> []
  | Exp.Bop (_, e1, e2) -> vars_in_exp e1.exp @ vars_in_exp e2.exp
  | Exp.Deref (e1, e2) -> vars_in_exp e1.exp @ vars_in_exp e2.exp
  | Exp.Malloc (e1, e2) -> vars_in_exp e1.exp @ vars_in_exp e2.exp
  | Exp.Assign (_, rhs) -> vars_in_exp rhs.exp
  | Exp.Seq (e1, e2) -> vars_in_exp e1.exp @ vars_in_exp e2.exp
  | Exp.If (c, t, f) ->
      vars_in_exp c.exp @ vars_in_exp t.exp @ vars_in_exp f.exp
  | Exp.While (_, c, b) -> vars_in_exp c.exp @ vars_in_exp b.exp

(* Resolve a Deref(base_e, idx_e) expression against a snapshot to find the
   heap cell's (joined_value, joined_ppset).  Returns None when the base
   expression does not resolve to a heap pointer in the snapshot.
   Scans all cells in the snapshot whose lbl matches the base pointer and
   whose offset interval overlaps the resolved index interval. *)
let resolve_deref_cell (snapshot : Abs_Mem.t) (base_e : Exp.lbl_t)
    (idx_e : Exp.lbl_t) : (Abs_Val.t * PPSet.t) option =
  let base_lbl_opt =
    match base_e.exp with
    | Exp.Var base_name ->
        let (_, _, ptr_loc), _ = Abs_Mem.find snapshot (Abs_Loc.get base_name) in
        (match ptr_loc with
        | Abs_Loc.AHeapLoc { lbl; _ } -> Some lbl
        | _ -> None)
    | _ -> None
  in
  match base_lbl_opt with
  | None -> None
  | Some target_lbl ->
      let idx_itv =
        match idx_e.exp with
        | Exp.Int n -> Itv.alpha n
        | Exp.Var idx_name ->
            let (itv, _, _), _ = Abs_Mem.find snapshot (Abs_Loc.get idx_name) in
            itv
        | _ -> Itv.top
      in
      let cell_val, cell_pps =
        Abs_Mem.fold
          (fun k (v, pps) (acc_v, acc_pps) ->
            match k with
            | Abs_Loc.AHeapLoc { lbl; offset }
              when Exp.Lbl.compare lbl target_lbl = 0
                   && Itv.is_overlap idx_itv offset ->
                (Abs_Val.join acc_v v, PPSet.union acc_pps pps)
            | _ -> (acc_v, acc_pps))
          snapshot (Abs_Val.bot, PPSet.empty)
      in
      if PPSet.is_empty cell_pps then None
      else Some (cell_val, cell_pps)

(* Build a unified label → lbl_t table across the whole program so we can
   look up the expression and line for any program point. *)
let build_lbl_table (pgm : Program.t) :
    Exp.lbl_t Syntax.Exp.Lbl_map.t =
  let add_lbl_t tbl (le : Exp.lbl_t) =
    let rec walk ({ lbl; exp; line } : Exp.lbl_t) acc =
      let acc =
        Exp.Lbl_map.add (Either.Left lbl)
          (({ lbl; exp; line } : Exp.lbl_t))
          acc
      in
      match exp with
      | Exp.Unit | Exp.Int _ | Exp.Var _ | Exp.Enable | Exp.Disable -> acc
      | Exp.Bop (_, e1, e2)
      | Exp.Deref (e1, e2)
      | Exp.Malloc (e1, e2)
      | Exp.Assign (e1, e2)
      | Exp.Seq (e1, e2) ->
          acc |> walk e1 |> walk e2
      | Exp.If (e1, e2, e3) -> acc |> walk e1 |> walk e2 |> walk e3
      | Exp.While (glbl, e1, e2) ->
          Exp.Lbl_map.add (Either.Right glbl)
            (({ lbl = glbl; exp; line } : Exp.lbl_t))
            (acc |> walk e1 |> walk e2)
    in
    walk le tbl
  in
  let tbl = Exp.Lbl_map.empty in
  let tbl = add_lbl_t tbl pgm.Program.global in
  let tbl = add_lbl_t tbl pgm.Program.main in
  List.fold_left
    (fun acc (h : Handler.t) -> add_lbl_t acc h.body)
    tbl pgm.Program.handler

let lookup_lbl_t (tbl : Exp.lbl_t Exp.Lbl_map.t) (pp : ProgramPoint.t) :
    Exp.lbl_t option =
  match pp with
  | ProgramPoint.Unit -> None
  | ProgramPoint.Label lbl ->
      Exp.Lbl_map.find_opt (Either.Left lbl) tbl

(* ===== Multi-hop provenance tracing ===== *)

let max_depth = 10

(* trace_pps and trace_heap_pps are mutually recursive.
   trace_pps:      follows named scalar variables backward through asem snapshots.
   trace_heap_pps: follows handler-PP sets that wrote to a heap cell; used when
                   the expression being traced reads from a heap cell via Deref. *)
let rec trace_pps (pps : PPSet.t) (var_name : string) (asem : Abs_Sem.t)
    (tbl : Exp.lbl_t Exp.Lbl_map.t) (visited : PPSet.t) (depth : int) :
    prov_node list =
  if depth = 0 then []
  else
    let visited_with_siblings = PPSet.union visited pps in
    PPSet.fold
      (fun pp acc ->
        if PPSet.mem pp visited then acc
        else
          let snapshot = Abs_Sem.find asem pp in
          let loc = Abs_Loc.get var_name in
          let value, pps_stored = Abs_Mem.find snapshot loc in
          let lbl_t_opt = lookup_lbl_t tbl pp in
          let line = Option.bind lbl_t_opt (fun lt -> lt.line) in
          let expr =
            match lbl_t_opt with
            | Some lt -> fmt_exp lt.exp
            | None -> "<unknown>"
          in
          let site = { pp; line; expr; value; pps = pps_stored } in
          let children =
            match lbl_t_opt with
            | None -> []
            | Some lt ->
                let rhs_vars = vars_in_exp lt.exp in
                let scalar_children =
                  List.concat_map
                    (fun v ->
                      let v_loc = Abs_Loc.get v in
                      let _, sub_pps = Abs_Mem.find snapshot v_loc in
                      trace_pps sub_pps v asem tbl visited_with_siblings (depth - 1))
                    rhs_vars
                in
                (* When RHS is a heap read (Deref), resolve the cell in the
                   snapshot and trace handler PPs that wrote to it. *)
                let heap_children =
                  let deref_opt =
                    match lt.exp with
                    | Exp.Assign (_, { exp = Exp.Deref (base_e, idx_e); _ }) ->
                        Some (base_e, idx_e)
                    | _ -> None
                  in
                  match deref_opt with
                  | None -> []
                  | Some (base_e, idx_e) -> (
                      match resolve_deref_cell snapshot base_e idx_e with
                      | None -> []
                      | Some (cell_val, cell_pps) ->
                          let handler_pps =
                            PPSet.filter
                              (fun p ->
                                match p with
                                | ProgramPoint.Label (Exp.Lbl.Handler _) -> true
                                | _ -> false)
                              cell_pps
                          in
                          if PPSet.is_empty handler_pps then []
                          else
                            trace_heap_pps handler_pps cell_val asem tbl
                              visited_with_siblings (depth - 1))
                in
                scalar_children @ heap_children
          in
          { site; children } :: acc)
      pps []

(* Trace provenance for a set of handler PPs that wrote to a heap cell.
   cell_val is the joined value stored in the cell (shown in the prov_node).
   For each handler PP, shows the write statement and traces its RHS variables
   one level deeper via trace_pps. *)
and trace_heap_pps (cell_pps : PPSet.t) (cell_val : Abs_Val.t)
    (asem : Abs_Sem.t) (tbl : Exp.lbl_t Exp.Lbl_map.t) (visited : PPSet.t)
    (depth : int) : prov_node list =
  if depth = 0 then []
  else
    let visited_with_siblings = PPSet.union visited cell_pps in
    PPSet.fold
      (fun pp acc ->
        if PPSet.mem pp visited then acc
        else
          let lbl_t_opt = lookup_lbl_t tbl pp in
          let line = Option.bind lbl_t_opt (fun lt -> lt.line) in
          let expr =
            match lbl_t_opt with
            | Some lt -> fmt_exp lt.exp
            | None -> "<unknown>"
          in
          (* Choose the most precise value/ppset for this specific write PP:
             - Scalar alias (Var lhs := rhs): look up lhs in the snapshot.
             - Heap write (deref base[idx] := rhs): resolve the cell from the
               snapshot to get the value as-of-this-write (e.g. [0,0] for the
               init write, not the globally-joined [0,255]).
               Falls back to cell_val when the snapshot is minimal and does not
               contain the base pointer (handler fast-path snapshots).
             - Anything else: use the passed cell_val / cell_pps. *)
          let snapshot = Abs_Sem.find asem pp in
          let site_value, site_pps =
            match lbl_t_opt with
            | Some { exp = Exp.Assign ({ exp = Exp.Var lhs_var; _ }, _); _ } ->
                Abs_Mem.find snapshot (Abs_Loc.get lhs_var)
            | Some { exp = Exp.Assign
                ({ exp = Exp.Deref (base_e, idx_e); _ }, _); _ } -> (
                match resolve_deref_cell snapshot base_e idx_e with
                | Some (v, pps) -> (v, pps)
                | None -> (cell_val, cell_pps))
            | _ -> (cell_val, cell_pps)
          in
          let site = { pp; line; expr; value = site_value; pps = site_pps } in
          let children =
            match lbl_t_opt with
            | None -> []
            | Some lt ->
                let rhs_vars = vars_in_exp lt.exp in
                (* Also trace the LHS base pointer for heap writes like
                   *base[idx] := rhs so the alias step (base := ...) appears. *)
                let lhs_base_vars =
                  match lt.exp with
                  | Exp.Assign
                      ({ exp = Exp.Deref ({ exp = Exp.Var bv; _ }, _); _ }, _) ->
                      [ bv ]
                  | _ -> []
                in
                List.concat_map
                  (fun v ->
                    let v_loc = Abs_Loc.get v in
                    let _, sub_pps = Abs_Mem.find snapshot v_loc in
                    trace_pps sub_pps v asem tbl visited_with_siblings (depth - 1))
                  (rhs_vars @ lhs_base_vars)
          in
          { site; children } :: acc)
      cell_pps []

(* ===== Deduplication ===== *)

(* Two Error.t records are the "same logical bug" when they refer to the same
   program point, the same access kind, and the same base array/location.
   Differences in OOB intervals or provenance sets arise from analysing the
   same expression under slightly different abstract states at different
   post-step iterations.  We merge such duplicates by taking the union of
   their OOB intervals and provenance sets. *)
(* Strip the interval offset from a base location so that errors at the same
   access site (same label, same array) with different abstract indices merge
   rather than appearing as separate entries. *)
let base_alloc_key (b : Abs_Loc.t) : Abs_Loc.t =
  match b with
  | Abs_Loc.AHeapLoc { lbl; _ } ->
      Abs_Loc.AHeapLoc { lbl; offset = Itv.bot }
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
      | Some existing ->
          let merged =
            {
              existing with
              in_itv = Itv.join existing.in_itv e.in_itv;
              left_oob = Itv.join existing.left_oob e.left_oob;
              right_oob = Itv.join existing.right_oob e.right_oob;
              base_pp = PPSet.union existing.base_pp e.base_pp;
              offset_pp = PPSet.union existing.offset_pp e.offset_pp;
              handler_caused = existing.handler_caused || e.handler_caused;
            }
          in
          Hashtbl.replace tbl key merged)
    errors;
  Hashtbl.fold (fun _ e acc -> ErrorSet.add e acc) tbl ErrorSet.empty

(* ===== Entry point ===== *)

let analyze (errors : ErrorSet.t) (asem : Abs_Sem.t) (pgm : Program.t) :
    chain list =
  let tbl = build_lbl_table pgm in
  let errors = merge_errors errors in
  ErrorSet.fold
    (fun (e : Error.t) acc ->
      let err_lbl_opt = lookup_lbl_t tbl e.at in
      let err_site =
        {
          pp = e.at;
          line = Option.bind err_lbl_opt (fun lt -> lt.line);
          expr =
            (match err_lbl_opt with
            | Some lt -> fmt_exp lt.exp
            | None -> "<unknown>");
          value = Abs_Val.bot;
          pps = PPSet.empty;
        }
      in
      (* Determine which variable names to trace for base and offset.
         We look at the expression at the error site to extract the base
         variable (LHS of Deref) and offset variable (index of Deref).
         off_e_opt carries the Deref sub-expression when the index is itself
         a Deref (Pattern 2: heap-mediated index), so we can call
         trace_heap_pps instead of trace_pps for the offset provenance. *)
      let base_var, offset_var, off_e_opt =
        match err_lbl_opt with
        | Some { exp = Exp.Deref (base_e, off_e); _ } ->
            let bv = match base_e.exp with Exp.Var x -> x | _ -> "<base>" in
            let ov, deref =
              match off_e.exp with
              | Exp.Var x -> (x, None)
              | Exp.Deref _ -> (fmt_exp off_e.exp, Some off_e)
              | _ -> ("<offset>", None)
            in
            (bv, ov, deref)
        | Some { exp = Exp.Assign (lhs, _); _ } -> (
            match lhs.exp with
            | Exp.Deref (base_e, off_e) ->
                let bv = match base_e.exp with Exp.Var x -> x | _ -> "<base>" in
                let ov, deref =
                  match off_e.exp with
                  | Exp.Var x -> (x, None)
                  | Exp.Deref _ -> (fmt_exp off_e.exp, Some off_e)
                  | _ -> ("<offset>", None)
                in
                (bv, ov, deref)
            | _ -> ("<base>", "<offset>", None))
        | _ -> ("<base>", "<offset>", None)
      in
      (* Look up the combined value and PPSet of the index at the error site.
         For a simple Var index: join across contributing PP snapshots.
         For a Deref index (heap-mediated): use the full abstract index range
         reconstructed from the error's OOB fields, attributed to the handler
         PPs in offset_pp. *)
      let is_deref_index = off_e_opt <> None in
      let handler_offset_pps =
        PPSet.filter
          (fun p ->
            match p with
            | ProgramPoint.Label (Exp.Lbl.Handler _) -> true
            | _ -> false)
          e.offset_pp
      in
      let index_val, index_pps =
        if offset_var = "<offset>" then (Abs_Val.bot, PPSet.empty)
        else
          (* Reconstruct the actual index interval from the OOB fields recorded
             at the error site.  Joining the variable's value across offset_pp
             snapshots gives a widened value (e.g. [0,∞] from loop widening)
             that disagrees with the in_itv/right_oob already shown on the same
             line.  Using join(in_itv, left_oob, right_oob) is consistent with
             how the deref-index (Pattern 2/3) case already works. *)
          let full_itv =
            Itv.join (Itv.join e.in_itv e.left_oob) e.right_oob
          in
          ((full_itv, Abs_Unit.bot, Abs_Loc.bot), e.offset_pp)
      in
      let err_site = { err_site with value = index_val; pps = index_pps } in
      let visited0 = PPSet.singleton e.at in
      let base_prov =
        trace_pps e.base_pp base_var asem tbl visited0 max_depth
      in
      let offset_prov =
        if is_deref_index then
          let heap_prov =
            if PPSet.is_empty handler_offset_pps then
              trace_pps e.offset_pp offset_var asem tbl visited0 max_depth
            else
              trace_heap_pps e.offset_pp index_val asem tbl visited0 max_depth
          in
          (* Also trace the inner base variable (e.g. NvmeCtrl) by looking it
             up in the snapshot at the error site. This surfaces aliasing steps
             like `NvmeCtrl := NvmeRegs` that are not part of the heap cell's
             own write history (offset_pp). *)
          let alias_prov =
            match off_e_opt with
            | Some { exp = Exp.Deref (inner_base_e, _); _ } -> (
                match inner_base_e.exp with
                | Exp.Var inner_base_var ->
                    let snapshot = Abs_Sem.find asem e.at in
                    let _, inner_pps =
                      Abs_Mem.find snapshot (Abs_Loc.get inner_base_var)
                    in
                    if PPSet.is_empty inner_pps then []
                    else
                      trace_pps inner_pps inner_base_var asem tbl visited0
                        (max_depth - 1)
                | _ -> [])
            | _ -> []
          in
          heap_prov @ alias_prov
        else
          trace_pps e.offset_pp offset_var asem tbl visited0 max_depth
      in
      { error = e; err_site; index_var = offset_var; base_prov; offset_prov }
      :: acc)
    errors []

(* ===== Formatters ===== *)

let indent (n : int) : string = String.make (n * 2) ' '

(* Collect all interrupt handler IIDs mentioned anywhere in a prov tree. *)
let rec handler_iids_in_node (node : prov_node) : int list =
  let own =
    match node.site.pp with
    | ProgramPoint.Label (Exp.Lbl.Handler (iid, _)) -> [ iid ]
    | _ -> []
  in
  own @ List.concat_map handler_iids_in_node node.children

let handler_iids_in_chain (c : chain) : int list =
  let from_base = List.concat_map handler_iids_in_node c.base_prov in
  let from_offset = List.concat_map handler_iids_in_node c.offset_prov in
  List.sort_uniq Int.compare (from_base @ from_offset)

let string_of_ppset (pps : PPSet.t) : string =
  if PPSet.is_empty pps then "{}"
  else
    let elems =
      PPSet.elements pps |> List.map ProgramPoint.string_of_t
    in
    "{" ^ String.concat ", " elems ^ "}"

let string_of_site (s : source_site) : string =
  let loc_str =
    match s.pp with
    | ProgramPoint.Label (Exp.Lbl.Handler (iid, _)) -> (
        match s.line with
        | Some l -> Printf.sprintf "handler %d, line %d" iid l
        | None -> Printf.sprintf "handler %d" iid)
    | _ -> (
        match s.line with
        | Some l -> Printf.sprintf "line %d" l
        | None -> ProgramPoint.string_of_t s.pp)
  in
  Printf.sprintf "%s: `%s`  (value: %s, pps: %s)" loc_str s.expr
    (Abs_Val.string_of_t s.value)
    (string_of_ppset s.pps)

let rec string_of_prov_node (depth : int) (node : prov_node) : string =
  let arrow = indent depth ^ "← " in
  let site_str = arrow ^ string_of_site node.site in
  if node.children = [] then site_str
  else
    let children_str =
      List.map (string_of_prov_node (depth + 1)) node.children
      |> String.concat "\n"
    in
    site_str ^ "\n" ^ children_str

let string_of_chain (c : chain) : string =
  let at_str =
    match c.err_site.line with
    | Some l -> Printf.sprintf "line %d" l
    | None -> ProgramPoint.string_of_t c.err_site.pp
  in
  let access_str = Error.string_of_access c.error.access in
  let kind_str = string_of_bug_kind (bug_kind_of c.error) in
  let handler_str =
    let iids = handler_iids_in_chain c in
    if iids = [] && not c.error.handler_caused then ""
    else
      let iid_str =
        if iids = [] then "unknown interrupt handler"
        else
          List.map (fun i -> Printf.sprintf "handler %d" i) iids
          |> String.concat ", "
      in
      Printf.sprintf "  [caused by interrupt: %s]\n" iid_str
  in
  let oob_str =
    Printf.sprintf "  safe range: %s  |  left OOB: %s  |  right OOB: %s"
      (Itv.string_of_t c.error.in_itv)
      (Itv.string_of_t c.error.left_oob)
      (Itv.string_of_t c.error.right_oob)
  in
  let index_str =
    if c.index_var = "<offset>" || c.err_site.value = Abs_Val.bot then ""
    else
      Printf.sprintf "\n  index (%s): %s  pps: %s" c.index_var
        (Abs_Val.string_of_t c.err_site.value)
        (string_of_ppset c.err_site.pps)
  in
  let base_str =
    if c.base_prov = [] then "  (no base provenance found)"
    else
      List.map (string_of_prov_node 1) c.base_prov |> String.concat "\n"
  in
  let offset_str =
    if c.offset_prov = [] then "  (no offset provenance found)"
    else
      List.map (string_of_prov_node 1) c.offset_prov |> String.concat "\n"
  in
  Printf.sprintf
    "Bug at %s: `%s`\n  %s %s\n%s%s%s\n  Base pointer provenance:\n%s\n  \
     Offset provenance:\n%s"
    at_str c.err_site.expr access_str kind_str handler_str oob_str index_str
    base_str offset_str

let string_of_report (chains : chain list) : string =
  if chains = [] then "No bugs found."
  else
    let n = List.length chains in
    let header =
      Printf.sprintf "=== Provenance Report: %d bug%s found ===\n" n
        (if n = 1 then "" else "s")
    in
    header
    ^ (List.mapi
         (fun i c ->
           Printf.sprintf "--- Bug #%d ---\n%s" (i + 1) (string_of_chain c))
         chains
      |> String.concat "\n\n")
