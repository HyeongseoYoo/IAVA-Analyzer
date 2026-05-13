open Syntax
open Domain
open Abs_dom

type conf = { env : Env.t; mem : Mem.t; imode : Interrupt.t }

type abs_conf = {
  asem : Abs_Sem.t;
  amem : Abs_Mem.t;
  aimode : Interrupt.t;
  errs : ErrorSet.t;
}

type result = { value : Value.t; pp : ProgramPoint.t; out : Outcome.t }
type abs_res = { avalue : Abs_Val.t; app : PPSet.t }

(* A single memory-modifying effect compiled from a handler body *)
type summary_atom = {
  lhs : Exp.lbl_t;
  rhs : Exp.lbl_t;
  assign_lbl : Exp.Lbl.t;
}

(* Pre-compiled symbolic summary of a handler body *)
type handler_summary =
  | Compiled of summary_atom list
  | Fallback of Exp.lbl_t

(* ===== Pre-compiled fixpoint types ===== *)

(* Fixpoint effect of all handlers combined on a single scalar location *)
type fp_scalar =
  | FPS_Unchanged           (* no handler writes here *)
  | FPS_Inc                 (* x := x + c (c>0): fixpoint sets upper → +∞ *)
  | FPS_Dec                 (* x := x - c (c>0): fixpoint sets lower → -∞ *)
  | FPS_JoinConsts of Itv.t (* x := constant: join current value with this itv *)
  | FPS_Top                 (* complex write: widen to top *)

(* A single heap-array write effect extracted from handler bodies *)
type fp_array_write = {
  fpa_lbl       : Exp.Lbl.t;                              (* heap alloc label *)
  fpa_idx       : [ `Const of Itv.t | `Var of Abs_Loc.t ]; (* index: constant or var *)
  fpa_rhs       : [ `Const of Abs_Val.t | `Var of Abs_Loc.t ]; (* value: const or var *)
  fpa_at        : ProgramPoint.t;
  fpa_offset_pp : PPSet.t;
}

type compiled_fixpoint = {
  fp_scalars : (Abs_Loc.t * fp_scalar * PPSet.t) list;
  fp_arrays  : fp_array_write list;
}

exception Runtime_error of string

let var_tbl : VarTbl.t ref = ref VarTbl.empty
let iset : IidSet.t ref = ref IidSet.empty
let handlers : HandlerStore.t ref = ref HandlerStore.empty
let size_tbl : Itv.t LblMap.t ref = ref LblMap.empty
let handler_summaries : handler_summary HandlerStore.IidMap.t ref =
  ref HandlerStore.IidMap.empty
let compiled_fp : compiled_fixpoint ref =
  ref { fp_scalars = []; fp_arrays = [] }
let use_compiled_fp : bool ref = ref false

(* Forward reference: will be set to apply_fixpoint_to_conf after it is defined *)
let post_steps_fn : (abs_conf -> abs_conf) ref = ref (fun c -> c)
let widen_cnt = 3

let rec eval ?(lvalue = false) (c : conf) (lbl_exp : Exp.lbl_t) : result * conf
    =
  let ({ lbl; exp; _ } : Exp.lbl_t) = lbl_exp in
  let ({ env = _; mem; imode = _ } : conf) = c in
  (* TODO: Done -> Non-Deterministic *)
  let r = { value = Value.Unit; pp = Unit; out = Outcome.Done } in
  (* TEST CODE *)
  (* let r = (if lbl = Exp.Lbl.Main 3 then { value = Value.Unit; pp = Unit; out = Outcome.I 0 } else { value = Value.Unit; pp = Unit; out = Outcome.Done }) in  *)
  let exp_r, exp_c =
    match exp with
    | Unit -> (r, c)
    | Int n -> ({ r with value = Value.Int n }, c)
    | Var x -> (
        let l = Loc.get x in
        if lvalue then ({ r with value = Value.Loc l }, c)
        else
          match Loc.Map.find_opt l mem with
          | Some (v, p) -> ({ r with value = v; pp = p }, c)
          | None ->
              raise
                (Runtime_error
                   ("[Mem] Location " ^ Loc.string_of_t l ^ " not found")))
    | Enable -> (r, { c with imode = Interrupt.Enabled })
    | Disable -> (r, { c with imode = Interrupt.Disabled })
    | Malloc (e1, e2) ->
        let r1, c1 = eval c e1 in
        let r2, c2 = eval c1 e2 in
        let n =
          match r1.value with
          | Value.Int n' -> n'
          | _ -> failwith "Malloc size must be an integer"
        in
        let v = r2.value in
        let new_r = (v, ProgramPoint.Label lbl) in
        let mem' =
          List.init n (fun i -> Loc.alloc lbl i)
          |> List.fold_left (fun m a -> Loc.Map.add a new_r m) c2.mem
        in
        ({ r with value = Value.Loc (Loc.alloc lbl 0) }, { c2 with mem = mem' })
    | Deref (e1, e2) -> (
        let r1, c1 = eval c e1 in
        let r2, c2 = eval c1 e2 in
        let base =
          match r1.value with
          | Value.Loc l -> l
          | _ -> failwith "Deref base must be a location"
        in
        let offset =
          match r2.value with
          | Value.Int i -> i
          | _ -> failwith "Deref offset must be an integer"
        in
        let l =
          match base with
          | Loc.VarLoc { id; offset = off } ->
              Loc.VarLoc { id; offset = off + offset }
          | Loc.HeapLoc { lbl; offset = off } ->
              Loc.HeapLoc { lbl; offset = off + offset }
        in
        if lvalue then ({ r with value = Value.Loc l }, c2)
        else
          match Loc.Map.find_opt l c2.mem with
          | Some (v, p) -> ({ r with value = v; pp = p }, c2)
          | None ->
              raise
                (Runtime_error
                   ("[Mem] Location " ^ Loc.string_of_t l ^ " not found")))
    | Bop (bop, e1, e2) -> (
        let r1, c1 = eval c e1 in
        let r2, c2 = eval c1 e2 in
        match bop with
        | Eq ->
            if Value.compare r1.value r2.value = 0 then
              ({ r with value = Value.Int 1 }, c2)
            else ({ r with value = Value.Int 0 }, c2)
        | Lt -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (if i1 < i2 then 1 else 0) }, c2)
            | _ -> failwith "Undefined operation")
        | Gt -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (if i1 > i2 then 1 else 0) }, c2)
            | _ -> failwith "Undefined operation")
        | Ne ->
            if Value.compare r1.value r2.value <> 0 then
              ({ r with value = Value.Int 1 }, c2)
            else ({ r with value = Value.Int 0 }, c2)
        | Le -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (if i1 <= i2 then 1 else 0) }, c2)
            | _ -> failwith "Undefined operation")
        | Ge -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (if i1 >= i2 then 1 else 0) }, c2)
            | _ -> failwith "Undefined operation")
        | Plus -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                let res = i1 + i2 in
                ({ r with value = Value.Int res }, c2)
            | _ -> failwith "Undefined operation")
        | Minus -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (i1 - i2) }, c2)
            | _ -> failwith "Undefined operation")
        | Times -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ({ r with value = Value.Int (i1 * i2) }, c2)
            | _ -> failwith "Undefined operation")
        | And -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ( {
                    r with
                    value = Value.Int (if i1 <> 0 && i2 <> 0 then 1 else 0);
                  },
                  c2 )
            | _ -> failwith "Undefined operation")
        | Or -> (
            match (r1.value, r2.value) with
            | Value.Int i1, Value.Int i2 ->
                ( {
                    r with
                    value = Value.Int (if i1 <> 0 || i2 <> 0 then 1 else 0);
                  },
                  c2 )
            | _ -> failwith "Undefined operation"))
    | Assign (e1, e2) ->
        let r1, c1 = eval c e1 ~lvalue:true in
        let r2, c2 = eval c1 e2 in
        let l =
          match r1.value with
          | Value.Loc l -> l
          | _ -> failwith "Left-hand side of assignment must be a location"
        in
        let mem' = Loc.Map.add l (r2.value, ProgramPoint.Label lbl) c2.mem in
        (r, { c2 with mem = mem' })
    | Seq (e1, e2) ->
        let _, c1 = eval c e1 in
        eval c1 e2
    | If (e1, e2, e3) -> (
        let r1, c1 = eval c e1 in
        match r1.value with
        | Value.Int n -> if n <> 0 then eval c1 e2 else eval c1 e3
        | _ -> failwith "Condition expression must evaluate to an integer")
    | While (_, e1, e2) -> (
        let r1, c1 = eval c e1 in
        match r1.value with
        | Value.Int n ->
            if n <> 0 then
              let _, c2 = eval c1 e2 in
              eval c2 lbl_exp
            else (r, c1)
        | _ -> failwith "Condition expression must evaluate to an integer")
  in
  match exp_r.out with
  | Outcome.Done -> (exp_r, exp_c)
  | Outcome.I iid -> (
      match HandlerStore.lookup !handlers iid with
      (* TODO: Done -> Non-Deterministic *)
      | None ->
          ({ exp_r with out = Outcome.Done }, exp_c)
          (* No handler, treat as Done *)
      | Some exp ->
          let _hdl_r, hdl_c =
            eval { exp_c with imode = Interrupt.Disabled } exp
          in
          ({ exp_r with out = Outcome.Done }, { exp_c with mem = hdl_c.mem }))

(* Helper *)
let join_res r1 r2 =
  { avalue = Abs_Val.join r1.avalue r2.avalue; app = PPSet.union r1.app r2.app }

let join_conf c1 c2 =
  {
    asem = Abs_Sem.join c1.asem c2.asem;
    amem = Abs_Mem.join c1.amem c2.amem;
    aimode = Interrupt.join c1.aimode c2.aimode;
    errs = ErrorSet.union c1.errs c2.errs;
  }

let join_out (r1, c1) (r2, c2) = (join_res r1 r2, join_conf c1 c2)

let widen_conf c1 c2 =
  {
    asem = Abs_Sem.widen c1.asem c2.asem;
    amem = Abs_Mem.widen c1.amem c2.amem;
    aimode = Interrupt.join c1.aimode c2.aimode;
    errs = ErrorSet.union c1.errs c2.errs;
  }

let leq_conf c1 c2 =
  Abs_Sem.leq c1.asem c2.asem
  && Abs_Mem.leq c1.amem c2.amem
  &&
  match (c1.aimode, c2.aimode) with
  | Interrupt.Disabled, Interrupt.Enabled -> true
  | Interrupt.Disabled, Interrupt.Disabled -> true
  | Interrupt.Enabled, Interrupt.Enabled -> true
  | Interrupt.Enabled, Interrupt.Disabled -> false

(* Convergence check for the handler post-step fixpoint.
   Excludes asem: record_sem inside eval_no_post grows asem on every
   iteration, so including it would prevent convergence even after amem
   has stabilised, causing unnecessary extra iterations. *)
let leq_conf_noasem c1 c2 =
  Abs_Mem.leq c1.amem c2.amem
  && ErrorSet.subset c1.errs c2.errs
  &&
  match (c1.aimode, c2.aimode) with
  | Interrupt.Disabled, Interrupt.Enabled -> true
  | Interrupt.Disabled, Interrupt.Disabled -> true
  | Interrupt.Enabled, Interrupt.Enabled -> true
  | Interrupt.Enabled, Interrupt.Disabled -> false

let record_sem (pp : ProgramPoint.t) (c : abs_conf) : abs_conf =
  { c with asem = Abs_Sem.weak_write c.asem pp c.amem }

(* Narrow v1 assuming (v1 bop v2) is true *)
let narrow_left (bop : Exp.bop) (v1 : Itv.t) (v2 : Itv.t) : Itv.t =
  match bop with
  | Lt ->
      let upper =
        match v2 with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (_, r) -> Itv.Itv (Itv.Bound.N_inf, Itv.Bound.pred r)
      in
      Itv.meet v1 upper
  | Le ->
      let upper =
        match v2 with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (_, r) -> Itv.Itv (Itv.Bound.N_inf, r)
      in
      Itv.meet v1 upper
  | Gt ->
      let lower =
        match v2 with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (l, _) -> Itv.Itv (Itv.Bound.succ l, Itv.Bound.P_inf)
      in
      Itv.meet v1 lower
  | Ge ->
      let lower =
        match v2 with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (l, _) -> Itv.Itv (l, Itv.Bound.P_inf)
      in
      Itv.meet v1 lower
  | Eq -> Itv.meet v1 v2
  | Ne -> (
      match v2 with
      | Itv.Itv (Itv.Bound.Z n, Itv.Bound.Z m) when n = m -> (
          match v1 with
          | Itv.Itv (Itv.Bound.Z l, r) when l = n ->
              Itv.meet v1 (Itv.Itv (Itv.Bound.Z (l + 1), r))
          | Itv.Itv (l, Itv.Bound.Z r) when r = n ->
              Itv.meet v1 (Itv.Itv (l, Itv.Bound.Z (r - 1)))
          | _ -> v1)
      | _ -> v1)
  | _ -> v1

let negate_bop (bop : Exp.bop) : Exp.bop =
  match bop with
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | other -> other

let flip_bop (bop : Exp.bop) : Exp.bop =
  match bop with Lt -> Gt | Le -> Ge | Gt -> Lt | Ge -> Le | other -> other

let get_itv_from_exp (e : Exp.t) (amem : Abs_Mem.t) : Itv.t =
  match e with
  | Int n -> Itv.alpha n
  | Var x -> (
      let loc = Abs_Loc.get x in
      match Abs_Mem.LocMap.find_opt loc amem with
      | Some ((itv, _, _), _) -> itv
      | None -> Itv.bot)
  | _ -> Itv.top

let narrow_var_in_amem (x : string) (bop : Exp.bop) (other_itv : Itv.t)
    (amem : Abs_Mem.t) : Abs_Mem.t =
  let loc = Abs_Loc.get x in
  match Abs_Mem.LocMap.find_opt loc amem with
  | None -> amem
  | Some ((curr_itv, u, l), pp) ->
      let narrowed = narrow_left bop curr_itv other_itv in
      Abs_Mem.LocMap.add loc ((narrowed, u, l), pp) amem

(* Refine abstract memory for a branch of an if-expression. [branch=true]
   assumes [cond_exp] is truthy (≠ 0). [branch=false] assumes [cond_exp] is
   falsy (= 0). *)
let refine_amem (cond_exp : Exp.lbl_t) (branch : bool) (amem : Abs_Mem.t) :
    Abs_Mem.t =
  match cond_exp.exp with
  | Bop (bop, lhs_e, rhs_e) -> (
      let actual_bop = if branch then bop else negate_bop bop in
      let v_lhs = get_itv_from_exp lhs_e.exp amem in
      let v_rhs = get_itv_from_exp rhs_e.exp amem in
      let amem1 =
        match lhs_e.exp with
        | Var x -> narrow_var_in_amem x actual_bop v_rhs amem
        | _ -> amem
      in
      match rhs_e.exp with
      | Var x -> narrow_var_in_amem x (flip_bop actual_bop) v_lhs amem1
      | _ -> amem1)
  | Var x -> (
      let loc = Abs_Loc.get x in
      match Abs_Mem.LocMap.find_opt loc amem with
      | None -> amem
      | Some ((curr_itv, u, l), pp) ->
          (* false branch: x = 0; true branch: no precise refinement for ≠ 0 in
             interval domain *)
          if branch then amem
          else
            let narrowed = Itv.meet curr_itv (Itv.alpha 0) in
            Abs_Mem.LocMap.add loc ((narrowed, u, l), pp) amem)
  | _ -> amem

let abs_unit () : Abs_Val.t = (Itv.bot, Abs_Unit.Unit, Abs_Loc.bot)
let abs_int (itv : Itv.t) : Abs_Val.t = (itv, Abs_Unit.bot, Abs_Loc.bot)
let abs_loc (l : Abs_Loc.t) : Abs_Val.t = (Itv.bot, Abs_Unit.bot, l)

let proj_int (v : Abs_Val.t) : Itv.t =
  let i, _u, _l = v in
  i

let proj_loc (v : Abs_Val.t) : Abs_Loc.t =
  let _i, _u, l = v in
  l

let get_offset (itv : Itv.t) : Itv.t =
  match itv with
  | Bot -> Bot
  | Itv (_, P_inf) -> Itv.bot
  | Itv (_, Z r) -> if r <= 0 then Itv.bot else Itv (Z 0, Z (r - 1))
  | _ -> Itv.bot

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

let find_size (lbl : Exp.Lbl.t) : Itv.t =
  match LblMap.find_opt lbl !size_tbl with Some s -> s | None -> Itv.bot

let itv_overlap (loc : Abs_Loc.t) : Itv.t * Itv.t * Itv.t =
  match loc with
  | Abs_Loc.Bot | Abs_Loc.AVarLoc _ -> (Bot, Bot, Bot)
  | Abs_Loc.Top -> (Itv.top, Itv.top, Itv.top)
  | Abs_Loc.AHeapLoc { lbl = base; offset = off } -> (
      let size = find_size base in
      match (size, off) with
      | Bot, _ | _, Bot -> (Bot, Bot, Bot)
      | _, _ ->
          let in_itv = Itv.meet size off in
          (* print_endline ("Size " ^ Itv.string_of_t size ^ ", Left "^
             Itv.string_of_t (Itv.left size) ^ ", Right " ^ Itv.string_of_t
             (Itv.right size) ^ ", Offset " ^ Itv.string_of_t off); *)
          let left_oob = Itv.meet off (Itv.left size) in
          let right_oob = Itv.meet off (Itv.right size) in
          (* print_endline ("Left oob " ^ Itv.string_of_t left_oob ^ ", Right
             oob "^ Itv.string_of_t right_oob); *)
          (in_itv, left_oob, right_oob))

let equal_check (v1 : Abs_Val.t) (v2 : Abs_Val.t) : Itv.t =
  let itv1, _u1, loc1 = v1 in
  let itv2, _u2, loc2 = v2 in
  (* print_endline ("[Equal Check] " ^ Abs_Val.string_of_t v1 ^ " vs " ^
     Abs_Val.string_of_t v2); *)
  let itv_bot = itv1 = Itv.bot || itv2 = Itv.bot in
  let loc_bot = loc1 = Abs_Loc.bot || loc2 = Abs_Loc.bot in
  if itv_bot && loc_bot then Itv.Bool.false_
  else
    let itv_must_true = Itv.single_eq itv1 itv2 in
    let itv_must_false = not (Itv.is_overlap itv1 itv2) in
    let loc_must_true = Abs_Loc.compare loc1 loc2 = 0 in
    let loc_must_false = not (loc_overlap loc1 loc2) in
    if itv_must_true && loc_must_true then Itv.Bool.true_
    else if itv_must_false && loc_must_false then Itv.Bool.false_
    else Itv.Bool.top

let is_pp_handler (pp : ProgramPoint.t) : bool =
  match pp with ProgramPoint.Label (Exp.Lbl.Handler _) -> true | _ -> false

let ppset_has_handler (pps : PPSet.t) : bool = PPSet.exists is_pp_handler pps

let add_deref_oob_errors ~(at : ProgramPoint.t) ~(access : Error.access)
    (* Read / Write *) ~(base : Abs_Loc.t)
    (* ~(offset:Itv.t) *)
    ~(in_itv : Itv.t) ~(left_oob : Itv.t) ~(right_oob : Itv.t)
    ~(base_pp : PPSet.t) (* r1.pp *) ~(offset_pp : PPSet.t) (* r2.pp *)
    (c : abs_conf) : abs_conf =
  (* OOB가 전혀 없으면 그대로 *)
  if left_oob = Itv.bot && right_oob = Itv.bot then c
  else
    let handler_caused =
      ppset_has_handler base_pp || ppset_has_handler offset_pp
    in
    match (left_oob, right_oob) with
    | Bot, Bot -> c
    | Itv _, Bot | Bot, Itv _ | Itv _, Itv _ ->
        let err =
          Error.make ~at ~access ~base ~in_itv ~left_oob ~right_oob ~base_pp
            ~offset_pp ~handler_caused
        in
        { c with errs = ErrorSet.add err c.errs }


let evA (self : ?lvalue:bool -> abs_conf -> Exp.lbl_t -> abs_res * abs_conf)
    ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) : abs_res * abs_conf
    =
  let ({ lbl; exp; _ } : Exp.lbl_t) = lbl_exp in
  let ({ amem; _ } : abs_conf) = c in
  let r = { avalue = Abs_Val.bot; app = PPSet.empty } in
  let pp = ProgramPoint.Label lbl in
  let res, c_after_eval =
    match exp with
    | Unit -> ({ r with avalue = abs_unit () }, c)
    | Int n -> ({ r with avalue = abs_int (Itv.alpha n) }, c)
    | Var x -> (
        let l = Abs_Loc.get x in
        if lvalue then ({ r with avalue = abs_loc l }, c)
        else
          match Abs_Mem.LocMap.find_opt l amem with
          | Some (v, p') -> ({ avalue = v; app = p' }, c)
          | None ->
              raise (Runtime_error ("[Abs_Mem] Variable " ^ x ^ " not found")))
    | Enable ->
        ({ r with avalue = abs_unit () }, { c with aimode = Interrupt.Enabled })
    | Disable ->
        ({ r with avalue = abs_unit () }, { c with aimode = Interrupt.Disabled })
    | Malloc (e1, e2) -> (
        let r1, c1 = self c e1 in
        let r2, c2 = self c1 e2 in
        (* TO-DO: if there is a negative value in range n_itv, it should be
           added in error list *)
        let n_itv = get_offset (proj_int r1.avalue) in
        (* max positive offset *)
        let v = r2.avalue in
        match n_itv with
        | Bot ->
            raise (Runtime_error "[Malloc] Number of allocation cannot be bot")
        | Itv _ ->
            let l = Abs_Loc.alloc lbl n_itv in
            size_tbl := LblMap.add lbl n_itv !size_tbl;
            (* [0, n-1] *)
            let base_l = Abs_Loc.alloc lbl (Itv (Z 0, Z 0)) in
            let amem' = Abs_Mem.write c2.amem l v pp in
            ({ r with avalue = abs_loc base_l }, { c2 with amem = amem' }))
    | Deref (e1, e2) -> (
        let r1, c1 = self c e1 in
        let r2, c2 = self c1 e2 in
        let base_loc = proj_loc r1.avalue in
        let offset_itv = proj_int r2.avalue in
        let shifted_loc = Abs_Loc.offset_add base_loc offset_itv in
        let full_base_loc, safe_itv, left_oob, right_oob =
          match base_loc with
          | Abs_Loc.AHeapLoc { lbl; _ } ->
              let size_itv = find_size lbl in
              let full_loc = Abs_Loc.AHeapLoc { lbl; offset = size_itv } in
              let safe_itv, left_oob, right_oob = itv_overlap shifted_loc in
              (full_loc, safe_itv, left_oob, right_oob)
          | Abs_Loc.AVarLoc _ | Abs_Loc.Bot ->
              (Abs_Loc.Bot, Itv.bot, Itv.bot, Itv.bot)
          | Abs_Loc.Top -> (Abs_Loc.Top, Itv.top, Itv.top, Itv.top)
        in

        let read_loc =
          match base_loc with
          | Abs_Loc.AHeapLoc { lbl; _ } ->
              Abs_Loc.AHeapLoc { lbl; offset = safe_itv }
          | Abs_Loc.AVarLoc _ | Abs_Loc.Bot -> Abs_Loc.Bot
          | Abs_Loc.Top -> Abs_Loc.Top
        in

        let write_loc =
          match base_loc with
          | Abs_Loc.AHeapLoc _ -> full_base_loc
          | Abs_Loc.AVarLoc _ | Abs_Loc.Bot -> Abs_Loc.Bot
          | Abs_Loc.Top -> Abs_Loc.Top
        in

        let access = if lvalue then Error.Write else Error.Read in
        let c2_err =
          add_deref_oob_errors ~at:pp ~access ~base:shifted_loc ~in_itv:safe_itv
            ~left_oob ~right_oob ~base_pp:r1.app ~offset_pp:r2.app c2
        in
        if lvalue then
          match write_loc with
          | Abs_Loc.Bot -> ({ r with avalue = Abs_Val.bot }, c2_err)
          | _ -> ({ r with avalue = abs_loc write_loc }, c2_err)
        else
          match read_loc with
          | Abs_Loc.Bot -> ({ r with avalue = Abs_Val.bot }, c2_err)
          | Abs_Loc.Top -> ({ r with avalue = Abs_Val.top }, c2_err)
          | Abs_Loc.AVarLoc _ | Abs_Loc.AHeapLoc _ ->
              let v_join, pp_join =
                Abs_Mem.fold
                  (fun (k : Abs_Loc.t) ((v, pps) : Abs_Val.t * PPSet.t)
                       (acc_v, acc_pps) ->
                    if loc_overlap read_loc k then
                      (Abs_Val.join acc_v v, PPSet.union acc_pps pps)
                    else (acc_v, acc_pps))
                  c2.amem (Abs_Val.bot, PPSet.empty)
              in
              ({ avalue = v_join; app = pp_join }, c2_err))
    | Bop (bop, e1, e2) -> (
        let r1, c1 = self c e1 in
        let r2, c2 = self c1 e2 in
        match bop with
        | Eq ->
            ( {
                avalue = abs_int (equal_check r1.avalue r2.avalue);
                app = PPSet.empty;
              },
              c2 )
        | Lt ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.lt v1 v2); app = PPSet.empty }, c2)
        | Gt ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.gt v1 v2); app = PPSet.empty }, c2)
        | Ne ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.ne v1 v2); app = PPSet.empty }, c2)
        | Le ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.le v1 v2); app = PPSet.empty }, c2)
        | Ge ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.ge v1 v2); app = PPSet.empty }, c2)
        | Plus ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            let res = Itv.add v1 v2 in
            ({ avalue = abs_int res; app = PPSet.union r1.app r2.app }, c2)
        | Minus ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            let res = Itv.add v1 (Itv.neg v2) in
            ({ avalue = abs_int res; app = PPSet.union r1.app r2.app }, c2)
        | Times ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ( {
                avalue = abs_int (Itv.mul v1 v2);
                app = PPSet.union r1.app r2.app;
              },
              c2 )
        | And ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.and_ v1 v2); app = PPSet.empty }, c2)
        | Or ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.or_ v1 v2); app = PPSet.empty }, c2))
    | Assign (e1, e2) ->
        let r1, c1 = self ~lvalue:true c e1 in
        let r2, c2 = self c1 e2 in
        let l = proj_loc r1.avalue in
        let amem' = Abs_Mem.write c2.amem l r2.avalue pp in
        ({ avalue = abs_unit (); app = PPSet.empty }, { c2 with amem = amem' })
    | Seq (e1, e2) ->
        let _, c1 = self c e1 in
        self c1 e2
    | If (e1, e2, e3) ->
        let r1, c1 = self c e1 in
        let v1 = proj_int r1.avalue in
        if v1 = Itv.Bool.true_ then
          let r2, c2 = self c1 e2 in
          (r2, c2)
        else if v1 = Itv.Bool.false_ then
          let r3, c3 = self c1 e3 in
          (r3, c3)
        else
          let c1_true = { c1 with amem = refine_amem e1 true c1.amem } in
          let c1_false = { c1 with amem = refine_amem e1 false c1.amem } in
          let r2, c2 = self c1_true e2 in
          let r3, c3 = self c1_false e3 in
          join_out (r2, c2) (r3, c3)
    | While (_id, econd, ebody) ->
        let rec iterate (i : int) (input : abs_conf) : abs_conf =
          let rcond, ccond = self input econd in
          let cond_itv = proj_int rcond.avalue in
          if cond_itv = Itv.Bool.false_ then ccond
          else begin
            let _rbody, cbody = self ccond ebody in
            let next =
              if cond_itv = Itv.Bool.top then join_conf ccond cbody else cbody
            in
            (* widen condition *)
            let wconf = if i < widen_cnt then next else widen_conf input next in
            if leq_conf_noasem wconf input then input else iterate (i + 1) wconf
          end
        in
        let output = iterate 0 c in
        ({ avalue = abs_unit (); app = PPSet.empty }, output)
  in
  match c.aimode with
  (* Check imode of input config *)
  | Disabled ->
      let c_recorded = record_sem pp c_after_eval in
      (res, c_recorded)
  | Enabled ->
      (* Restrict handler post-steps to the expressions where they carry new
         information, rather than firing at every AST node.
         - Assign / Malloc : amem actually changes here; most important yield.
         - Var             : reading a variable models the handler firing and
                             modifying that variable between a condition check
                             and the body that re-reads it (TOCTOU pattern).
         Excluded: Int, Bop, Seq, If, While, Enable, Disable, Deref — for
         pure expressions amem is unchanged so the handler effect would be
         identical to the one already recorded at the nearest Var/Assign. *)
      let is_yield_point =
        match exp with Assign _ | Malloc _ | Var _ -> true | _ -> false
      in
      if is_yield_point then
        let c_after_post = !post_steps_fn c_after_eval in
        let c_recorded = record_sem pp c_after_post in
        (res, c_recorded)
      else
        let c_recorded = record_sem pp c_after_eval in
        (res, c_recorded)

let rec evalA ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) :
    abs_res * abs_conf =
  evA evalA ~lvalue c lbl_exp

(* Evaluates lbl_exp under c without ever triggering the handler post-step.
   Forces aimode = Disabled at every recursive level so that the Enabled branch
   of evA is never reached during summary application. *)
let rec eval_no_post ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) :
    abs_res * abs_conf =
  evA eval_no_post ~lvalue { c with aimode = Interrupt.Disabled } lbl_exp

(* Apply one atomic effect (a compiled assignment) to the abstract conf.
   Uses eval_no_post for both the lvalue (write target) and rvalue so that
   no handler post-step is triggered during summary application. *)
let apply_atom (atom : summary_atom) (c : abs_conf) : abs_conf =
  let r1, c1 = eval_no_post ~lvalue:true c atom.lhs in
  let r2, c2 = eval_no_post c1 atom.rhs in
  let l = proj_loc r1.avalue in
  let pp = ProgramPoint.Label atom.assign_lbl in
  match l with
  | Abs_Loc.Bot -> c2
  | _ ->
      let amem' = Abs_Mem.write c2.amem l r2.avalue pp in
      { c2 with amem = amem' }

(* Apply a handler summary to c (which must already have aimode = Disabled).
   Compiled: apply each atom in sequence using eval_no_post.
   Fallback:  evaluate the handler body directly with eval_no_post — still no
              recursive post-step, but skips the atom fast-path. *)
let apply_handler_summary (summary : handler_summary) (c : abs_conf) : abs_conf
    =
  match summary with
  | Compiled atoms ->
      List.fold_left (fun acc_c atom -> apply_atom atom acc_c) c atoms
  | Fallback body ->
      let _r, c' = eval_no_post c body in
      c'

(* One step of the handler fixpoint using precompiled summaries.
   Each handler is evaluated with asem = bot to avoid record_sem calls inside
   eval_no_post from inflating the main asem on every invocation.  The handler
   body's own semantic states (small and bounded) are merged back once at the
   end so provenance tracing can still reach handler expressions. *)
let post_step_summary (c : abs_conf) : abs_conf =
  let input_c = c in
  (* Strip asem so handler-body record_sem calls don't grow it each iteration *)
  let input_clean = { input_c with asem = Abs_Sem.bot; aimode = Interrupt.Disabled } in
  let joined =
    IidSet.fold
      (fun iid acc ->
        match HandlerStore.IidMap.find_opt iid !handler_summaries with
        | None -> acc
        | Some summary ->
            let c' = apply_handler_summary summary input_clean in
            join_conf acc c')
      !iset { input_clean with aimode = c.aimode }
  in
  (* Restore aimode; merge bounded handler-body asem into the caller's asem *)
  { joined with aimode = c.aimode; asem = Abs_Sem.join c.asem joined.asem }

(* Iterate post_step_summary to a fixpoint with widening.
   Uses leq_conf_noasem so that asem growth from record_sem calls inside
   eval_no_post does not prevent convergence on amem. *)
let post_steps_summary (c0 : abs_conf) : abs_conf =
  let rec iterate (i : int) (cur : abs_conf) : abs_conf =
    let stepped = post_step_summary cur in
    let joined = join_conf cur stepped in
    if leq_conf_noasem joined cur then cur
    else
      let next = if i < widen_cnt then joined else widen_conf cur joined in
      if leq_conf_noasem next cur then cur else iterate (i + 1) next
  in
  iterate 0 c0

(* ===== Pre-compiled fixpoint: classification helpers ===== *)

(* A variable whose name is all-uppercase (letters, digits, underscores) is
   treated as a compile-time constant.  Its value is resolved to a concrete
   integer from init_amem so that assignments like [x := FAULT_SLOT] are
   classified as FPS_JoinConsts [200] rather than FPS_Top. *)
let is_const_name (s : string) : bool =
  String.length s > 0
  && String.for_all
       (fun c -> (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')
       s

(* Build a name→int map for every all-uppercase scalar variable in init_amem
   whose abstract value is a singleton interval [n, n]. *)
let build_const_map (init_amem : Abs_Mem.t) : (string, int) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  Abs_Mem.fold
    (fun loc (v, _) () ->
      match loc with
      | Abs_Loc.AVarLoc { id = x; offset = Itv.Itv (Itv.Bound.Z 0, Itv.Bound.Z 0) }
        when is_const_name x ->
          let (itv, _, _) = v in
          (match itv with
          | Itv.Itv (Itv.Bound.Z n, Itv.Bound.Z m) when n = m ->
              Hashtbl.replace tbl x n
          | _ -> ())
      | _ -> ())
    init_amem ();
  tbl

let combine_fp_scalar (a : fp_scalar) (b : fp_scalar) : fp_scalar =
  match (a, b) with
  | FPS_Top, _ | _, FPS_Top -> FPS_Top
  | FPS_Unchanged, x | x, FPS_Unchanged -> x
  | FPS_Inc, FPS_Inc -> FPS_Inc
  | FPS_Dec, FPS_Dec -> FPS_Dec
  | FPS_Inc, FPS_Dec | FPS_Dec, FPS_Inc -> FPS_Top
  | FPS_Inc, FPS_JoinConsts _ | FPS_JoinConsts _, FPS_Inc -> FPS_Inc
  | FPS_Dec, FPS_JoinConsts _ | FPS_JoinConsts _, FPS_Dec -> FPS_Dec
  | FPS_JoinConsts c1, FPS_JoinConsts c2 -> FPS_JoinConsts (Itv.join c1 c2)

(* Apply a pre-compiled scalar fixpoint effect to an abstract value *)
let apply_fp_scalar (fps : fp_scalar) ((itv, u, l) : Abs_Val.t) : Abs_Val.t =
  let new_itv =
    match fps with
    | FPS_Unchanged -> itv
    | FPS_Inc -> (
        match itv with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (lo, _) -> Itv.Itv (lo, Itv.Bound.P_inf))
    | FPS_Dec -> (
        match itv with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (_, hi) -> Itv.Itv (Itv.Bound.N_inf, hi))
    | FPS_JoinConsts c -> Itv.join itv c
    | FPS_Top -> Itv.top
  in
  (new_itv, u, l)

(* Classify one summary_atom into scalar and array-write effects.
   init_amem is used to resolve which heap block an array pointer refers to. *)
let classify_atom (atom : summary_atom) (init_amem : Abs_Mem.t)
    (const_map : (string, int) Hashtbl.t)
    (scalar_tbl : (Abs_Loc.t, fp_scalar * PPSet.t) Hashtbl.t)
    (array_writes : fp_array_write list ref) : unit =
  match atom.lhs.exp with
  | Var x ->
      let loc = Abs_Loc.get x in
      let fps =
        match atom.rhs.exp with
        | Int n -> FPS_JoinConsts (Itv.alpha n)
        | Bop (Plus, { exp = Var y; _ }, { exp = Int n; _ })
          when String.equal x y ->
            if n > 0 then FPS_Inc else if n < 0 then FPS_Dec else FPS_Unchanged
        | Bop (Plus, { exp = Int n; _ }, { exp = Var y; _ })
          when String.equal x y ->
            if n > 0 then FPS_Inc else if n < 0 then FPS_Dec else FPS_Unchanged
        | Bop (Minus, { exp = Var y; _ }, { exp = Int n; _ })
          when String.equal x y ->
            if n > 0 then FPS_Dec else if n < 0 then FPS_Inc else FPS_Unchanged
        | Var y when is_const_name y ->
            (match Hashtbl.find_opt const_map y with
            | Some n -> FPS_JoinConsts (Itv.alpha n)
            | None -> FPS_Top)
        | _ -> FPS_Top
      in
      let handler_pp = ProgramPoint.Label atom.assign_lbl in
      let old_fps, old_pps =
        match Hashtbl.find_opt scalar_tbl loc with
        | None -> (FPS_Unchanged, PPSet.empty)
        | Some (f, pps) -> (f, pps)
      in
      Hashtbl.replace scalar_tbl loc
        (combine_fp_scalar old_fps fps, PPSet.add handler_pp old_pps)
  | Deref ({ exp = Var arr_name; _ }, idx_e) ->
      (* Array write: *arr_name[idx] := rhs.
         Look up which heap block arr_name points to in init_amem. *)
      let arr_var_loc = Abs_Loc.get arr_name in
      (match Abs_Mem.LocMap.find_opt arr_var_loc init_amem with
      | Some ((_, _, Abs_Loc.AHeapLoc { lbl; _ }), _) ->
          let fpa_idx =
            match idx_e.exp with
            | Int n -> `Const (Itv.alpha n)
            | Var idx_name -> `Var (Abs_Loc.get idx_name)
            | _ -> `Const Itv.top
          in
          let fpa_rhs =
            match atom.rhs.exp with
            | Int n -> `Const (abs_int (Itv.alpha n))
            | Var v -> `Var (Abs_Loc.get v)
            | _ -> `Const Abs_Val.top
          in
          let fpa_at = ProgramPoint.Label atom.assign_lbl in
          array_writes :=
            { fpa_lbl = lbl; fpa_idx; fpa_rhs; fpa_at;
              fpa_offset_pp = PPSet.singleton fpa_at }
            :: !array_writes
      | _ -> ())
  | _ -> ()

(* Scan all Compiled handler summaries and build a compiled_fixpoint.
   Fallback handlers are left to the iterative fallback path. *)
let compile_fixpoint (init_amem : Abs_Mem.t) : unit =
  let const_map = build_const_map init_amem in
  let scalar_tbl : (Abs_Loc.t, fp_scalar * PPSet.t) Hashtbl.t = Hashtbl.create 16 in
  let array_writes : fp_array_write list ref = ref [] in
  HandlerStore.IidMap.iter
    (fun _iid summary ->
      match summary with
      | Compiled atoms ->
          List.iter
            (fun atom ->
              classify_atom atom init_amem const_map scalar_tbl array_writes)
            atoms
      | Fallback _ -> ())
    !handler_summaries;
  let fp_scalars =
    Hashtbl.fold (fun loc (fps, pps) acc -> (loc, fps, pps) :: acc) scalar_tbl []
  in
  compiled_fp := { fp_scalars; fp_arrays = !array_writes };
  let has_fallback =
    HandlerStore.IidMap.exists
      (fun _iid s -> match s with Fallback _ -> true | _ -> false)
      !handler_summaries
  in
  use_compiled_fp := not has_fallback

(* Apply the pre-compiled fixpoint to an abstract conf in two passes:
   1. Scalar pass: update each scalar loc's interval according to its fp_scalar.
      - FPS_Inc  →  upper bound → +∞  (models unbounded increments at fixpoint)
      - FPS_Dec  →  lower bound → -∞  (models unbounded decrements at fixpoint)
      - FPS_JoinConsts c  →  join current itv with c
      - FPS_Top  →  set to top
   2. Array pass: for each fp_array_write, resolve the index and value
      from the post-scalar amem (so that widened scalar values propagate
      into array index expressions), perform a weak write, and check OOB. *)
let apply_compiled_fixpoint (fp : compiled_fixpoint) (c : abs_conf) : abs_conf =
  (* Pass 1: scalar effects *)
  let amem1 =
    List.fold_left
      (fun amem (loc, fps, handler_pps) ->
        match fps with
        | FPS_Unchanged -> amem
        | _ -> (
            match Abs_Mem.LocMap.find_opt loc amem with
            | None -> amem
            | Some (v, existing_pps) ->
                let v' = apply_fp_scalar fps v in
                let new_pps = PPSet.union existing_pps handler_pps in
                Abs_Mem.LocMap.add loc (v', new_pps) amem))
      c.amem fp.fp_scalars
  in
  (* Pass 2: array write effects using post-scalar amem for index/value lookup *)
  let amem2, errs2 =
    List.fold_left
      (fun (amem, errs) (fpa : fp_array_write) ->
        let offset_itv =
          match fpa.fpa_idx with
          | `Const itv -> itv
          | `Var idx_loc -> (
              match Abs_Mem.LocMap.find_opt idx_loc amem with
              | Some ((itv, _, _), _) -> itv
              | None -> Itv.bot)
        in
        let write_loc =
          Abs_Loc.AHeapLoc { lbl = fpa.fpa_lbl; offset = offset_itv }
        in
        let in_itv, left_oob, right_oob = itv_overlap write_loc in
        let errs' =
          if left_oob = Itv.bot && right_oob = Itv.bot then errs
          else
            let err =
              Error.make ~at:fpa.fpa_at ~access:Error.Write ~base:write_loc
                ~in_itv ~left_oob ~right_oob ~base_pp:PPSet.empty
                ~offset_pp:fpa.fpa_offset_pp ~handler_caused:true
            in
            ErrorSet.add err errs
        in
        let rhs_val =
          match fpa.fpa_rhs with
          | `Const v -> v
          | `Var val_loc -> (
              match Abs_Mem.LocMap.find_opt val_loc amem with
              | Some (v, _) -> v
              | None -> Abs_Val.bot)
        in
        let amem' =
          if in_itv = Itv.bot then amem
          else Abs_Mem.write amem write_loc rhs_val fpa.fpa_at
        in
        (amem', errs'))
      (amem1, c.errs) fp.fp_arrays
  in
  { c with amem = amem2; errs = errs2 }

(* apply_fixpoint_to_conf: the new post_steps_fn.
   For programs where all handlers compiled successfully, apply the pre-compiled
   fixpoint in O(|scalars| + |arrays|) — no iteration needed.
   For programs with Fallback handlers (loops / malloc in handler body), fall
   back to the iterative post_steps_summary for soundness. *)
let apply_fixpoint_to_conf (c : abs_conf) : abs_conf =
  if !use_compiled_fp then apply_compiled_fixpoint !compiled_fp c
  else post_steps_summary c

(* Wire up the forward reference now that apply_fixpoint_to_conf is defined. *)
let () = post_steps_fn := apply_fixpoint_to_conf

let string_of_summary_atom (a : summary_atom) : string =
  Printf.sprintf "  at [%s]  %s  :=  %s"
    (Exp.Lbl.string_of_t a.assign_lbl)
    (Exp.string_of_t a.lhs.exp)
    (Exp.string_of_t a.rhs.exp)

let string_of_handler_summary (iid : int) (s : handler_summary) : string =
  match s with
  | Compiled [] ->
      Printf.sprintf "handler %d  →  Compiled  (no memory effects)" iid
  | Compiled atoms ->
      Printf.sprintf "handler %d  →  Compiled\n%s" iid
        (String.concat "\n" (List.map string_of_summary_atom atoms))
  | Fallback body ->
      Printf.sprintf "handler %d  →  Fallback  (body: %s)" iid
        (Exp.string_of_t body.exp)

let print_handler_summaries () : unit =
  print_endline "=== Handler Summaries ===";
  HandlerStore.IidMap.iter
    (fun iid s ->
      print_endline (string_of_handler_summary iid s))
    !handler_summaries;
  print_endline "========================="

(* Pre-processing: compile a handler body AST into a symbolic summary.
   Assign nodes become atoms; sequences and if-branches are flattened.
   Loops and malloc fall back to full no-post evaluation for soundness. *)
let rec compile_handler (lbl_exp : Exp.lbl_t) : handler_summary =
  match lbl_exp.exp with
  | Assign (lhs, rhs) ->
      let assign_lbl = lbl_exp.lbl in
      (match lhs.exp with
      | Var _ | Deref _ -> Compiled [ { lhs; rhs; assign_lbl } ]
      | _ -> Fallback lbl_exp)
  | Seq (e1, e2) -> (
      match (compile_handler e1, compile_handler e2) with
      | Compiled a1, Compiled a2 -> Compiled (a1 @ a2)
      | _ -> Fallback lbl_exp)
  | If (_, e_then, e_else) -> (
      (* Both branches are included as an over-approximation; only one is
         actually taken but we do not track the condition here. *)
      match (compile_handler e_then, compile_handler e_else) with
      | Compiled a1, Compiled a2 -> Compiled (a1 @ a2)
      | _ -> Fallback lbl_exp)
  | Enable | Disable | Unit | Int _ | Var _ | Bop _ | Deref _ ->
      (* No memory-modifying effect *)
      Compiled []
  | While _ | Malloc _ ->
      (* Conservative: cannot represent loop or allocation symbolically *)
      Fallback lbl_exp

let init_conf (pgm : Program.t) : conf =
  let c0 = { env = Env.empty; mem = Mem.empty; imode = Interrupt.Enabled } in
  let _, c_globals = eval c0 pgm.global in
  let hs', iset' =
    List.fold_left
      (fun (hs, iset) (d : Handler.t) ->
        let hs = HandlerStore.add hs d.iid d.body in
        let iset = IidSet.add (Handler.get_iid d) iset in
        (hs, iset))
      (HandlerStore.empty, IidSet.empty)
      pgm.handler
  in
  iset := iset';
  handlers := hs';
  { env = c_globals.env; mem = c_globals.mem; imode = c_globals.imode }

let def_intp (pgm : Program.t) : Mem.t =
  let c_init = init_conf pgm in
  print_endline "=== Initial Memory ===";
  print_endline (Mem.string_of_t c_init.mem);
  print_endline "=== Final Memory ===";
  let _, c_final = eval c_init pgm.main in
  c_final.mem

let init_confa (pgm : Program.t) : abs_conf =
  let c0 =
    {
      asem = Abs_Sem.bot;
      amem = Abs_Mem.bot;
      aimode = Interrupt.Disabled;
      errs = ErrorSet.empty;
    }
  in
  let _, c_globals = evalA c0 pgm.global in
  let hs', iset' =
    List.fold_left
      (fun (hs, iset) (d : Handler.t) ->
        let hs = HandlerStore.add hs d.iid d.body in
        let iset = IidSet.add (Handler.get_iid d) iset in
        (hs, iset))
      (HandlerStore.empty, IidSet.empty)
      pgm.handler
  in
  iset := iset';
  handlers := hs';
  let summaries =
    List.fold_left
      (fun acc (d : Handler.t) ->
        HandlerStore.IidMap.add d.iid (compile_handler d.body) acc)
      HandlerStore.IidMap.empty pgm.handler
  in
  handler_summaries := summaries;
  compile_fixpoint c_globals.amem;
  print_handler_summaries ();
  {
    asem = c_globals.asem;
    amem = c_globals.amem;
    aimode = Interrupt.Enabled;
    errs = c_globals.errs;
  }

let filter_main_sem (asem : Abs_Sem.t) : Abs_Sem.t =
  Abs_Sem.fold
    (fun pp mem acc ->
      match pp with
      | ProgramPoint.Label (Exp.Lbl.Main _)
      | ProgramPoint.Label (Exp.Lbl.Init 1) ->
          Abs_Sem.write acc pp mem
      | _ -> acc)
    asem Abs_Sem.bot

let abs_def_intp (pgm : Program.t) : Abs_Sem.t =
  let c_init = init_confa pgm in
  let _, c_final = evalA c_init pgm.main in
  filter_main_sem c_final.asem

let abs_analyze (pgm : Program.t) : Abs_Sem.t * ErrorSet.t =
  let c_init = init_confa pgm in
  let _, c_final = evalA c_init pgm.main in
  (c_final.asem, c_final.errs)
