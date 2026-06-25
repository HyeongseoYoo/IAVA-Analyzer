open Syntax
open Domain
open Abs_dom
open Interp

type abs_conf = {
  amem : Abs_Mem.t;
  aimode : Interrupt.t;
}

type abs_res = { avalue : Abs_Val.t; app : PPSet.t }

(* A single memory-modifying effect compiled from a handler body *)
type summary_atom = {
  lhs : Exp.lbl_t;
  rhs : Exp.lbl_t;
  assign_lbl : Exp.Lbl.t;
  guards : (Exp.bop * Exp.lbl_t) list;
}

(* Pre-compiled symbolic summary of a handler body *)
type handler_summary =
  | Compiled of summary_atom list
  | Fallback of Exp.lbl_t

(* Pre-compiled fixpoint types *)

(* Fixpoint effect of all handlers combined on a single scalar location *)
type fp_scalar =
  | FPS_Unchanged           (* no handler writes here *)
  | FPS_Inc                 (* x := x + c (c>0): fixpoint sets upper → +∞ *)
  | FPS_Dec                 (* x := x - c (c>0): fixpoint sets lower → -∞ *)
  | FPS_JoinConsts of Itv.t (* x := constant: join current value with this itv *)
  | FPS_JoinVal of Abs_Val.t (* x := abstract constant, such as &y *)
  | FPS_Top                 (* complex write: widen to top *)

(* A single heap-array write effect extracted from handler bodies *)
type fp_array_write = {
  fpa_lbl       : Exp.Lbl.t;                              (* heap alloc label *)
  fpa_idx       : [ `Const of Itv.t | `Var of Abs_Loc.t ]; (* index: constant or var *)
  fpa_rhs       : [ `Const of Abs_Val.t | `Var of Abs_Loc.t ]; (* value: const or var *)
  fpa_at        : ProgramPoint.t;
  fpa_offset_pp : PPSet.t;
  fpa_guards    : (Exp.bop * Exp.lbl_t) list; (* branch guards applied before index lookup *)
  fpa_base_var  : Abs_Loc.t; (* base pointer variable of the array write *)
}

type compiled_fixpoint = {
  fp_scalars : (Abs_Loc.t * (fp_scalar * PPSet.t) list) list;
  fp_arrays  : fp_array_write list;
}

let aenv : Abs_Env.t ref = ref Abs_Env.empty
let aenv0 : Abs_Env.t ref = ref Abs_Env.empty
let size_tbl : Itv.t LblMap.t ref = ref LblMap.empty
let handler_summaries : handler_summary HandlerStore.IidMap.t ref =
  ref HandlerStore.IidMap.empty
let compiled_fp : compiled_fixpoint ref =
  ref { fp_scalars = []; fp_arrays = [] }
let use_compiled_fp : bool ref = ref false
let use_selective_handler_application : bool ref = ref true
let asem : Abs_Sem.t ref = ref Abs_Sem.bot
let errs : ErrorSet.t ref = ref ErrorSet.empty
let in_handler_exec : bool ref = ref false

(* forward ref, set to apply_fixpoint_to_conf below *)
let post_steps_fn : (abs_conf -> abs_conf) ref = ref (fun c -> c)
let widen_cnt = 3

let reset_outputs () =
  asem := Abs_Sem.bot;
  errs := ErrorSet.empty;
  aenv := Abs_Env.empty;
  aenv0 := Abs_Env.empty;
  in_handler_exec := false;
  use_selective_handler_application := true

let join_res r1 r2 =
  { avalue = Abs_Val.join r1.avalue r2.avalue; app = PPSet.union r1.app r2.app }

let join_conf c1 c2 =
  {
    amem = Abs_Mem.join c1.amem c2.amem;
    aimode = Interrupt.join c1.aimode c2.aimode;
  }

let join_out (r1, c1) (r2, c2) = (join_res r1 r2, join_conf c1 c2)

let widen_conf c1 c2 =
  {
    amem = Abs_Mem.widen c1.amem c2.amem;
    aimode = Interrupt.join c1.aimode c2.aimode;
  }

let leq_conf c1 c2 =
  Abs_Mem.leq c1.amem c2.amem
  &&
  match (c1.aimode, c2.aimode) with
  | Interrupt.Disabled, Interrupt.Enabled -> true
  | Interrupt.Disabled, Interrupt.Disabled -> true
  | Interrupt.Enabled, Interrupt.Enabled -> true
  | Interrupt.Enabled, Interrupt.Disabled -> false

let leq_conf_noasem c1 c2 =
  Abs_Mem.leq c1.amem c2.amem
  &&
  match (c1.aimode, c2.aimode) with
  | Interrupt.Disabled, Interrupt.Enabled -> true
  | Interrupt.Disabled, Interrupt.Disabled -> true
  | Interrupt.Enabled, Interrupt.Enabled -> true
  | Interrupt.Enabled, Interrupt.Disabled -> false

let record_sem (pp : ProgramPoint.t) (c : abs_conf) : unit =
  asem := Abs_Sem.weak_write !asem pp c.amem

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
      match Abs_Env.find !aenv x with
      | None -> Itv.bot
      | Some loc ->
          match Abs_Mem.LocMap.find_opt loc amem with
          | Some ((itv, _, _), _) -> itv
          | None -> Itv.bot)
  | _ -> Itv.top

let narrow_var_in_amem (x : string) (bop : Exp.bop) (other_itv : Itv.t)
    (amem : Abs_Mem.t) : Abs_Mem.t =
  match Abs_Env.find !aenv x with
  | None -> amem
  | Some loc ->
      match Abs_Mem.LocMap.find_opt loc amem with
      | None -> amem
      | Some ((curr_itv, u, l), pp) ->
          let narrowed = narrow_left bop curr_itv other_itv in
          Abs_Mem.LocMap.add loc ((narrowed, u, l), pp) amem

(* Narrow x in amem assuming guard = Some(bop, Bop(_, Var x, rhs)) is true. *)
let refine_amem_by_guard (guard : (Exp.bop * Exp.lbl_t) option)
    (amem : Abs_Mem.t) : Abs_Mem.t =
  match guard with
  | None -> amem
  | Some (eff_bop, cond_e) -> (
      match cond_e.exp with
      | Bop (_, { exp = Var guard_var; _ }, rhs_e) ->
          let bound_itv = get_itv_from_exp rhs_e.exp amem in
          narrow_var_in_amem guard_var eff_bop bound_itv amem
      | _ -> amem)

let refine_amem_by_guards (guards : (Exp.bop * Exp.lbl_t) list)
    (amem : Abs_Mem.t) : Abs_Mem.t =
  List.fold_left
    (fun acc (bop, cond_e) -> refine_amem_by_guard (Some (bop, cond_e)) acc)
    amem guards

(* Narrow amem for the true (branch=true) or false (branch=false) branch of cond_exp. *)
let refine_amem (cond_exp : Exp.lbl_t) (branch : bool) (amem : Abs_Mem.t) :
    Abs_Mem.t =
  match cond_exp.exp with
  | Bop (bop, lhs_e, rhs_e) -> (
      let actual_bop = if branch then bop else negate_bop bop in
      let v_lhs = get_itv_from_exp lhs_e.exp amem in
      let amem1 = refine_amem_by_guard (Some (actual_bop, cond_exp)) amem in
      match rhs_e.exp with
      | Var x -> narrow_var_in_amem x (flip_bop actual_bop) v_lhs amem1
      | _ -> amem1)
  | Var x -> (
      match Abs_Env.find !aenv x with
      | None -> amem
      | Some loc ->
          match Abs_Mem.LocMap.find_opt loc amem with
          | None -> amem
          | Some ((curr_itv, u, l), pp) ->
              (* false branch: narrow x to 0; true branch: no refinement *)
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
          let left_oob = Itv.meet off (Itv.left size) in
          let right_oob = Itv.meet off (Itv.right size) in
          (in_itv, left_oob, right_oob))

let equal_check (v1 : Abs_Val.t) (v2 : Abs_Val.t) : Itv.t =
  let itv1, _u1, loc1 = v1 in
  let itv2, _u2, loc2 = v2 in
  let has_int1 = itv1 <> Itv.bot in
  let has_int2 = itv2 <> Itv.bot in
  let has_loc1 = loc1 <> Abs_Loc.bot in
  let has_loc2 = loc2 <> Abs_Loc.bot in
  (* Return definite answer only when both int and loc components agree. *)
  let int_answer =
    if not has_int1 && not has_int2 then None
    else if has_int1 && has_int2 then
      if Itv.single_eq itv1 itv2 then Some Itv.Bool.true_
      else if not (Itv.is_overlap itv1 itv2) then Some Itv.Bool.false_
      else Some Itv.Bool.top
    else Some Itv.Bool.false_
  in
  let loc_answer =
    if not has_loc1 && not has_loc2 then None
    else if has_loc1 && has_loc2 then
      if Abs_Loc.single_eq loc1 loc2 then Some Itv.Bool.true_
      else if not (loc_overlap loc1 loc2) then Some Itv.Bool.false_
      else Some Itv.Bool.top
    else Some Itv.Bool.false_
  in
  match (int_answer, loc_answer) with
  | None, None -> Itv.Bool.false_
  | Some a, None | None, Some a -> a
  | Some a, Some b ->
      if a = b then a else Itv.Bool.top

let is_pp_handler (pp : ProgramPoint.t) : bool =
  match pp with ProgramPoint.Label (Exp.Lbl.Handler _) -> true | _ -> false

let ppset_has_handler (pps : PPSet.t) : bool = PPSet.exists is_pp_handler pps

let add_deref_oob_errors ~(at : ProgramPoint.t) ~(access : Error.access)
    ~(base : Abs_Loc.t) ~(in_itv : Itv.t) ~(left_oob : Itv.t)
    ~(right_oob : Itv.t) ~(base_pp : PPSet.t) ~(offset_pp : PPSet.t)
    (c : abs_conf) : abs_conf =
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
        errs := ErrorSet.add err !errs;
        c


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
        if lvalue then
          let l =
            if !in_handler_exec then
              match Abs_Env.find !aenv x with
              | Some existing -> existing
              | None ->
                  let loc = Abs_Loc.get (x ^ "#") in
                  aenv := Abs_Env.write !aenv x loc;
                  loc
            else begin
              let l = Abs_Loc.get x in
              aenv := Abs_Env.write !aenv x l;
              l
            end
          in
          ({ r with avalue = abs_loc l }, c)
        else
          let loc =
            match Abs_Env.find !aenv x with
            | Some l -> l
            | None -> raise (Runtime_error ("[Abs_Env] " ^ x ^ " not declared"))
          in
          (match Abs_Mem.LocMap.find_opt loc amem with
           | Some (v, p') -> ({ avalue = v; app = p' }, c)
           | None -> raise (Runtime_error ("[Abs_Mem] " ^ x ^ " not initialized"))))
    | AddrOf x -> (
        match Abs_Env.find !aenv x with
        | Some loc -> ({ r with avalue = abs_loc loc }, c)
        | None -> raise (Runtime_error ("[Abs_Env] AddrOf: " ^ x ^ " not declared")))
    | Enable ->
        ({ r with avalue = abs_unit () }, { c with aimode = Interrupt.Enabled })
    | Disable ->
        ({ r with avalue = abs_unit () }, { c with aimode = Interrupt.Disabled })
    | Malloc (e1, e2) -> (
        let r1, c1 = self c e1 in
        let r2, c2 = self c1 e2 in
        (* TODO: report negative malloc sizes *)
        let n_itv = get_offset (proj_int r1.avalue) in
        let v = r2.avalue in
        match n_itv with
        | Bot ->
            raise (Runtime_error "[Malloc] Number of allocation cannot be bot")
        | Itv _ ->
            let l = Abs_Loc.alloc lbl n_itv in
            size_tbl := LblMap.add lbl n_itv !size_tbl;
            let base_l = Abs_Loc.alloc lbl (Itv (Z 0, Z 0)) in
            (* Store finite heap allocations as singleton cells. *)
            let heap_cells =
              match Abs_Loc.heap_singletons_opt l with
              | Some cells -> cells
              | None -> [ l ]
            in
            let amem' =
              List.fold_left
                (fun amem cell -> Abs_Mem.write amem cell v pp)
                c2.amem heap_cells
            in
            ({ r with avalue = abs_loc base_l }, { c2 with amem = amem' }))
    | Deref (e1, e2) -> (
        let r1, c1 = self c e1 in
        let r2, c2 = self c1 e2 in
        let base_loc = proj_loc r1.avalue in
        let offset_itv = proj_int r2.avalue in
        let shifted_loc = Abs_Loc.offset_add base_loc offset_itv in
        (* Clip dereference targets to in-bounds offsets before memory access. *)
        let safe_itv, left_oob, right_oob =
          match base_loc with
          | Abs_Loc.AHeapLoc _ -> itv_overlap shifted_loc
          | Abs_Loc.AVarLoc _ ->
              (* TODO: only offset 0 is valid for variable addresses. *)
              if Itv.leq offset_itv (Itv.alpha 0) then
                (Itv.alpha 0, Itv.bot, Itv.bot)
              else
                raise
                  (Runtime_error
                     "[Deref] Variable-address dereference only supports offset 0")
          | Abs_Loc.Bot -> (Itv.bot, Itv.bot, Itv.bot)
          | Abs_Loc.Top -> (Itv.top, Itv.top, Itv.top)
        in

        let access_loc =
          match shifted_loc with
          | Abs_Loc.AHeapLoc { lbl; _ } ->
              Abs_Loc.AHeapLoc { lbl; offset = safe_itv }
          | Abs_Loc.AVarLoc _ -> shifted_loc
          | Abs_Loc.Bot -> Abs_Loc.Bot
          | Abs_Loc.Top -> Abs_Loc.Top
        in

        let access = if lvalue then Error.Write else Error.Read in
        let c2_err =
          add_deref_oob_errors ~at:pp ~access ~base:shifted_loc ~in_itv:safe_itv
            ~left_oob ~right_oob ~base_pp:r1.app ~offset_pp:r2.app c2
        in
        if lvalue then
          match access_loc with
          | Abs_Loc.Bot -> ({ r with avalue = Abs_Val.bot }, c2_err)
          | _ -> ({ r with avalue = abs_loc access_loc }, c2_err)
        else
          match access_loc with
          | Abs_Loc.Bot -> ({ r with avalue = Abs_Val.bot }, c2_err)
          | Abs_Loc.Top -> ({ r with avalue = Abs_Val.top }, c2_err)
          | Abs_Loc.AVarLoc _ | Abs_Loc.AHeapLoc _ ->
              let v_join, pp_join =
                Abs_Mem.fold
                  (fun (k : Abs_Loc.t) ((v, pps) : Abs_Val.t * PPSet.t)
                       (acc_v, acc_pps) ->
                    if loc_overlap access_loc k then
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
                app = PPSet.union r1.app r2.app;
              },
              c2 )
        | Lt ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.lt v1 v2); app = PPSet.union r1.app r2.app }, c2)
        | Gt ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.gt v1 v2); app = PPSet.union r1.app r2.app }, c2)
        | Ne ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.ne v1 v2); app = PPSet.union r1.app r2.app }, c2)
        | Le ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.le v1 v2); app = PPSet.union r1.app r2.app }, c2)
        | Ge ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.ge v1 v2); app = PPSet.union r1.app r2.app }, c2)
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
            ({ avalue = abs_int (Itv.and_ v1 v2); app = PPSet.union r1.app r2.app }, c2)
        | Or ->
            let v1 = proj_int r1.avalue in
            let v2 = proj_int r2.avalue in
            ({ avalue = abs_int (Itv.or_ v1 v2); app = PPSet.union r1.app r2.app }, c2))
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
        let c1_true  = { c1 with amem = refine_amem e1 true  c1.amem } in
        let c1_false = { c1 with amem = refine_amem e1 false c1.amem } in
        if v1 = Itv.Bool.true_ then
          let r2, c2 = self c1_true e2 in
          (r2, c2)
        else if v1 = Itv.Bool.false_ then
          let r3, c3 = self c1_false e3 in
          (r3, c3)
        else
          let r2, c2 = self c1_true e2 in
          let r3, c3 = self c1_false e3 in
          join_out (r2, c2) (r3, c3)
    | While (_id, econd, ebody) ->
        let rec iterate (i : int) (input : abs_conf) : abs_conf =
          let rcond, ccond = self input econd in
          let cond_itv = proj_int rcond.avalue in
          if cond_itv = Itv.Bool.false_ then
            { ccond with amem = refine_amem econd false ccond.amem }
          else begin
            let ccond_true = { ccond with amem = refine_amem econd true ccond.amem } in
            let _rbody, cbody = self ccond_true ebody in
            let next =
              if cond_itv = Itv.Bool.top then join_conf ccond cbody else cbody
            in
            let wconf = if i < widen_cnt then next else widen_conf input next in
            if leq_conf_noasem wconf input then
              { ccond with amem = refine_amem econd false ccond.amem }
            else iterate (i + 1) wconf
          end
        in
        let output = iterate 0 c in
        ({ avalue = abs_unit (); app = PPSet.empty }, output)
  in
  match c.aimode with
  | Disabled ->
      record_sem pp c_after_eval;
      (res, c_after_eval)
  | Enabled ->
      (* Apply handler effects only at yield points unless selective application
         is disabled for performance/precision experiments. *)
      let is_yield_point =
        match exp with Assign _ | Malloc _ | Var _ | Deref _ -> true | _ -> false
      in
      if (not !use_selective_handler_application) || is_yield_point then begin
        let c_after_post = !post_steps_fn c_after_eval in
        record_sem pp c_after_post;
        (res, c_after_post)
      end else begin
        record_sem pp c_after_eval;
        (res, c_after_eval)
      end

let rec evalA ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) :
    abs_res * abs_conf =
  evA evalA ~lvalue c lbl_exp

(* Evaluate without triggering handler post-step (forces aimode = Disabled). *)
let rec eval_no_post ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) :
    abs_res * abs_conf =
  evA eval_no_post ~lvalue { c with aimode = Interrupt.Disabled } lbl_exp

(* Apply a single compiled assignment atom without triggering post-step. *)
let apply_atom (atom : summary_atom) (c : abs_conf) : abs_conf =
  let c_guarded = { c with amem = refine_amem_by_guards atom.guards c.amem } in
  let r1, c1 = eval_no_post ~lvalue:true c_guarded atom.lhs in
  let r2, c2 = eval_no_post c1 atom.rhs in
  let l = proj_loc r1.avalue in
  let pp = ProgramPoint.Label atom.assign_lbl in
  match l with
  | Abs_Loc.Bot -> c2
  | _ ->
      let amem' = Abs_Mem.write c2.amem l r2.avalue pp in
      { c2 with amem = amem' }

(* Apply handler summary (aimode must be Disabled).
   Compiled: apply atoms in sequence; Fallback: evaluate body with eval_no_post.
   aenv is saved, reset to aenv0 for the handler's local scope, then restored. *)
let apply_handler_summary (summary : handler_summary) (c : abs_conf) : abs_conf =
  let saved_env = !aenv in
  aenv := !aenv0;
  in_handler_exec := true;
  let result =
    match summary with
    | Compiled atoms ->
        List.fold_left (fun acc_c atom -> apply_atom atom acc_c) c atoms
    | Fallback body ->
        let _r, c' = eval_no_post c body in
        c'
  in
  in_handler_exec := false;
  aenv := saved_env;
  result

(* One step of the handler fixpoint using precompiled summaries. *)
let post_step_summary (c : abs_conf) : abs_conf =
  let input_clean = { c with aimode = Interrupt.Disabled } in
  let saved_asem = !asem in
  asem := Abs_Sem.bot;
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
  let handler_asem = !asem in
  asem := Abs_Sem.join saved_asem handler_asem;
  { joined with aimode = c.aimode }

(* Iterate post_step_summary to a fixpoint with widening. *)
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

(* Fixpoint classification helpers *)

(* All-uppercase variable names are treated as compile-time integer constants. *)
let is_const_name (s : string) : bool =
  String.length s > 0
  && String.for_all
       (fun c -> (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')
       s

(* Map all-uppercase singleton-valued scalars in init_amem to their integer value. *)
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


(* Apply a pre-compiled scalar fixpoint effect to an abstract value *)
let apply_fp_scalar (fps : fp_scalar) (((itv, u, l) as v) : Abs_Val.t) :
    Abs_Val.t =
  match fps with
  | FPS_Unchanged -> v
  | FPS_Inc ->
      let new_itv =
        match itv with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (lo, _) -> Itv.Itv (lo, Itv.Bound.P_inf)
      in
      (new_itv, u, l)
  | FPS_Dec ->
      let new_itv =
        match itv with
        | Itv.Bot -> Itv.Bot
        | Itv.Itv (_, hi) -> Itv.Itv (Itv.Bound.N_inf, hi)
      in
      (new_itv, u, l)
  | FPS_JoinConsts c -> Abs_Val.join v (abs_int c)
  | FPS_JoinVal v' -> Abs_Val.join v v'
  | FPS_Top -> (Itv.top, u, l)

let apply_scalar_effects (effects : (fp_scalar * PPSet.t) list)
    (v : Abs_Val.t) (existing_pps : PPSet.t) : Abs_Val.t * PPSet.t =
  let all_pps =
    List.fold_left (fun acc (_, pps) -> PPSet.union acc pps) existing_pps effects
  in
  let has_top = List.exists (fun (fps, _) -> fps = FPS_Top) effects in
  let has_inc = List.exists (fun (fps, _) -> fps = FPS_Inc) effects in
  let has_dec = List.exists (fun (fps, _) -> fps = FPS_Dec) effects in
  if has_top || (has_inc && has_dec) then
    (Abs_Val.top, all_pps)
  else
    let v' = List.fold_left (fun acc (fps, _) -> apply_fp_scalar fps acc) v effects in
    (v', all_pps)

(* Map handler-local pointer variables to heap labels, handling simple aliases. *)
let build_local_ptr_map (atoms : summary_atom list) (init_amem : Abs_Mem.t) :
    (string, Exp.Lbl.t) Hashtbl.t =
  let tbl : (string, Exp.Lbl.t) Hashtbl.t = Hashtbl.create 4 in
  let get_loc0 name =
    match Abs_Env.find !aenv0 name with
    | Some l -> l
    | None -> Abs_Loc.get (name ^ "#")
  in
  let resolve_ptr_lbl name =
    let loc = get_loc0 name in
    match Abs_Mem.LocMap.find_opt loc init_amem with
    | Some ((_, _, Abs_Loc.AHeapLoc { lbl; _ }), _) -> Some lbl
    | _ -> Hashtbl.find_opt tbl name
  in
  List.iter
    (fun (atom : summary_atom) ->
      match atom.lhs.exp with
      | Var x -> (
          match atom.rhs.exp with
          | Var y -> (
              match resolve_ptr_lbl y with
              | Some lbl -> Hashtbl.replace tbl x lbl
              | None -> ())
          | _ -> ())
      | _ -> ())
    atoms;
  tbl

(* Classify one atom as a scalar or array-write fixpoint effect. *)
let classify_atom (atom : summary_atom) (init_amem : Abs_Mem.t)
    (const_map : (string, int) Hashtbl.t)
    (local_ptr_map : (string, Exp.Lbl.t) Hashtbl.t)
    (scalar_tbl : (Abs_Loc.t, (fp_scalar * PPSet.t) list) Hashtbl.t)
    (array_writes : fp_array_write list ref) : unit =
  let get_loc0 name =
    match Abs_Env.find !aenv0 name with
    | Some l -> l
    | None -> Abs_Loc.get (name ^ "#")
  in
  match atom.lhs.exp with
  | Var x ->
      let loc = get_loc0 x in
      let fps =
        match atom.rhs.exp with
        | Int n -> FPS_JoinConsts (Itv.alpha n)
        | AddrOf y -> FPS_JoinVal (abs_loc (get_loc0 y))
        | Bop (Plus, { exp = Var y; _ }, { exp = Int n; _ })
          when Abs_Loc.compare loc (get_loc0 y) = 0 ->
            if n > 0 then FPS_Inc else if n < 0 then FPS_Dec else FPS_Unchanged
        | Bop (Plus, { exp = Int n; _ }, { exp = Var y; _ })
          when Abs_Loc.compare loc (get_loc0 y) = 0 ->
            if n > 0 then FPS_Inc else if n < 0 then FPS_Dec else FPS_Unchanged
        | Bop (Minus, { exp = Var y; _ }, { exp = Int n; _ })
          when Abs_Loc.compare loc (get_loc0 y) = 0 ->
            if n > 0 then FPS_Dec else if n < 0 then FPS_Inc else FPS_Unchanged
        | Var y when is_const_name y ->
            (match Hashtbl.find_opt const_map y with
            | Some n -> FPS_JoinConsts (Itv.alpha n)
            | None -> FPS_Top)
        | _ -> FPS_Top
      in
      if fps <> FPS_Unchanged then begin
        let handler_pp = ProgramPoint.Label atom.assign_lbl in
        let old_effects =
          match Hashtbl.find_opt scalar_tbl loc with
          | None -> []
          | Some effects -> effects
        in
        Hashtbl.replace scalar_tbl loc ((fps, PPSet.singleton handler_pp) :: old_effects)
      end
  | Deref ({ exp = Var arr_name; _ }, idx_e) ->
      (* Resolve target heap block: check init_amem first, then local_ptr_map. *)
      let arr_var_loc = get_loc0 arr_name in
      let heap_lbl_opt =
        match Abs_Mem.LocMap.find_opt arr_var_loc init_amem with
        | Some ((_, _, Abs_Loc.AHeapLoc { lbl; _ }), _) -> Some lbl
        | _ -> Hashtbl.find_opt local_ptr_map arr_name
      in
      (match heap_lbl_opt with
      | Some lbl ->
          let fpa_idx =
            match idx_e.exp with
            | Int n -> `Const (Itv.alpha n)
            | Var idx_name -> `Var (get_loc0 idx_name)
            | _ -> `Const Itv.top
          in
          let fpa_rhs =
            match atom.rhs.exp with
            | Int n -> `Const (abs_int (Itv.alpha n))
            | AddrOf x -> `Const (abs_loc (get_loc0 x))
            | Var v -> `Var (get_loc0 v)
            | _ -> `Const Abs_Val.top
          in
          let fpa_at = ProgramPoint.Label atom.assign_lbl in
          array_writes :=
            { fpa_lbl = lbl; fpa_idx; fpa_rhs; fpa_at;
              fpa_offset_pp = PPSet.singleton fpa_at; fpa_guards = atom.guards;
              fpa_base_var = arr_var_loc }
            :: !array_writes
      | None -> ())
  | _ -> ()

(* Build compiled_fixpoint from all Compiled handler summaries. *)
let compile_fixpoint (init_amem : Abs_Mem.t) : unit =
  let const_map = build_const_map init_amem in
  let scalar_tbl : (Abs_Loc.t, (fp_scalar * PPSet.t) list) Hashtbl.t = Hashtbl.create 16 in
  let array_writes : fp_array_write list ref = ref [] in
  HandlerStore.IidMap.iter
    (fun _iid summary ->
      match summary with
      | Compiled atoms ->
          let local_ptr_map = build_local_ptr_map atoms init_amem in
          List.iter
            (fun atom ->
              classify_atom atom init_amem const_map local_ptr_map scalar_tbl array_writes)
            atoms
      | Fallback _ -> ())
    !handler_summaries;
  let fp_scalars =
    Hashtbl.fold (fun loc effects acc -> (loc, effects) :: acc) scalar_tbl []
  in
  compiled_fp := { fp_scalars; fp_arrays = !array_writes };
  let has_fallback =
    HandlerStore.IidMap.exists
      (fun _iid s -> match s with Fallback _ -> true | _ -> false)
      !handler_summaries
  in
  use_compiled_fp := not has_fallback

(* Apply pre-compiled fixpoint: scalar pass, then array-write pass, then synthesize asem snapshots. *)
let apply_compiled_fixpoint (fp : compiled_fixpoint) (c : abs_conf) : abs_conf =
  (* Pass 1: scalar effects *)
  let amem1 =
    List.fold_left
      (fun amem (loc, effects) ->
        let active = List.filter (fun (fps, _) -> fps <> FPS_Unchanged) effects in
        if active = [] then amem
        else
          match Abs_Mem.LocMap.find_opt loc amem with
          | None -> amem
          | Some (v, existing_pps) ->
              let v', new_pps = apply_scalar_effects active v existing_pps in
              Abs_Mem.LocMap.add loc (v', new_pps) amem)
      c.amem fp.fp_scalars
  in
  (* Pass 2: array write effects *)
  let amem2 =
    List.fold_left
      (fun (amem, errs) (fpa : fp_array_write) ->
        let amem_for_idx = refine_amem_by_guards fpa.fpa_guards amem in
        let offset_itv =
          match fpa.fpa_idx with
          | `Const itv -> itv
          | `Var idx_loc ->
              (match Abs_Mem.LocMap.find_opt idx_loc amem_for_idx with
              | Some ((itv, _, _), _) -> itv
              | None -> Itv.bot)
        in
        let write_loc =
          Abs_Loc.AHeapLoc { lbl = fpa.fpa_lbl; offset = offset_itv }
        in
        let in_itv, left_oob, right_oob = itv_overlap write_loc in
        (* Report the original target, but write only the safe in-bounds part. *)
        let in_bounds_write_loc =
          Abs_Loc.AHeapLoc { lbl = fpa.fpa_lbl; offset = in_itv }
        in
        let base_pp =
          match Abs_Mem.LocMap.find_opt fpa.fpa_base_var amem with
          | Some (_, pps) -> pps
          | None -> PPSet.empty
        in
        let errs' =
          if left_oob = Itv.bot && right_oob = Itv.bot then errs
          else
            let err =
              Error.make ~at:fpa.fpa_at ~access:Error.Write ~base:write_loc
                ~in_itv ~left_oob ~right_oob ~base_pp
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
          else Abs_Mem.weak_write amem in_bounds_write_loc rhs_val fpa.fpa_at
        in
        (amem', errs'))
      (amem1, !errs) fp.fp_arrays
    |> fun (amem, errs') ->
    errs := errs';
    amem
  in
  (* Pass 3: synthesize asem snapshots for handler scalar assignments (for provenance). *)
  let asem3 =
    List.fold_left
      (fun asem (loc, effects) ->
        List.fold_left
          (fun asem' (fps, handler_pps) ->
            match fps with
            | FPS_Unchanged -> asem'
            | _ ->
                let contributed_itv =
                  match fps with
                  | FPS_JoinConsts c -> c
                  | FPS_JoinVal (itv, _, _) -> itv
                  | _ -> Itv.top
                in
                let contributed_val =
                  match fps with
                  | FPS_JoinVal v -> v
                  | _ -> (contributed_itv, Abs_Unit.bot, Abs_Loc.bot)
                in
                PPSet.fold
                  (fun pp asem'' ->
                    let snapshot = Abs_Mem.write Abs_Mem.bot loc contributed_val pp in
                    Abs_Sem.weak_write asem'' pp snapshot)
                  handler_pps asem')
          asem effects)
      !asem fp.fp_scalars
  in
  (* Pass 4: synthesize asem snapshots for handler array writes (for provenance). *)
  let scalar_pps_map =
    List.fold_left
      (fun m (loc, effects) ->
        let all_pps =
          List.fold_left (fun acc (_, pps) -> PPSet.union acc pps) PPSet.empty effects
        in
        if PPSet.is_empty all_pps then m
        else Abs_Mem.LocMap.add loc all_pps m)
      Abs_Mem.LocMap.empty fp.fp_scalars
  in
  let asem4 =
    List.fold_left
      (fun asem (fpa : fp_array_write) ->
        let rhs_snapshot =
          match fpa.fpa_rhs with
          | `Const _ -> Abs_Mem.bot
          | `Var val_loc -> (
              match Abs_Mem.LocMap.find_opt val_loc amem2 with
              | None -> Abs_Mem.bot
              | Some (v, existing_pps) ->
                  PPSet.fold
                    (fun pp snap -> Abs_Mem.weak_write snap val_loc v pp)
                    existing_pps Abs_Mem.bot)
        in
        (* Add base pointer variable to snapshot if assigned inside the handler. *)
        let base_assign_pps =
          match Abs_Mem.LocMap.find_opt fpa.fpa_base_var scalar_pps_map with
          | Some pps -> pps
          | None -> PPSet.empty
        in
        let base_ptr_val_opt =
          if PPSet.is_empty base_assign_pps then None
          else
            Some (abs_loc (Abs_Loc.AHeapLoc { lbl = fpa.fpa_lbl; offset = Itv.alpha 0 }))
        in
        let enriched_snapshot =
          match base_ptr_val_opt with
          | None -> rhs_snapshot
          | Some bpv ->
              Abs_Mem.LocMap.add fpa.fpa_base_var (bpv, base_assign_pps) rhs_snapshot
        in
        let asem' =
          if enriched_snapshot = Abs_Mem.bot then asem
          else
            PPSet.fold
              (fun pp asem_acc -> Abs_Sem.weak_write asem_acc pp enriched_snapshot)
              fpa.fpa_offset_pp asem
        in
        (* Synthesize snapshot at base-pointer assignment PP for trace. *)
        match base_ptr_val_opt with
        | None -> asem'
        | Some bpv ->
            let base_snapshot =
              Abs_Mem.LocMap.add fpa.fpa_base_var (bpv, PPSet.empty) Abs_Mem.bot
            in
            PPSet.fold
              (fun pp asem_acc -> Abs_Sem.weak_write asem_acc pp base_snapshot)
              base_assign_pps asem')
      asem3 fp.fp_arrays
  in
  asem := asem4;
  { c with amem = amem2 }

(* Use compiled fixpoint if all handlers compiled; otherwise fall back to iterative. *)
let apply_fixpoint_to_conf (c : abs_conf) : abs_conf =
  if !use_compiled_fp then begin
    let saved_env = !aenv in
    aenv := !aenv0;
    let result = apply_compiled_fixpoint !compiled_fp c in
    aenv := saved_env;
    result
  end
  else post_steps_summary c

(* Wire up forward reference. *)
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

let is_comparison_bop (bop : Exp.bop) =
  match bop with
  | Eq | Ne | Lt | Le | Gt | Ge -> true
  | _ -> false

(* Extract branch guards for simple "Var cmp rhs" conditions:
   returns (then_guard, else_guard). *)
let extract_branch_guards (cond_e : Exp.lbl_t)
    : (Exp.bop * Exp.lbl_t) option * (Exp.bop * Exp.lbl_t) option =
  match cond_e.exp with
  | Bop (bop, { exp = Var _; _ }, _) when is_comparison_bop bop ->
      (Some (bop, cond_e), Some (negate_bop bop, cond_e))
  | _ -> (None, None)

let attach_guard (g : (Exp.bop * Exp.lbl_t) option) (s : handler_summary)
    : handler_summary =
  match s with
  | Compiled atoms ->
      Compiled
        (List.map
           (fun a ->
             match g with None -> a | Some guard -> { a with guards = guard :: a.guards })
           atoms)
  | Fallback _ -> s

(* Compile handler body to a summary: assigns → atoms, if-branches flattened, loops/malloc → Fallback. *)
let rec compile_handler (lbl_exp : Exp.lbl_t) : handler_summary =
  match lbl_exp.exp with
  | Assign (lhs, rhs) ->
      let assign_lbl = lbl_exp.lbl in
      (match lhs.exp with
      | Var _ | Deref _ -> Compiled [ { lhs; rhs; assign_lbl; guards = [] } ]
      | _ -> Fallback lbl_exp)
  | Seq (e1, e2) -> (
      match (compile_handler e1, compile_handler e2) with
      | Compiled a1, Compiled a2 -> Compiled (a1 @ a2)
      | _ -> Fallback lbl_exp)
  | If (cond_e, e_then, e_else) ->
      let then_guard, else_guard = extract_branch_guards cond_e in
      let then_summary = attach_guard then_guard (compile_handler e_then) in
      let else_summary = attach_guard else_guard (compile_handler e_else) in
      (match (then_summary, else_summary) with
      | Compiled a1, Compiled a2 -> Compiled (a1 @ a2)
      | _ -> Fallback lbl_exp)
  | Enable | Disable | Unit | Int _ | Var _ | AddrOf _ | Bop _ | Deref _ ->
      Compiled []
  | While _ | Malloc _ ->
      Fallback lbl_exp

let init_confa (pgm : Program.t) : abs_conf =
  reset_outputs ();
  let c0 = { amem = Abs_Mem.bot; aimode = Interrupt.Disabled } in
  let _, c_globals = evalA c0 pgm.global in
  aenv0 := !aenv;
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
    amem = c_globals.amem;
    aimode = Interrupt.Enabled;
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
  let _, _c_final = evalA c_init pgm.main in
  filter_main_sem !asem

let abs_analyze ?(use_compile_opt = true) ?(use_selective_opt = true)
    ?use_opt (pgm : Program.t) : Abs_Sem.t * ErrorSet.t =
  let c_init = init_confa pgm in
  let use_compile_opt, use_selective_opt =
    match use_opt with
    | Some legacy_use_opt -> (legacy_use_opt, legacy_use_opt)
    | None -> (use_compile_opt, use_selective_opt)
  in
  if not use_compile_opt then use_compiled_fp := false;
  use_selective_handler_application := use_selective_opt;
  let _, _c_final = evalA c_init pgm.main in
  (!asem, !errs)
