open Syntax
open Domain
open Abs_dom

type conf = {
  env : Env.t;
  mem : Mem.t;
  imode : Interrupt.t;
}

type abs_conf = {
  aenv : Abs_Env.t;
  amem : Abs_Mem.t;
  aimode : Interrupt.t;
}

type result = {
  value : Value.t;
  pp : ProgramPoint.t;
  out : Outcome.t
  }

type abs_res = {
  avalue : Abs_Val.t;
  app : PPSet.t;
}

exception Runtime_error of string

let iset : IidSet.t ref = ref IidSet.empty
let handlers : HandlerStore.t ref = ref HandlerStore.empty
let abs_handlers : Abs_HandlerStore.t ref = ref Abs_HandlerStore.bot

let rec eval ?(lvalue = false) (c : conf) (lbl_exp : Exp.lbl_t) : result * conf
    =
  let ({ lbl; exp } : Exp.lbl_t) = lbl_exp in
  let ({ env; mem; imode } : conf) = c in
  (* TODO: Done -> Non-Deterministic *)
  let r = { value = Value.Unit; pp = Unit; out = Outcome.Done } in 
  (* TEST CODE *)
  (* let r = (if lbl = Exp.Lbl.Main 3 then { value = Value.Unit; pp = Unit; out = Outcome.I 0 } else { value = Value.Unit; pp = Unit; out = Outcome.Done }) in  *)
  let (exp_r, exp_c) = 
  (match exp with
  | Unit -> (r, c)
  | Int n -> ({ r with value = Value.Int n }, c)
  | Var x -> (
      if lvalue then
        match Var.Map.find_opt x env with
        | Some l -> ({ r with value = Value.Loc l }, c)
        | None ->
            let l = Loc.of_pp (ProgramPoint.Label lbl) in
            ( { r with value = Value.Loc l },
              { c with env = Var.Map.add x l env } )
      else
        match Var.Map.find_opt x env with
        | Some l -> (
            match Loc.Map.find_opt l mem with
            | Some (v, p) -> ({ r with value = v; pp = p }, c)
            | None ->
                raise
                  (Runtime_error
                     ("[Mem] Location " ^ Loc.string_of_t l ^ " not found")))
        | None -> raise (Runtime_error ("[Env] Variable " ^ x ^ " not found")))
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
      let mem' = c2.mem in
      let new_v = (v, ProgramPoint.Label lbl) in
      let base_pp = ProgramPoint.Label lbl in
      let mem'' =
        List.init n (fun i -> Loc.of_pp ~index:i base_pp)
        |> List.fold_left (fun m a -> Loc.Map.add a new_v m) mem'
      in
      ( { r with value = Value.Loc (Loc.of_pp base_pp) },
        { c with mem = mem'' } )
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
      let base_pp, base_idx = base in
      let l = (base_pp, base_idx + offset) in
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
      | Plus -> (
          match (r1.value, r2.value) with
          | Value.Int i1, Value.Int i2 ->
              let res = i1 + i2 in
              ({ r with value = Value.Int res }, c2)
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
  | Let (x, e1, e2) ->
      let r1, c1 = eval c e1 in
      let l = Loc.of_pp (ProgramPoint.Label lbl) in
      let env' = Var.Map.add x l c1.env in
      let mem' = Loc.Map.add l (r1.value, r1.pp) c1.mem in
      let c2 = { c1 with env = env'; mem = mem' } in
      let r3, c3 = eval c2 e2 in
      (r3, { c3 with env = c1.env })) in
  match exp_r.out with
  | Outcome.Done -> (exp_r, exp_c)
  | Outcome.I iid -> (
    let (exp_h, env0) = HandlerStore.lookup !handlers iid in
    let (hdl_r, hdl_c) = eval {exp_c with env = env0; imode = Interrupt.Disabled} exp_h in
    (* TODO: Done -> Non-Deterministic *)
    ({exp_r with out = Outcome.Done}, { exp_c with mem = hdl_c.mem; } ) 
  )

(* Helper *)
let join_res r1 r2 = {
  avalue = Abs_Val.join r1.avalue r2.avalue;
  app = PPSet.union r1.app r2.app;
}

let join_conf c1 c2 = {
  aenv = Abs_Env.join c1.aenv c2.aenv;
  amem = Abs_Mem.join c1.amem c2.amem;
  aimode = (match (c1.aimode, c2.aimode) with
    | Interrupt.Enabled, _ | _, Interrupt.Enabled -> Interrupt.Enabled
    | _ -> Interrupt.Disabled);
}

let join_out (r1, c1) (r2, c2) =
  (join_res r1 r2, join_conf c1 c2)

let widen_conf c1 c2 = {
  aenv = Abs_Env.widen c1.aenv c2.aenv;
  amem = Abs_Mem.widen c1.amem c2.amem;
  aimode =
    (match (c1.aimode, c2.aimode) with
     | Interrupt.Enabled, _ | _, Interrupt.Enabled -> Interrupt.Enabled
     | _ -> Interrupt.Disabled);
}

let leq_conf c1 c2 =
  Abs_Env.leq c1.aenv c2.aenv && Abs_Mem.leq c1.amem c2.amem
  && (match (c1.aimode, c2.aimode) with
      | Interrupt.Disabled, Interrupt.Enabled -> true
      | Interrupt.Disabled, Interrupt.Disabled -> true
      | Interrupt.Enabled, Interrupt.Enabled -> true
      | Interrupt.Enabled, Interrupt.Disabled -> false)

let interrupt_transform (lbl:Exp.lbl_t) (c: abs_conf) : abs_conf =
  let ({ aenv; amem; aimode } : abs_conf) = c in
  match aimode with
  | _ -> c (* TODO: Implement interrupt_transform *)


let post_step (lbl: Exp.lbl_t) (c: abs_conf) : abs_conf =
  interrupt_transform lbl c

let abs_unit () : Abs_Val.t =
  (Itv.bot, Abs_Unit.Unit, Abs_Loc.bot)

let abs_int (itv:Itv.t) : Abs_Val.t =
  (itv, Abs_Unit.bot, Abs_Loc.bot)

let abs_loc (l:Abs_Loc.t) : Abs_Val.t =
  (Itv.bot, Abs_Unit.bot, l)

let proj_int (v:Abs_Val.t) : Itv.t =
  let (i, _u, _l) = v in i

let proj_loc (v:Abs_Val.t) : Abs_Loc.t =
  let (_i, _u, l) = v in l

let get_offset (itv:Itv.t) : Itv.t =
  match itv with
  | Bot -> Bot
  | Itv (_, P_inf) | Itv (N_inf, _) -> Itv.bot
  | Itv (Z l, Z r) -> (if l <= 0 then Itv.bot else Itv (Z 0, Z (r-1)))
  | _ -> Itv.bot
let offset_checked ~(who:string) (base:Abs_Loc.t) (off:Itv.t) : Abs_Loc.t =
  PPMap.mapi
    (fun pp valid ->
      let access = Itv.add valid off in
      if Itv.leq access valid then access
      else
        raise (Runtime_error
          (Printf.sprintf
             "[%s] Deref out-of-bounds: pp=%s, valid=%s, off=%s, access=%s"
             who
             (ProgramPoint.string_of_t pp)
             (Itv.string_of_t valid)
             (Itv.string_of_t off)
             (Itv.string_of_t access))))
    base
let equal_check (v1:Abs_Val.t) (v2:Abs_Val.t) : Itv.t =
  let (itv1, _u1, loc1) = v1 in
  let (itv2, _u2, loc2) = v2 in
  let itv_check = (itv1 = Itv.bot || itv2 = Itv.bot) in
  let loc_check = (loc1 = Abs_Loc.bot || loc2 = Abs_Loc.bot) in
  if itv_check && loc_check then Itv.Bool.false_
  else (
    (*TO-DO*)
    Itv.Bool.top
  )

let evA (self: ?lvalue : bool -> abs_conf -> Exp.lbl_t -> abs_res * abs_conf) ?(lvalue = false) (c : abs_conf) (lbl_exp : Exp.lbl_t) : abs_res * abs_conf =
  let ({ lbl; exp } : Exp.lbl_t) = lbl_exp in
  let ({ aenv; amem; aimode } : abs_conf) = c in
  let r = { avalue = Abs_Val.bot; app = PPSet.empty } in
  let pp = ProgramPoint.Label lbl in
  (* TO-DO : after evaluating internal e, check interrupt mode and do post-step*)
  match exp with
  | Unit -> ({ r with avalue = abs_unit () }, c)
  | Int n -> ({ r with avalue = abs_int (Itv.alpha n) }, c)
  | Var x -> (
      if lvalue then
        match Abs_Env.find aenv x with
        | Some l -> ({ r with avalue = abs_loc l}, c)
        | None ->
            let l = Abs_Loc.alloc (ProgramPoint.Label lbl) in
            let aenv' = Abs_Env.write aenv x l in
            ( { r with avalue = abs_loc l},
              { c with aenv = aenv' } )
      else
        match Abs_Env.find aenv x with
        | Some l -> 
            let (v, p') = Abs_Mem.find amem l in
            ({ avalue = v; app = p'; }, c)
        | None -> raise (Runtime_error ("[Abs_Env] Variable " ^ x ^ " not found")))
  | Enable -> ({r with avalue = abs_unit ()}, { c with aimode = Interrupt.Enabled })
  | Disable -> ({r with avalue = abs_unit ()}, { c with aimode = Interrupt.Disabled })
  | Malloc (e1, e2) ->
      let r1, c1 = self c e1 in
      (* TO-DO : post-step or not *)
      (* let c1 = post_step e1 c1 in *)
      let r2, c2 = self c1 e2 in
      (* let c2 = post_step e2 c2 in *)
      let n_itv = get_offset (proj_int r1.avalue) in
      let v = r2.avalue in
      (match n_itv with
      | Bot -> raise (Runtime_error ("[Malloc] Number of allocation cannot be zero"))
      | Itv _ -> (
        let base_pp = ProgramPoint.Label lbl in
        let amem' = c2.amem in
        let base_loc = Abs_Loc.create base_pp n_itv in
        let amem'' =  Abs_Mem.write amem' base_loc v pp in
        ({ r with avalue = abs_loc base_loc }, { c2 with amem = amem'' } )
      ))
  | Deref (e1, e2) -> (
    let r1, c1 = self c e1 in
      (* TO-DO : post-step or not *)
    let r2, c2 = self c1 e2 in
      (* let c2 = post_step e2 c2 in *)
    let base = proj_loc r1.avalue in
    let off = proj_int r2.avalue in
    let aloc = offset_checked ~who:"Deref" base off in
    (* TO-DO: base에서 aloc 쪼개서 amem 변형하기 *)
    if lvalue then ({ r with avalue = abs_loc aloc }, c2 )
    else (
      let (v, pps) = Abs_Mem.find c2.amem aloc in
      ({ avalue = v; app = pps }, c2))
  )
  | Bop (bop, e1, e2) -> (
      let r1, c1 = self c e1 in
      let r2, c2 = self c1 e2 in
      match bop with
      | Eq -> ({ avalue = abs_int (equal_check r1.avalue r2.avalue); app = PPSet.empty }, c2)
      | Plus -> (
          let v1 = proj_int r1.avalue in
          let v2 = proj_int r2.avalue in
          let res = Itv.add v1 v2 in
          ({ avalue = abs_int res; app = PPSet.union r1.app r2.app }, c2)
      )
  )
  | Assign (e1, e2) ->
      let r1, c1 = self ~lvalue:true c e1 in
      let r2, c2 = self c1 e2 in
      let l = proj_loc r1.avalue in
      let amem' = Abs_Mem.write c2.amem l r2.avalue pp in
      ( { avalue = abs_unit (); app = PPSet.empty },
        { c2 with amem = amem' } )
  | Seq (e1, e2) ->
      let _, c1 = self c e1 in
      self c1 e2
  | If (e1, e2, e3) -> (
      let r1, c1 = self c e1 in
      let v1 = proj_int r1.avalue in
      if v1 = Itv.Bool.true_ then
        let r2, c2 = self c1 e2 in
        (r2, c2)
      else if v1 = Itv.Bool.false_ then
        let r3, c3 = self c1 e3 in
        (r3, c3)
      else
        let r2, c2 = self c1 e2 in
        let r3, c3 = self c1 e3 in
        join_out (r2, c2) (r3, c3))
  | Let (x, e1, e2) ->
      let r1, c1 = self c e1 in
      let l = Abs_Loc.alloc (ProgramPoint.Label lbl) in
      let aenv' = Abs_Env.write c1.aenv x l in
      let amem' = Abs_Mem.write c1.amem l r1.avalue pp in
      let c2 = { c1 with aenv = aenv'; amem = amem' } in
      let r2, c3 = self c2 e2 in
      (r2, { c3 with aenv = c1.aenv })
  | While (_id, econd, ebody) ->
      (* join-only widening fixpoint for while *)
      let rec iterate (i:int) (input:abs_conf) : abs_conf =
        let rcond, ccond = self input econd in
        let cond_itv = proj_int rcond.avalue in
        if cond_itv = Itv.Bool.false_ then input
        else begin
          let _rbody, cbody = self ccond ebody in
          let joined = join_conf input cbody in
          (* widen condition *)
          let next = if i = 0 then joined else widen_conf input joined in
          if leq_conf next input then input
          else iterate (i+1) next
        end
      in
      let output = iterate 0 c in
      ({ avalue = abs_unit (); app = PPSet.empty }, output)

let rec evalA ?(lvalue=false) (c:abs_conf) (lbl_exp:Exp.lbl_t) : abs_res * abs_conf =
  evA evalA ~lvalue c lbl_exp

let init_conf (pgm : Program.t) : conf =
  let c0 =
    {
      env = Env.empty;
      mem = Mem.empty;
      imode = Interrupt.Enabled;
    }
  in
  let _, c_globals = eval c0 pgm.global in
  let hs', iset' =
    List.fold_left
      (fun (hs, iset) (d : Handler.t) ->
        let hs =
          HandlerStore.add hs ~iid:d.iid ~body:d.body ~env:c_globals.env
        in
        let iset = IidSet.add (Handler.get_iid d) iset in
        (hs, iset))
      (HandlerStore.empty, IidSet.empty)
      pgm.handler
  in
  iset := iset';
  handlers := hs';
  {
    env = c_globals.env;
    mem = c_globals.mem;
    imode = c_globals.imode;
  }

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
      aenv = Abs_Env.bot;
      amem = Abs_Mem.bot;
      aimode = Interrupt.Enabled;
    }
  in
  let _, c_globals = evalA c0 pgm.global in
  let hs', iset' =
    List.fold_left
      (fun (hs, iset) (d : Handler.t) ->
        let hs =
          Abs_HandlerStore.add hs d.iid d.body c_globals.aenv
        in
        let iset = IidSet.add (Handler.get_iid d) iset in
        (hs, iset))
      (Abs_HandlerStore.bot, IidSet.empty)
      pgm.handler
  in
  iset := iset';
  abs_handlers := hs';
  {
    aenv = c_globals.aenv;
    amem = c_globals.amem;
    aimode = c_globals.aimode;
  }



let abs_def_intp (pgm : Program.t) : Abs_Mem.t =
  print_endline "<<<Abstract Interpretation>>>";
  let c_init = init_confa pgm in
  print_endline "=== Initial Abstract Memory ===";
  print_endline (Abs_Mem.string_of_t c_init.amem);
  print_endline "=== Final Abstract Memory ===";
  let _, c_final = evalA c_init pgm.main in
  c_final.amem