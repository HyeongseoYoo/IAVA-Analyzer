open Syntax
open Domain

type conf = {
  env : Env.t;
  mem : Mem.t;
  imode : Interrupt.t;
  iset : IidSet.t;
  handlers : HandlerStore.t;
}

type result = { value : Value.t; pp : ProgramPoint.t; out : Outcome.t }

exception Runtime_error of string

let rec eval ?(lvalue = false) (c : conf) (lbl_exp : Exp.lbl_t) : result * conf
    =
  let ({ lbl; exp } : Exp.lbl_t) = lbl_exp in
  let ({ env; mem; imode; iset; handlers } : conf) = c in
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
    let (exp_h, env0) = HandlerStore.lookup exp_c.handlers iid in
    let (hdl_r, hdl_c) = eval {exp_c with env = env0; imode = Interrupt.Disabled} exp_h in
    (* TODO: Done -> Non-Deterministic *)
    ({exp_r with out = Outcome.Done}, { exp_c with mem = hdl_c.mem; } ) 
  )

let init_conf (pgm : Program.t) : conf =
  let c0 =
    {
      env = Env.empty;
      mem = Mem.empty;
      imode = Interrupt.Enabled;
      iset = IidSet.empty;
      handlers = HandlerStore.empty;
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
  {
    env = c_globals.env;
    mem = c_globals.mem;
    imode = c_globals.imode;
    iset = iset';
    handlers = hs';
  }

let def_intp (pgm : Program.t) : Mem.t =
  let c_init = init_conf pgm in
  print_endline "=== Initial Memory ===";
  print_endline (Mem.string_of_t c_init.mem);
  print_endline "=== Final Memory ===";
  let _, c_final = eval c_init pgm.main in
  c_final.mem
