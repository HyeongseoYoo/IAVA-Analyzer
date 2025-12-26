open Syntax
open Domain

type conf = {
  env : Env.t;
  mem : Mem.t;
  imode : Interrupt.t;
  iset : IidSet.t;
  handlers : HandlerStore.t;
  fresh_loc : Loc.t;
}

type result = { value : Value.t; pp : ProgramPoint.t; out : Outcome.t }

exception Runtime_error of string

let rec eval ?(lvalue = false) (c : conf) (lbl_exp : Exp.lbl_t) : result * conf
    =
  let ({ lbl; exp } : Exp.lbl_t) = lbl_exp in
  let ({ env; mem; imode; iset; handlers; fresh_loc } : conf) = c in
  let r = { value = Value.Unit; pp = Unit; out = Outcome.Done } in
  match exp with
  | Unit -> (r, c)
  | Int n -> ({ r with value = Value.Int n }, c)
  | Var x -> (
      if lvalue then
        match Var.Map.find_opt x env with
        | Some l -> ({ r with value = Value.Loc l }, c)
        | None ->
            ( { r with value = Value.Loc fresh_loc },
              {
                c with
                env = Var.Map.add x fresh_loc env;
                fresh_loc = fresh_loc + 1;
              } )
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
      let mem'' =
        List.init n (fun i -> c2.fresh_loc + i)
        |> List.fold_left (fun m a -> Loc.Map.add a new_v m) mem'
      in
      ( { r with value = Value.Loc c2.fresh_loc },
        { c with mem = mem''; fresh_loc = c2.fresh_loc + n } )
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
      let l = base + offset in
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
      Printf.printf "[Assign] %s := <%s, %s>\n" (Loc.string_of_t l)
        (Value.string_of_t r2.value)
        (ProgramPoint.string_of_t (ProgramPoint.Label lbl));
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
      let l = c1.fresh_loc in
      let env' = Var.Map.add x l c1.env in
      let mem' = Loc.Map.add l (r1.value, r1.pp) c1.mem in
      let c2 =
        { c1 with env = env'; mem = mem'; fresh_loc = c1.fresh_loc + 1 }
      in
      let r3, c3 = eval c2 e2 in
      (r3, { c3 with env = c1.env })

let init_conf (pgm : Program.t) : conf =
  let c0 =
    {
      env = Env.empty;
      mem = Mem.empty;
      imode = Interrupt.Enabled;
      iset = IidSet.empty;
      handlers = HandlerStore.empty;
      fresh_loc = Loc.init;
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
    fresh_loc = c_globals.fresh_loc;
  }

let def_intp (pgm : Program.t) : Mem.t =
  let c_init = init_conf pgm in
  print_endline "=== Initial Memory ===";
  print_endline (Mem.string_of_t c_init.mem);
  print_endline "=== Final Memory ===";
  let _, c_final = eval c_init pgm.main in
  c_final.mem
