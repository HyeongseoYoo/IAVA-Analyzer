open Syntax
open Domain

type conf = {
  env : Env.t;
  mem : Mem.t;
  imode : Interrupt.t;
  iset : IidSet.t;
  handlers : HandlerStore.t;
}

type result = {
  value : Value.t;
  pp : ProgramPoint.t;
  out : Outcome.t;
}

exception Runtime_error of string


let rec eval ?(lvalue=false) (c: conf) (lbl_exp: Exp.lbl_t)  : (result * conf) =
  let ({lbl; exp}: Exp.lbl_t) = lbl_exp in
  let ({env; mem; imode; iset; handlers}: conf) = c in
  let r0 = {value = Value.Unit; pp = Unit; out = Outcome.Done} in
  match exp with
  (* | Unit -> (r0, c)
  | Int n -> ({r0 with value = Value.Int n}, c)
  | Var x -> (
    if lvalue then (match env.find_opt x with | Some a -> )
    else ()) *)
  | _ -> failwith "Not implemented yet"

let init_conf (pgm: Program.t) : conf =
  let c0 = {
    env = Env.empty;
    mem = Mem.empty;
    imode = Interrupt.Enabled;
    iset = IidSet.empty;
    handlers = HandlerStore.empty;
  } in
  let (_, c_globals) = eval c0 pgm.global in
  let (h', iset') =
    List.fold_left
      (fun (hs, iset) (d : Handler.t) ->
        let hs' =
          HandlerStore.add hs
            ~iid:d.iid
            ~body:d.body
            ~env:c_globals.env
        in
        let iset' = IidSet.add (Handler.get_iid d) iset in
        (hs', iset'))
      (HandlerStore.empty, IidSet.empty)
      pgm.handler
  in
  {
    env = c_globals.env;
    mem = c_globals.mem;
    imode = c_globals.imode;
    iset = iset';
    handlers = h';
  }

let def_intp (pgm: Program.t) : Mem.t =
  let c_init = init_conf pgm in
  print_endline "=== Initial Memory ===";
  print_endline (Mem.string_of_t c_init.mem);
  let (_, c_final) = eval c_init pgm.main in
  c_final.mem

