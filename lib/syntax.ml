(* * Interrupt-aware toy language (init/main) * Exp-only syntax (expressions include commands) *)
module Exp = struct
  type id = string
  type bop = Eq | Plus

  type t =
  | Unit 
  | Int of int 
  | Var of id 
  | Enable 
  | Disable 
  | Bop of bop * t * t 
  | Deref of t * t (* *e[e] *) 
  | Malloc of t * t (* malloc(e, e) *) 
  | Assign of t * t (* e := e *) 
  | Seq of t * t 
  | If of t * t * t 
  | While of t * t 
  | Let of id * t * t
  
  let string_of_bop : bop -> string = function
    | Plus -> "+" 
    | Eq -> "=" 
    
  let rec string_of_t : t -> string = function 
  | Unit -> "unit" 
  | Int n -> string_of_int n 
  | Var x -> x 
  | Enable -> "enable" 
  | Disable -> "disable" 
  | Bop (bop, e1, e2) -> Printf.sprintf "(%s %s %s)" (string_of_t e1) (string_of_bop bop) (string_of_t e2) 
  | Deref (e1, e2) -> Printf.sprintf "*%s[%s]" (string_of_t e1) (string_of_t e2) 
  | Malloc (e1, e2) -> Printf.sprintf "malloc(%s, %s)" (string_of_t e1) (string_of_t e2) 
  | Assign (e1, e2) -> Printf.sprintf "%s := %s" (string_of_t e1) (string_of_t e2) 
  | Seq (e1, e2) -> Printf.sprintf "%s; %s" (string_of_t e1) (string_of_t e2) 
  | If (e1, e2, e3) -> Printf.sprintf "if %s then %s else %s" (string_of_t e1) (string_of_t e2) (string_of_t e3) 
  | While (e1, e2) -> Printf.sprintf "while %s do %s" (string_of_t e1) (string_of_t e2) 
  | Let (x, e1, e2) -> Printf.sprintf "let %s = %s in %s" x (string_of_t e1) (string_of_t e2)
end

module Handler = struct
  type iid = int
  type def = { iid : iid; body : Exp.t }  (* handler i e *)

  let string_of_def d =
    Printf.sprintf "handler %d %s" d.iid (Exp.string_of_t d.body)
end

module Init = struct
  type t = {
    globals : Exp.t;
    handlers : Handler.def list;
  }

  let string_of_t (i : t) : string =
    let globals_s = Exp.string_of_t i.globals in
    match i.handlers with
    | [] -> globals_s
    | hs ->
        let hs_s = hs |> List.map Handler.string_of_def |> String.concat ";\n  " in
        Printf.sprintf "%s;\n  %s" globals_s hs_s
end

module Program = struct 
  type t = {
    init : Init.t;
    main : Exp.t;
  }

  let string_of_t (p : t) : string =
    Printf.sprintf "init {\n  %s\n}\n\nmain {\n  %s\n}\n"
      (Init.string_of_t p.init)
      (Exp.string_of_t p.main)
end

module Tabulate = struct
  module Label = struct
    type t =
      | Init of int                 
      | Main of int                 
      | Handler of Handler.iid * int

    let compare a b =
      match (a, b) with
      | Init x, Init y -> Int.compare x y
      | Init _, _ -> -1
      | _, Init _ -> 1
      | Main x, Main y -> Int.compare x y
      | Main _, _ -> -1
      | _, Main _ -> 1
      | Handler (i1, x1), Handler (i2, x2) ->
          let c = Int.compare i1 i2 in
          if c <> 0 then c else Int.compare x1 x2

    let string_of_t = function
      | Init n -> "[Init]" ^ (string_of_int n)
      | Main n ->"[Main]"^ (string_of_int n)
      | Handler (iid, n) -> "[Handler " ^ (string_of_int iid) ^ "]" ^  (string_of_int n)
  end

  module Tbl = Map.Make (Label)

  type t = Exp.t Tbl.t

  let empty : t = Tbl.empty

  (* exp를 preorder로 순회하며 (key ↦ subexp) 테이블에 등록 *)
  let tabulate (mkkey : int -> Label.t) (e : Exp.t) (tbl : t) : t =
    let rec go (n : int) (e : Exp.t) (tbl : t) : int * t =
      let tbl = Tbl.add (mkkey n) e tbl in
      let n' = n + 1 in
      match e with
      | Exp.Unit | Exp.Int _ | Exp.Var _ | Exp.Enable | Exp.Disable ->
          (n', tbl)

      | Exp.Bop (_, e1, e2)
      | Exp.Malloc (e1, e2)
      | Exp.Assign (e1, e2)
      | Exp.Seq (e1, e2)
      | Exp.While (e1, e2) ->
          let n, tbl = go n' e1 tbl in
          go n e2 tbl

      | Exp.Deref (base, idx) ->
          let n, tbl = go n' base tbl in
          go n idx tbl

      | Exp.If (c, t, f) ->
          let n, tbl = go n' c tbl in
          let n, tbl = go n t tbl in
          go n f tbl

      | Exp.Let (_, e1, e2) ->
          let n, tbl = go n' e1 tbl in
          go n e2 tbl
    in
    snd (go 0 e tbl)

  let tabulate_all (p : Program.t) : t =
    let tbl0 = empty in
    (* globals *)
    let tbl1 = tabulate (fun n -> Label.Init n) p.init.globals tbl0 in
    (* handlers *)
    let tbl2 =
      List.fold_left
        (fun tbl (d : Handler.def) ->
          tabulate (fun n -> Label.Handler (d.iid, n)) d.body tbl)
        tbl1
        p.init.handlers
    in
    (* main *)
    tabulate (fun n -> Label.Main n) p.main tbl2

  let iter (f : Label.t -> Exp.t -> unit) (tbl : t) : unit =
    Tbl.iter f tbl

end
