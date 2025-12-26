(* * Interrupt-aware toy language (init/main) * Exp-only syntax (expressions
   include commands) *)
module Exp = struct
  module Lbl = struct
    type t = Init of int | Main of int | Handler of int * int

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

    let succ_lbl = function
      | Init n -> Init (n + 1)
      | Main n -> Main (n + 1)
      | Handler (id, n) -> Handler (id, n + 1)

    let string_of_t = function
      | Init n -> "[Init]" ^ string_of_int n
      | Main n -> "[Main]" ^ string_of_int n
      | Handler (iid, n) ->
          "[Handler " ^ string_of_int iid ^ "]" ^ string_of_int n
  end

  type bop = Eq | Plus

  type lbl_t = { lbl : Lbl.t; exp : t }
  and lbl = (Lbl.t, Lbl.t) Either.t

  and t =
    | Unit
    | Int of int
    | Var of string
    | Enable
    | Disable
    | Bop of bop * lbl_t * lbl_t
    | Deref of lbl_t * lbl_t (* *e[e] *)
    | Malloc of lbl_t * lbl_t (* malloc(e, e) *)
    | Assign of lbl_t * lbl_t (* e := e *)
    | Seq of lbl_t * lbl_t
    | If of lbl_t * lbl_t * lbl_t
    | While of Lbl.t (*ghost*) * lbl_t * lbl_t
    | Let of string * lbl_t * lbl_t

  module Lbl_map = struct
    include Map.Make (struct
      type t = (Lbl.t, Lbl.t) Either.t

      let compare = Either.compare ~left:Lbl.compare ~right:Lbl.compare
    end)

    let string_of_t : key -> string = function
      | Either.Left lbl ->
          Printf.sprintf "[L]%s" (Lbl.string_of_t lbl) (* normal [L]abel *)
      | Either.Right glbl -> Printf.sprintf "[G]%s" (Lbl.string_of_t glbl)
    (* [G]host label *)
  end

  let tabulate (l_exp : lbl_t) : t Lbl_map.t =
    let rec tabulate' ({ lbl; exp } : lbl_t) (tbl : t Lbl_map.t) =
      let tbl = Lbl_map.add (Either.left lbl) exp tbl in
      match exp with
      | Unit | Int _ | Var _ | Enable | Disable -> tbl
      | Bop (_, e1, e2)
      | Deref (e1, e2)
      | Malloc (e1, e2)
      | Assign (e1, e2)
      | Seq (e1, e2)
      | Let (_, e1, e2) ->
          tbl |> tabulate' e1 |> tabulate' e2
      | If (e1, e2, e3) -> tbl |> tabulate' e1 |> tabulate' e2 |> tabulate' e3
      | While (glbl, e1, e2) ->
          tbl
          |> Lbl_map.add (Either.Right glbl) exp
          |> tabulate' e1 |> tabulate' e2
    in
    tabulate' l_exp Lbl_map.empty

  let relabel (le : lbl_t) (lt : Lbl.t) : lbl_t =
    let rec relabel' (lbl : Lbl.t) ({ exp; _ } : lbl_t) : lbl_t * Lbl.t =
      let my_lbl = Lbl.succ_lbl lbl in
      match exp with
      | Unit | Int _ | Var _ | Enable | Disable ->
          ({ lbl = my_lbl; exp }, my_lbl)
      | Bop (b, e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Bop (b, e1', e2') }, l2)
      | Deref (e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Deref (e1', e2') }, l2)
      | Malloc (e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Malloc (e1', e2') }, l2)
      | Assign (e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Assign (e1', e2') }, l2)
      | Seq (e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Seq (e1', e2') }, l2)
      | While (_, e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          let glbl = Lbl.succ_lbl l2 in
          ({ lbl = my_lbl; exp = While (glbl, e1', e2') }, glbl)
      | Let (x, e1, e2) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          ({ lbl = my_lbl; exp = Let (x, e1', e2') }, l2)
      | If (e1, e2, e3) ->
          let e1', l1 = relabel' my_lbl e1 in
          let e2', l2 = relabel' l1 e2 in
          let e3', l3 = relabel' l2 e3 in
          ({ lbl = my_lbl; exp = If (e1', e2', e3') }, l3)
    in
    fst (relabel' lt le)

  let string_of_bop : bop -> string = function Plus -> "+" | Eq -> "="

  let rec string_of_lbl_t ({ lbl; exp } : lbl_t) =
    Printf.sprintf "%s: (%s)" (Lbl.string_of_t lbl) (string_of_t exp)

  and string_of_t ?(lvl : int = 0) : t -> string = function
    | Unit -> "unit"
    | Int n -> string_of_int n
    | Var x -> x
    | Enable -> "enable"
    | Disable -> "disable"
    | Bop (bop, e1, e2) ->
        Printf.sprintf "(%s %s %s)" (string_of_lbl_t e1) (string_of_bop bop)
          (string_of_lbl_t e2)
    | Deref (e1, e2) ->
        Printf.sprintf "*%s[%s]" (string_of_lbl_t e1) (string_of_lbl_t e2)
    | Malloc (e1, e2) ->
        Printf.sprintf "malloc(%s, %s)" (string_of_lbl_t e1)
          (string_of_lbl_t e2)
    | Assign (e1, e2) ->
        Printf.sprintf "%s := %s" (string_of_lbl_t e1) (string_of_lbl_t e2)
    | Seq (e1, e2) ->
        Printf.sprintf "%s;\n%s" (string_of_lbl_t e1) (string_of_lbl_t e2)
    | If (e1, e2, e3) ->
        Printf.sprintf "if %s\nthen %s\nelse %s" (string_of_lbl_t e1)
          (string_of_lbl_t e2) (string_of_lbl_t e3)
    | While (_, e1, e2) ->
        Printf.sprintf "while %s\ndo %s" (string_of_lbl_t e1)
          (string_of_lbl_t e2)
    | Let (x, e1, e2) ->
        Printf.sprintf "let %s = %s in\n%s" x (string_of_lbl_t e1)
          (string_of_lbl_t e2)
end

module Handler = struct
  type t = { iid : int; body : Exp.lbl_t } (* handler i e *)

  let get_body h = h.body
  let get_iid h = h.iid

  let string_of_t d =
    Printf.sprintf "handler %d %s" d.iid (Exp.string_of_lbl_t d.body)

  let string_of_handler_list (li : t list) : string =
    li |> List.map string_of_t |> String.concat "\n"
end

module Program = struct
  type t = { global : Exp.lbl_t; handler : Handler.t list; main : Exp.lbl_t }

  let string_of_t (p : t) : string =
    Printf.sprintf "init {\n%s \n%s \n}\n\nmain {\n%s\n}\n"
      (Exp.string_of_lbl_t p.global)
      (Handler.string_of_handler_list p.handler)
      (Exp.string_of_lbl_t p.main)
end
