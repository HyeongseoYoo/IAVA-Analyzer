open Syntax

module Loc = struct
  type t = int

  module Map = Map.Make (Int)

  let init : t = 0
  let string_of_t (a : t) : string = "Loc " ^ string_of_int a
end

module Value = struct
  type t = Int of int | Loc of Loc.t | Unit

  let compare v1 v2 =
    match (v1, v2) with
    | Int n1, Int n2 -> Int.compare n1 n2
    | Loc a1, Loc a2 -> Int.compare a1 a2
    | Unit, Unit -> 0
    | Unit, _ -> -1
    | _, Unit -> 1
    | Int _, _ -> -1
    | _, Int _ -> 1

  let string_of_t = function
    | Int n -> "Int " ^ string_of_int n
    | Loc a -> "Loc " ^ string_of_int a
    | Unit -> "unit"
end

module ProgramPoint = struct
  type t = Label of Exp.Lbl.t | Unit

  let compare p1 p2 =
    match (p1, p2) with
    | Unit, Unit -> 0
    | Unit, _ -> -1
    | _, Unit -> 1
    | Label l1, Label l2 -> Exp.Lbl.compare l1 l2

  let string_of_t = function Unit -> "●" | Label l -> Exp.Lbl.string_of_t l
end

module IidSet = Set.Make (Int)

module Interrupt = struct
  type t = Disabled | Enabled
end

module Outcome = struct
  type t = I of int | Done

  let compare o1 o2 =
    match (o1, o2) with
    | Done, Done -> 0
    | Done, _ -> -1
    | _, Done -> 1
    | I i1, I i2 -> Int.compare i1 i2
end

module Var = struct
  type t = string

  let compare = String.compare

  module Map = Map.Make (String)
end

module Env = struct
  type t = Loc.t Var.Map.t

  let empty : t = Var.Map.empty

  let string_of_t (env : t) : string =
    let bindings =
      Var.Map.bindings env
      |> List.map (fun (x, a) -> Printf.sprintf "%s ↦ %s" x (Loc.string_of_t a))
    in
    String.concat "\n" bindings
end

module Mem = struct
  type t = (Value.t * ProgramPoint.t) Loc.Map.t

  let empty : t = Loc.Map.empty

  let string_of_t (m : t) : string =
    let bindings =
      Loc.Map.bindings m
      |> List.map (fun (a, (v, p)) ->
          Printf.sprintf "%s ↦ <%s, %s>" (Loc.string_of_t a)
            (Value.string_of_t v)
            (ProgramPoint.string_of_t p))
    in
    String.concat "\n" bindings

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let string_of_t (ms : t) : string =
      let elems = elements ms |> List.map string_of_t in
      "{" ^ String.concat "\n -- \n" elems ^ "}"
  end
end

module HandlerStore = struct
  module IidMap = Map.Make (Int)

  type t = (Exp.lbl_t * Env.t) IidMap.t

  let empty : t = IidMap.empty

  let add (hs : t) ~(iid : int) ~(body : Exp.lbl_t) ~(env : Env.t) : t =
    IidMap.add iid (body, env) hs

  let lookup (hs : t) (iid : int) : (Exp.lbl_t * Env.t) option =
    IidMap.find_opt iid hs
end
