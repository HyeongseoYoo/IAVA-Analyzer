open Syntax

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

module Var = struct
  type t = string

  let compare = String.compare

  module Map = Map.Make (String)
end


module Loc = struct
  type t = VarLoc of {id: Var.t; offset: int} | HeapLoc of {lbl: Exp.Lbl.t; offset: int}

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
  let compare l1 l2 =
    match (l1, l2) with
    | VarLoc {id = id1; offset = off1}, VarLoc {id = id2; offset = off2} ->
        let c = Var.compare id1 id2 in
        if c <> 0 then c else Int.compare off1 off2
    | HeapLoc {lbl = lbl1; offset = off1}, HeapLoc {lbl = lbl2; offset = off2} ->
        let c = Exp.Lbl.compare lbl1 lbl2 in
        if c <> 0 then c else Int.compare off1 off2
    | VarLoc _, HeapLoc _ -> -1
    | HeapLoc _, VarLoc _ -> 1

  let get x = VarLoc {id = x; offset = 0}
  let alloc lbl n = HeapLoc {lbl; offset = n}

  let string_of_t = function
    | VarLoc {id; offset} -> Printf.sprintf "%s+%d" id offset
    | HeapLoc {lbl; offset} -> Printf.sprintf "%s+%d" (Exp.Lbl.string_of_t lbl) offset

end

module Value = struct
  type t = Int of int | Loc of Loc.t | Unit

  let compare v1 v2 =
    match (v1, v2) with
    | Int n1, Int n2 -> Int.compare n1 n2
    | Loc a1, Loc a2 -> Loc.compare a1 a2
    | Unit, Unit -> 0
    | Unit, _ -> -1
    | _, Unit -> 1
    | Int _, _ -> -1
    | _, Int _ -> 1

  let string_of_t = function
    | Int n -> "Int " ^ string_of_int n
    | Loc a -> Loc.string_of_t a
    | Unit -> "unit"
end


module IidSet = Set.Make (Int)

module Interrupt = struct
  type t = Disabled | Enabled

  let compare i1 i2 =
    match (i1, i2) with
    | Disabled, Disabled -> 0
    | Disabled, Enabled -> -1
    | Enabled, Disabled -> 1
    | Enabled, Enabled -> 0
  
  let join i1 i2 =
    match (i1, i2) with
    | Disabled, Disabled -> Disabled
    | _ -> Enabled
  let string_of_t = function Disabled -> "Disabled" | Enabled -> "Enabled"
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


module VarSet = Set.Make (Var)

module VarTbl = struct
  type t = int Var.Map.t

  let empty : t = Var.Map.empty

  let find (tbl : t) (x : Var.t) : int =
    match Var.Map.find_opt x tbl with
    | Some n -> n
    | None -> failwith ("[VarTbl] Variable not found: " ^ x)

  let build (vars : VarSet.t) : t =
    let rec build' vars n tbl =
      match VarSet.elements vars with
      | [] -> tbl
      | x :: _ -> build' (VarSet.remove x vars) (n + 1) (Var.Map.add x n tbl)
    in
    build' vars 0 Var.Map.empty
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

  type t = Exp.lbl_t IidMap.t

  let empty : t = IidMap.empty

  let add (hs : t) (iid : int) (body : Exp.lbl_t) : t =
    IidMap.add iid body hs

  let lookup (hs : t) (iid : int) : Exp.lbl_t option =
    IidMap.find_opt iid hs
end
