open Syntax
open Domain

module PPMap = Map.Make(ProgramPoint)

module Abs_Loc = struct
  type t = Itv.t PPMap.t

  let bot = PPMap.empty

  let compare (l1 : t) (l2 : t) : int =
    PPMap.compare Itv.compare l1 l2
  
  let alpha (loc : Loc.t) : t = 
    let (pp, n) = loc in
    PPMap.singleton pp (Itv.alpha n)

  let join (l1 : t) (l2 : t) : t =
    let f _key itv1 itv2 = Some (Itv.join itv1 itv2) in
    PPMap.union f l1 l2

  let widen (l1 : t) (l2 : t) : t =
    let f _key itv1 itv2 = Some (Itv.widen itv1 itv2) in
    PPMap.union f l1 l2
  
  let leq (l1 : t) (l2 : t) : bool =
    PPMap.for_all
      (fun pp itv1 ->
        match PPMap.find_opt pp l2 with
        | None -> Itv.leq itv1 Itv.bot
        | Some itv2 -> Itv.leq itv1 itv2)
      l1
  let alloc (pp : ProgramPoint.t) : t =
    PPMap.add pp (Itv.alpha 0) bot

  let create (pp: ProgramPoint.t) (itv : Itv.t) : t =
    PPMap.add pp itv bot

  let string_of_t (l : t) : string =
    if l = bot then "⊥"
    else begin
    let bindings = PPMap.bindings l in
    let binding_strs =
      List.map
        (fun (pp, itv) ->
          let pp_str = ProgramPoint.string_of_t pp
          and itv_str = Itv.string_of_t itv in
          Printf.sprintf "<%s, %s>" pp_str itv_str)
        bindings
    in
    "{" ^ String.concat "; " binding_strs ^ "}" end
end

module Abs_Unit = struct
  type t = Unit | Bot
  let bot = Bot

  let compare (u1 : t) (u2 : t) : int =
    match (u1, u2) with
    | Bot, Bot | Unit, Unit -> 0
    | Bot, Unit -> -1
    | Unit, Bot -> 1

  let join (u1 : t) (u2 : t) : t =
    match (u1, u2) with Unit, _ | _, Unit -> Unit | _ -> Bot

  let widen = join

  let leq (u1 : t) (u2 : t) : bool =
    match (u1, u2) with
    | Bot, _ -> true
    | Unit, Unit -> true
    | Unit, Bot -> false

  let string_of_t = function
    | Bot -> "⊥"
    | Unit -> "Unit"
end

module Abs_Env = struct
  type t = Abs_Loc.t Var.Map.t

  let bot : t = Var.Map.empty

  let compare (e1 : t) (e2 : t) : int =
    Var.Map.compare Abs_Loc.compare e1 e2

  let alpha (env : Env.t) : t =
    Var.Map.fold
      (fun x l acc -> Var.Map.add x (Abs_Loc.alpha l) acc)
      env Var.Map.empty

  let join (e1 : t) (e2 : t) : t =
    let f _key v1 v2 = Some (Abs_Loc.join v1 v2) in
    Var.Map.union f e1 e2

  let widen (e1 : t) (e2 : t) : t =
    let f _key v1 v2 = Some (Abs_Loc.widen v1 v2) in
    Var.Map.union f e1 e2

  let leq (e1 : t) (e2 : t) : bool =
    Var.Map.for_all
      (fun x l1 ->
        match Var.Map.find_opt x e2 with
        | None -> Abs_Loc.leq l1 Abs_Loc.bot
        | Some l2 -> Abs_Loc.leq l1 l2)
      e1
  let find (e : t) (x : string) : Abs_Loc.t option =
    Var.Map.find_opt x e
  
  let write (e : t) (x : string) (l : Abs_Loc.t) : t =
    Var.Map.add x l e
  
  let string_of_t (e : t) : string = 
    if e = bot then "⊥"
    else begin
    let bindings = Var.Map.bindings e in
    let binding_strs =
      List.map
        (fun (x, l) ->
          let l_str = Abs_Loc.string_of_t l in
          Printf.sprintf "%s ↦ %s" x l_str)
        bindings
    in
    "{" ^ String.concat "; " binding_strs ^ "}" end
end

module Abs_Val = struct
  type t = Itv.t * Abs_Unit.t * Abs_Loc.t

  let top = (Itv.top, Abs_Unit.Unit, Abs_Loc.bot)
  let bot = (Itv.bot, Abs_Unit.bot, Abs_Loc.bot)

  let alpha (v : Value.t) : t =
    match v with
    | Value.Int n -> (Itv.alpha n, Abs_Unit.bot, Abs_Loc.bot)
    | Value.Unit -> (Itv.bot, Abs_Unit.Unit, Abs_Loc.bot)
    | Value.Loc l -> (Itv.bot, Abs_Unit.bot, Abs_Loc.alpha l)

  let compare (v1: t) (v2: t) : int =
    let (itv1, u1, l1) = v1 in
    let (itv2, u2, l2) = v2 in
    let c = Itv.compare itv1 itv2 in
    if c <> 0 then c
    else
      let c = Abs_Unit.compare u1 u2 in
      if c <> 0 then c
      else Abs_Loc.compare l1 l2
  
  let join (v1: t) (v2: t) : t =
    let (itv1, u1, l1) = v1 in
    let (itv2, u2, l2) = v2 in
    (Itv.join itv1 itv2, Abs_Unit.join u1 u2, Abs_Loc.join l1 l2)

  let widen (v1: t) (v2: t) : t =
    let (itv1, u1, l1) = v1 in
    let (itv2, u2, l2) = v2 in
    (Itv.widen itv1 itv2, Abs_Unit.widen u1 u2, Abs_Loc.widen l1 l2)
    
  let leq (v1: t) (v2: t) : bool =
    let (itv1, u1, l1) = v1 in
    let (itv2, u2, l2) = v2 in
    Itv.leq itv1 itv2 && Abs_Unit.leq u1 u2 && Abs_Loc.leq l1 l2
  
  let equal (v1: t) (v2: t) : bool =
    compare v1 v2 = 0

  let string_of_t (v : t) : string =
    let (itv, u, l) = v in
    let itv_str = Itv.string_of_t itv in
    let u_str = Abs_Unit.string_of_t u in
    let l_str = Abs_Loc.string_of_t l in
    Printf.sprintf "<%s, %s, %s>" itv_str u_str l_str
end

module PPSet = Set.Make(ProgramPoint)

module Abs_Mem = struct
  module LocMap = Map.Make(struct
    type t = Abs_Loc.t
    let compare = Abs_Loc.compare
  end)

  type t = (Abs_Val.t * PPSet.t) LocMap.t

  let bot : t = LocMap.empty

  let compare (m1 : t) (m2 : t) : int =
    LocMap.compare
      (fun (v1, p1) (v2, p2) ->
        let c = Abs_Val.compare v1 v2 in
        if c <> 0 then c else PPSet.compare p1 p2)
      m1 m2

  let join (m1 : t) (m2 : t) : t =
    let f _key (v1, p1) (v2, p2) =
      Some (Abs_Val.join v1 v2, PPSet.union p1 p2)
    in
    LocMap.union f m1 m2

  let widen (m1 : t) (m2 : t) : t =
    let f _key (v1, p1) (v2, p2) =
      Some (Abs_Val.widen v1 v2, PPSet.union p1 p2)
    in
    LocMap.union f m1 m2

  let leq (m1 : t) (m2 : t) : bool =
    LocMap.for_all
      (fun l (v1, p1) ->
        match LocMap.find_opt l m2 with
        | None -> false
        | Some (v2, p2) -> Abs_Val.leq v1 v2 && PPSet.subset p1 p2)
      m1
  let find (m : t) (l : Abs_Loc.t) : (Abs_Val.t * PPSet.t) =
    match LocMap.find_opt l m with
    | Some vp -> vp
    | None -> (Abs_Val.bot, PPSet.empty)

  let write (m : t) (l : Abs_Loc.t) (v : Abs_Val.t) (pp : ProgramPoint.t) : t =
    let (old_v, old_pps) = find m l in
    let new_v = Abs_Val.join old_v v in
    let new_pps = PPSet.add pp old_pps in
    LocMap.add l (new_v, new_pps) m

    let string_of_t (m : t) : string =
      let bindings = LocMap.bindings m in
      let binding_strs =
        List.map
          (fun (l, (v, pps)) ->
            let l_str = Abs_Loc.string_of_t l
            and v_str = Abs_Val.string_of_t v
            and pps_str =
              PPSet.elements pps
              |> List.map ProgramPoint.string_of_t
              |> String.concat ", "
            in
            Printf.sprintf "%s ↦ <%s, {%s}>" l_str v_str pps_str)
          bindings
      in
      "{ " ^ String.concat "; " binding_strs ^ " }"
end

module Abs_HandlerStore = struct
  module IidMap = Map.Make(Int)

  type t = (Exp.lbl_t * Abs_Env.t) IidMap.t

  let bot : t = IidMap.empty

  let lookup (hs : t) (iid : int) : (Exp.lbl_t * Abs_Env.t) option =
    IidMap.find_opt iid hs
  let compare (h1 : t) (h2 : t) : int =
    IidMap.compare
      (fun (e1, env1) (e2, env2) ->
        let c = Stdlib.compare e1 e2 in
        if c <> 0 then c else Abs_Env.compare env1 env2)
      h1 h2

  let join (h1 : t) (h2 : t) : t =
    let f _key (e1, env1) (e2, env2) =
      let exp = if Stdlib.compare e1 e2 = 0 then e1 else e1 in
      Some (exp, Abs_Env.join env1 env2)
    in
    IidMap.union f h1 h2

  let widen (h1 : t) (h2 : t) : t =
    let f _key (e1, env1) (e2, env2) =
      let exp = if Stdlib.compare e1 e2 = 0 then e1 else e1 in
      Some (exp, Abs_Env.widen env1 env2)
    in
    IidMap.union f h1 h2

  let leq (h1 : t) (h2 : t) : bool =
    IidMap.for_all
      (fun iid (_e1, env1) ->
        match IidMap.find_opt iid h2 with
        | None -> false
        | Some (_e2, env2) -> Abs_Env.leq env1 env2)
      h1
  let add (hs : t) (iid : int) (exp : Exp.lbl_t) (env : Abs_Env.t) : t =
    IidMap.add iid (exp, env) hs
end
