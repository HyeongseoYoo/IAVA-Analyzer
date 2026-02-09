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
end

