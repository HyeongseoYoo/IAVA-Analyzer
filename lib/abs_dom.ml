open Syntax
open Domain

module PPMap = Map.Make(ProgramPoint)
module LblMap = Map.Make(Exp.Lbl)
module PPSet = Set.Make(ProgramPoint)

module Abs_Loc = struct
  type t = Bot | AVarLoc of {id: Var.t; offset: Itv.t} | AHeapLoc of {lbl: Exp.Lbl.t; offset: Itv.t} | Top

  let bot = Bot
  let top = Top

  let compare l1 l2 =
    match (l1, l2) with
    | Bot, Bot | Top, Top -> 0
    | Bot, _ -> -1
    | _, Bot -> 1
    | Top, _ -> 1
    | _, Top -> -1
    | AVarLoc {id = id1; offset = off1}, AVarLoc {id = id2; offset = off2} ->
        let c = Var.compare id1 id2 in
        if c <> 0 then c else Itv.compare off1 off2
    | AHeapLoc {lbl = lbl1; offset = off1}, AHeapLoc {lbl = lbl2; offset = off2} ->
        let c = Exp.Lbl.compare lbl1 lbl2 in
        if c <> 0 then c else Itv.compare off1 off2
    | AVarLoc _, AHeapLoc _ -> -1
    | AHeapLoc _, AVarLoc _ -> 1

  let alpha (l : Loc.t) : t =
    match l with
    | VarLoc {id; offset} -> AVarLoc {id; offset = Itv.alpha offset}
    | HeapLoc {lbl; offset} -> AHeapLoc {lbl; offset = Itv.alpha offset}

  let get (id: Var.t) : t = AVarLoc {id; offset = Itv.alpha 0}
  let alloc (lbl: Exp.Lbl.t) (offset: Itv.t) : t = AHeapLoc {lbl; offset}

  let offset_add (base: t) (offset: Itv.t) : t =
    match base with
      | Top -> Top
      | Bot | AVarLoc _ -> Bot
      | AHeapLoc { lbl; offset = base_off } -> AHeapLoc { lbl; offset = Itv.add base_off offset }

  let leq l1 l2 =
    match (l1, l2) with
    | Bot, _ -> true
    | _, Top -> true
    | AVarLoc {id = id1; offset = off1}, AVarLoc {id = id2; offset = off2} ->
        Var.compare id1 id2 = 0 && Itv.leq off1 off2
    | AHeapLoc {lbl = lbl1; offset = off1}, AHeapLoc {lbl = lbl2; offset = off2} ->
        Exp.Lbl.compare lbl1 lbl2 = 0 && Itv.leq off1 off2
    | _ -> false
  
  let single_eq l1 l2 =
    match (l1, l2) with
    | Bot, _ | _, Bot -> false
    | AVarLoc {id = id1; offset = off1}, AVarLoc {id = id2; offset = off2} ->
        Var.compare id1 id2 = 0 && Itv.single_eq off1 off2
    | AHeapLoc {lbl = lbl1; offset = off1}, AHeapLoc {lbl = lbl2; offset = off2} ->
        Exp.Lbl.compare lbl1 lbl2 = 0 && Itv.single_eq off1 off2
    | _ -> false

  let join l1 l2 =
    match (l1, l2) with
    | Bot, l | l, Bot -> l
    | Top, _ | _, Top -> Top
    | AVarLoc {id = id1; offset = off1}, AVarLoc {id = id2; offset = off2} when Var.compare id1 id2 = 0 ->
        AVarLoc {id = id1; offset = Itv.join off1 off2}
    | AHeapLoc {lbl = lbl1; offset = off1}, AHeapLoc {lbl = lbl2; offset = off2} when Exp.Lbl.compare lbl1 lbl2 = 0 ->
        AHeapLoc {lbl = lbl1; offset = Itv.join off1 off2}
    | _ -> Top

  let widen l1 l2 =
    match (l1, l2) with
    | Bot, l | l, Bot -> l
    | Top, _ | _, Top -> Top
    | AVarLoc {id = id1; offset = off1}, AVarLoc {id = id2; offset = off2} when Var.compare id1 id2 = 0 ->
        AVarLoc {id = id1; offset = Itv.widen off1 off2}
    | AHeapLoc {lbl = lbl1; offset = off1}, AHeapLoc {lbl = lbl2; offset = off2} when Exp.Lbl.compare lbl1 lbl2 = 0 ->
        AHeapLoc {lbl = lbl1; offset = Itv.widen off1 off2}
    | _ -> Top

  let string_of_t = function
    | Bot -> "⊥"
    | Top -> "⊤"
    | AVarLoc {id; offset} -> Printf.sprintf "%s" id
    | AHeapLoc {lbl; offset} -> Printf.sprintf "%s+%s" (Exp.Lbl.string_of_t lbl) (Itv.string_of_t offset)

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
    match l with
    | Abs_Loc.AVarLoc {id; offset} ->  LocMap.add l (v, PPSet.singleton pp) m
    | _ -> 
      let (old_v, old_pps) = find m l in
      let new_v = Abs_Val.join old_v v in
      let new_pps = PPSet.add pp old_pps in
      LocMap.add l (new_v, new_pps) m

let fold (f : Abs_Loc.t -> (Abs_Val.t * PPSet.t) -> 'a -> 'a) (m : t) (init : 'a) : 'a =
  LocMap.fold (fun l (v, pps) acc -> f l (v, pps) acc) m init

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
      String.concat "\n" binding_strs
end

module Error = struct
  type access = Read | Write
  type t = {
    at: ProgramPoint.t;
    access: access;
    base: Abs_Loc.t;
    (* offset: Itv.t; *)
    in_itv: Itv.t;
    left_oob: Itv.t;
    right_oob: Itv.t;

    base_pp: PPSet.t;
    offset_pp: PPSet.t;

    handler_caused: bool;
  }

  let make
      ~(at:ProgramPoint.t)
      ~(access:access)
      ~(base:Abs_Loc.t)
      (* ~(offset:Itv.t) *)
      ~(in_itv:Itv.t)
      ~(left_oob:Itv.t)
      ~(right_oob:Itv.t)
      ~(base_pp:PPSet.t)
      ~(offset_pp:PPSet.t)
      ~(handler_caused:bool)
      : t =
    { at; access; base; in_itv; left_oob; right_oob; base_pp; offset_pp; handler_caused }

  (* Set에 넣으려면 total order 필요 *)
  let compare_access a1 a2 =
    match (a1, a2) with
    | Read, Read | Write, Write -> 0
    | Read, Write -> -1
    | Write, Read -> 1

  let compare (e1:t) (e2:t) : int =
    let c = ProgramPoint.compare e1.at e2.at in
    if c <> 0 then c else
    let c = compare_access e1.access e2.access in
    if c <> 0 then c else
    let c = Abs_Loc.compare e1.base e2.base in
    if c <> 0 then c else
    (* let c = Itv.compare e1.offset e2.offset in
    if c <> 0 then c else *)
    let c = Itv.compare e1.in_itv e2.in_itv in
    if c <> 0 then c else
    let c = Itv.compare e1.left_oob e2.left_oob in
    if c <> 0 then c else
    let c = Itv.compare e1.right_oob e2.right_oob in
    if c <> 0 then c else
    let c = PPSet.compare e1.base_pp e2.base_pp in
    if c <> 0 then c else
    let c = PPSet.compare e1.offset_pp e2.offset_pp in
    if c <> 0 then c else
    Bool.compare e1.handler_caused e2.handler_caused

  let string_of_access = function
    | Read -> "Read"
    | Write -> "Write"

  let string_of_ppset (pps:PPSet.t) : string =
    let elems =
      PPSet.fold
        (fun pp acc ->
          let s =
            try ProgramPoint.string_of_t pp with _ -> "<pp>"
          in
          s :: acc)
        pps
        []
    in
    "{" ^ String.concat ", " (List.rev elems) ^ "}"
  let to_string (e:t) : string =
    let at_s =
      (* 예: ProgramPoint.string_of_t 가 없으면 직접 구현/교체 *)
      try ProgramPoint.string_of_t e.at with _ -> "<pp>"
    in
    Printf.sprintf
      "[OOB:%s]%s at=%s base=%s in=%s left=%s right=%s base_src=%s off_src=%s"
      (string_of_access e.access)
      (if e.handler_caused then " (handler-caused)" else "")
      at_s
      (Abs_Loc.string_of_t e.base)
      (* (Itv.string_of_t e.offset) *)
      (Itv.string_of_t e.in_itv)
      (Itv.string_of_t e.left_oob)
      (Itv.string_of_t e.right_oob)
      (string_of_ppset e.base_pp)
      (string_of_ppset e.offset_pp)

  let is_handler_caused (e:t) = e.handler_caused
end

module ErrorSet = struct
  module S = Set.Make(struct
    type t = Error.t
    let compare = Error.compare
  end)
  include S

  let string_of_t (es:t) : string =
    let elems = elements es |> List.map Error.to_string in
    "{" ^ String.concat ", " elems ^ "}"
end


