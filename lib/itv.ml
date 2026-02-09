module Bound = struct
  type t = Z of int | P_inf | N_inf

  let compare b1 b2 =
    match (b1, b2) with
    | N_inf, N_inf | P_inf, P_inf -> 0
    | N_inf, _ -> -1
    | _, N_inf -> 1
    | P_inf, _ -> 1
    | _, P_inf -> -1
    | Z n1, Z n2 -> Int.compare n1 n2

  let max b1 b2 =
    match (b1, b2) with
    | N_inf, b | b, N_inf -> b
    | P_inf, _ | _, P_inf -> P_inf
    | Z n1, Z n2 -> Z (max n1 n2)

  let min b1 b2 =
    match (b1, b2) with
    | N_inf, _ | _, N_inf -> N_inf
    | P_inf, b | b, P_inf -> b
    | Z n1, Z n2 -> Z (min n1 n2)

  (* b1 <= b2 *)
  let leq b1 b2 = min b1 b2 = b1
  let ( <= ) = leq

  let add b1 b2 =
    match (b1, b2) with
    | N_inf, P_inf | P_inf, N_inf -> failwith "plus_bound not defined"
    | N_inf, _ | _, N_inf -> N_inf
    | P_inf, _ | _, P_inf -> P_inf
    | Z n1, Z n2 -> Z (n1 + n2)

  let ( + ) = add

  let mul b1 b2 =
    match (b1, b2) with
    | N_inf, P_inf | P_inf, N_inf -> N_inf
    | N_inf, N_inf | P_inf, P_inf -> P_inf
    | Z n1, Z n2 -> Z (n1 * n2)
    | N_inf, Z n | Z n, N_inf ->
        if n > 0 then N_inf else if n = 0 then Z 0 else P_inf
    | P_inf, Z n | Z n, P_inf ->
        if n > 0 then P_inf else if n = 0 then Z 0 else N_inf

  let ( * ) = mul
  let neg b = match b with N_inf -> P_inf | P_inf -> N_inf | Z n -> Z (-n)
  let ( ~- ) = neg

  let string_of_t = function
    | P_inf -> "∞"
    | N_inf -> "-∞"
    | Z n -> string_of_int n
end

(*
 * always Itv (l, r) satisfies: 
 * (1) l <= r
 * (2) if l = -∞ then r != -∞
 * (3) if r = ∞ then l != ∞
 *)
type t = Bot | Itv of Bound.t * Bound.t

let top = Itv (N_inf, P_inf)
let bot = Bot

let alpha n = Itv (Z n, Z n)

let compare i1 i2 =
  match (i1, i2) with
  | Bot, Bot -> 0
  | Bot, _ -> -1
  | _, Bot -> 1
  | Itv (l1, r1), Itv (l2, r2) ->
      let c = Bound.compare l1 l2 in
      if c <> 0 then c else Bound.compare r1 r2

let meet i1 i2 =
  match (i1, i2) with
  | Bot, _ | _, Bot -> Bot
  | Itv (l, r), Itv (l', r') -> (
      match Bound.(max l l', min r r') with
      | N_inf, r -> Itv (N_inf, r)
      | l, P_inf -> Itv (l, P_inf)
      | Z l, Z r -> if l <= r then Itv (Z l, Z r) else Bot
      | _ -> Bot)

let join i1 i2 =
  match (i1, i2) with
  | Bot, i | i, Bot -> i
  | Itv (l, r), Itv (l', r') ->
      let open Bound in
      Itv (min l l', max r r')

let widen i1 i2 =
  let i = join i1 i2 in
  match (i1, i) with
  | Bot, _ | _, Bot -> i
  | Itv (l, r), Itv (l', r') ->
      let open Bound in
      let left = if l <= l' then l else N_inf in
      let right = if r' <= r then r else P_inf in
      Itv (left, right)

let add i1 i2 =
  match (i1, i2) with
  | Bot, _ | _, Bot -> Bot
  | Itv (l, r), Itv (l', r') ->
      let open Bound in
      Itv (l + l', r + r')

let ( ++ ) = add

let mul i1 i2 =
  match (i1, i2) with
  | Bot, _ | _, Bot -> Bot
  | Itv (l, r), Itv (l', r') ->
      let open Bound in
      let l = [ l * l'; l * r'; r * l'; r * r' ] in
      Itv (List.fold_left min P_inf l, List.fold_left max N_inf l)

let ( ** ) = mul

let neg = function
  | Bot -> Bot
  | Itv (l, r) ->
      let open Bound in
      Itv (~-r, ~-l)

let ( ~-- ) = neg

module Bool = struct
  let true_ = Itv (Z 1, Z 1)
  let false_ = Itv (Z 0, Z 0)
  let top = Itv (Z 0, Z 1)
  let bot = Bot
end

(** Interval less than lifted *)
let less i1 i2 =
  let open Bool in
  match (i1, i2) with
  | Bot, _ | _, Bot -> bot
  | Itv (l, r), Itv (l', r') -> (
      match Bound.(l' <= r, r' <= l) with
      | true, true -> false_
      | true, false -> top
      | false, _ -> true_)

let lt i1 i2 = less i1 i2
let ge i1 i2 =
  let b = lt i1 i2 in
  let open Bool in
  if b = true_ then false_
  else if b = false_ then true_
  else top

let gt i1 i2 =
  let open Bool in
  match (i1, i2) with
  | Bot, _ | _, Bot -> bot
  | Itv (l, r), Itv (l', r') -> (
      match Bound.(l <= r', r <= l') with
      | true, true -> false_
      | true, false -> top
      | false, _ -> true_)
let le i1 i2 =
  let b = gt i1 i2 in
  let open Bool in
  if b = true_ then false_
  else if b = false_ then true_
  else top

let eq i1 i2 =
  let b1 = ge i1 i2 in
  let b2 = le i1 i2 in
  let open Bool in
  if b1 = true_ && b2 = true_ then true_
  else if b1 = false_ || b2 = false_ then false_
  else top

let ne i1 i2 =
  let b = eq i1 i2 in
  let open Bool in
  if b = true_ then false_
  else if b = false_ then true_
  else top

(* Interval domain partial order *)
let leq i1 i2 =
  match (i1, i2) with
  | Bot, _ -> true
  | _, Bot -> false
  | Itv (l1, r1), Itv (l2, r2) ->
      let open Bound in
      l2 <= l1 && r1 <= r2

let ( <= ) = leq

(** Does the interval contain 0? *)
let maybe_false = function
  | Bot -> false
  | Itv (l, r) ->
      let open Bound in
      l <= Z 0 && Z 0 <= r

(* Does the interval contain a non-zero value? *)
let maybe_true = function
  | Bot -> false
  | Itv (l, r) -> ( match (l, r) with Z 0, Z 0 -> false | _ -> true)

let not i =
  let open Bool in
  match (maybe_false i, maybe_true i) with
  | false, false -> bot
  | false, true -> false_
  | true, false -> true_
  | true, true -> top

let and_ i1 i2 =
  let open Bool in
  match (i1, i2) with
  | Bot, _ | _, Bot -> bot
  | _ -> (
      let t1 = maybe_true i1 in
      let t2 = maybe_true i2 in
      let f1 = maybe_false i1 in
      let f2 = maybe_false i2 in
      match (t1 && t2, f1 || f2) with
      | true, true -> top
      | true, false -> true_
      | _ -> false_)

let or_ i1 i2 =
  let open Bool in
  match (i1, i2) with
  | Bot, _ | _, Bot -> bot
  | _ -> (
      let t1 = maybe_true i1 in
      let t2 = maybe_true i2 in
      let f1 = maybe_false i1 in
      let f2 = maybe_false i2 in
      match (t1 || t2, f1 && f2) with
      | true, true -> top
      | true, false -> true_
      | _ -> false_)

let string_of_t = function
  | Bot -> "⟂"
  | Itv (l, r) -> Bound.("[" ^ string_of_t l ^ "," ^ string_of_t r ^ "]")
