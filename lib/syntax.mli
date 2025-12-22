module Exp : sig
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
  
  val string_of_bop : bop -> string 
  val string_of_t : t -> string
end

module Handler : sig
  type iid = int
  type def = { iid : iid; body : Exp.t }

  val string_of_def : def -> string
end

module Init : sig
  type t = {
    globals : Exp.t;
    handlers : Handler.def list;
  }

  val string_of_t : t -> string
end

module Program : sig 
  type t = {
    init : Init.t;
    main : Exp.t;
  }

  val string_of_t : t -> string
end

module Tabulate : sig
  module Label : sig
    type t =
      | Init of int
      | Main of int
      | Handler of Handler.iid * int

    val compare : t -> t -> int
    val string_of_t : t -> string
  end

  type t

  val empty : t
  val tabulate_all : Program.t -> t
  val iter : (Label.t -> Exp.t -> unit) -> t -> unit
end