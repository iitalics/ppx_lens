
type ('s, 't, 'a, 'b) lens = ('s -> 'a) * ('b -> 's -> 't)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens

val lens    : view:('s -> 'a) -> set:('b -> 's -> 't) -> ('s, 't, 'a, 'b) lens
val view    : ('s, _ , 'a, _ ) lens -> ('s -> 'a)
val set     : ('s, 't, _ , 'b) lens -> ('b -> 's -> 't)
val over    : ('s, 't, 'a, 'b) lens -> f:('a -> 'b) -> ('s -> 't)
val compose : ('ss, 'tt, 's, 't) lens
              -> ('s, 't, 'a, 'b) lens
              -> ('ss, 'tt, 'a, 'b) lens

val _1 : ('x * 'a, 'y * 'a, 'x, 'y) lens
val _2 : ('a * 'x, 'a * 'y, 'x, 'y) lens
val _id : ('a, 'b, 'a, 'b) lens
val _hd : ('a list, 'a) lens'
val _tl : ('a list, 'a list) lens'

module Infix : sig
  (** view *)
  val (^.) : 's -> ('s, _, 'a, _) lens -> 'a

  (** set *)
  val (^~) : ('s, 't, _, 'b) lens -> 'b -> 's -> 't

  (** over *)
  val (^%) : ('s, 't, 'a, 'b) lens -> ('a -> 'b) -> ('s -> 't)

  (** compose *)
  val (^>) : ('ss, 'tt, 's, 't) lens
             -> ('s, 't, 'a, 'b) lens
             -> ('ss, 'tt, 'a, 'b) lens

  (** compose (flipped) *)
  val (^<) : ('s, 't, 'a, 'b) lens
             -> ('ss, 'tt, 's, 't) lens
             -> ('ss, 'tt, 'a, 'b) lens
end
