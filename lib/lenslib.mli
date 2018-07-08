
type ('s, 't, 'a, 'b) lens = ('s -> 'a) * ('b -> 's -> 't)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens

val _1 : ('x * 'a, 'y * 'a, 'x, 'y) lens
val _2 : ('a * 'x, 'a * 'y, 'x, 'y) lens

val view    : ('s, _ , 'a, _ ) lens -> ('s -> 'a)
val set     : ('s, 't, _ , 'b) lens -> ('s -> 'b -> 't)
val over    : ('s, 't, 'a, 'b) lens -> f:('a -> 'b) -> ('s -> 't)
val compose : ('ss, 'tt, 's, 't) lens
              -> ('s, 't, 'a, 'b) lens
              -> ('ss, 'tt, 'a, 'b) lens

module Infix : sig
  val (^.) : 's -> ('s, _, 'a, _) lens -> 'a
  val (^>) : ('ss, 'tt, 's, 't) lens
             -> ('s, 't, 'a, 'b) lens
             -> ('ss, 'tt, 'a, 'b) lens
end
