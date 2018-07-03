
type ('s, 't, 'a, 'b) lens =
  ('s -> 'a) * ('b -> 's -> 't)
type ('s, 'a) lens' =
  ('s, 's, 'a, 'a) lens

let _1 : ('x * 'a, 'y * 'a, 'x, 'y) lens = fst, fun x (_, y) -> x, y
let _2 : ('a * 'x, 'a * 'y, 'x, 'y) lens = snd, fun y (x, _) -> x, y

module Lens = struct
  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) lens
  type ('s, 'a) t' = ('s, 'a) lens'

  let[@ocaml.inline] view (lns : ('s, 't, 'a, 'b) t) s : 'a =
    fst lns s
  let[@ocaml.inline] set (lns : ('s, 't, 'a, 'b) t) s x : 't =
    snd lns x s
  let[@ocaml.inline] over (lns : ('s, 't, 'a, 'b) t) ~(f : 'a -> 'b) s : 't =
    snd lns (f (fst lns s)) s
  let[@ocaml.inline] compose lns1 lns2 : (_, _, _, _) t =
    (fun s -> view lns2 (view lns1 s)),
    (fun y s -> set lns1 s (set lns2 (view lns1 s) y))

  module Infix = struct
    let[@ocaml.inline] (^.) s lns = view lns s
    let (^>) = compose
  end
  include Infix
end
