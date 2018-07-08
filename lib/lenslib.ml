
type ('s, 't, 'a, 'b) lens = ('s -> 'a) * ('b -> 's -> 't)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens

let _1 = fst, fun x (_, y) -> x, y
let _2 = snd, fun y (x, _) -> x, y

let[@ocaml.inline] lens ~view ~set = view, set
let[@ocaml.inline] view lns s = fst lns s
let[@ocaml.inline] set lns s x = snd lns x s
let[@ocaml.inline] over lns ~f s = snd lns (f (fst lns s)) s
let[@ocaml.inline] compose lns1 lns2 =
  (fun s -> view lns2 (view lns1 s)),
  (fun y s -> set lns1 s (set lns2 (view lns1 s) y))

module Infix = struct
  let (^.) s lns = view lns s
  let (^~) lns x s = set lns s x
  let (^%) lns f s = over lns ~f s
  let (^>) = compose
  let (^<) m n = compose n m
end
