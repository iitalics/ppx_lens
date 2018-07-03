
module Posn = struct
  type t =
    { x : int
    ; y : int }
      [@@lens generate ~no_field_prefix]

  let bounce_left_wall : t -> t =
    update_x ~f:abs
end

let _1 = fst, fun x (_, y) -> x, y
let _2 = snd, fun y (x, _) -> x, y

module Lens = struct
  type ('s, 't, 'a, 'b) t =
    ('s -> 'a) * ('b -> 's -> 't)
  type ('s, 'a) u =
    ('s, 's, 'a, 'a) t

  let[@ocaml.inline] view (lns : ('s, 't, 'a, 'b) t) s : 'a =
    fst lns s
  let[@ocaml.inline] set (lns : ('s, 't, 'a, 'b) t) s x : 't =
    snd lns x s
  let[@ocaml.inline] over (lns : ('s, 't, 'a, 'b) t) ~(f : 'a -> 'b) s : 't =
    snd lns (f (fst lns s)) s
  let[@ocaml.inline] compose lns1 lns2 : (_, _, _, _) t =
    (fun s -> view lns2 (view lns1 s)),
    (fun y s -> set lns1 s (set lns2 (view lns1 s) y))

  let[@ocaml.inline] (^.) s lns =
    view lns s
  let (^>) = compose
end

type vv = { v : int }
and cc = { c : vv }
           [@@lens generate
                ~self_arg_first
                ~no_field_prefix]

let () =
  let open OUnit2 in
  let open Lens in
  run_test_tt_main
  @@ test_list
       [ "bounce_left_wall" >::
           begin fun ctxt ->
           Posn.(
             assert_equal ~ctxt
               { x = 2 ; y = 4 }
               (bounce_left_wall { x = -2 ; y = 4 }))
           end

       ; "_1" >::
           begin fun ctxt ->
           assert_equal ~ctxt 5 ((5, 3) ^. _1) ;
           assert_equal ~ctxt (false, 3) (set _1 (5, 3) false) ;
           end

       ; "thrush1" >::
           begin fun ctxt ->
           assert_equal ~ctxt 5 ({ c = { v = 5 } } ^. _c ^> _v) ;
           end

       ; "thrush2" >::
           begin fun ctxt ->
           let tup1 = (3, 5),   "a" in
           let tup2 = (3, "5"), "a" in
           assert_equal ~ctxt tup2
             (over (_1 ^> _2) tup1
                ~f:string_of_int) ;
           end ]
