open Lenslib
open Lenslib.Lens.Infix

module Posn = struct
  type t =
    { x : int
    ; y : int }
      [@@lens generate ~no_field_prefix]

  let bounce_left_wall : t -> t =
    update_x ~f:abs
end

type vv = { v : int }
and cc = { c : vv }
           [@@lens generate
                ~self_arg_first
                ~no_field_prefix]

let () =
  let open OUnit2 in
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
           assert_equal ~ctxt (false, 3) (Lens.set _1 (5, 3) false) ;
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
             (Lens.over (_1 ^> _2) tup1
                ~f:string_of_int) ;
           end ]
