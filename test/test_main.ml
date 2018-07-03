
module Posn = struct
  type t =
    { x : int
    ; y : int }
      [@@lens generate ~no_field_prefix ]

  let bounce_left_wall : t -> t =
    update_x ~f:abs
end

type ('a, 'b, 'c) trip =
  { fst : 'a ; snd : 'b ; thd : 'c }
    [@@lens generate
         ~no_update]

let ignore1 t = set_trip_fst () t

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

       ; "succ1" >::
           begin fun ctxt ->
           let p0 = { fst = 10 ; snd = 0 ; thd = 1 } in
           let p1 = { fst = () ; snd = 0 ; thd = 1 } in
           assert_equal ~ctxt
             p1
             (ignore1 p0)
           end ]
