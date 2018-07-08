open Lenslib.Infix
open Lenslib

module Posn = struct
  type t =
    { x : int
    ; y : int }
      [@@lens generate]

  let bounce_left_wall : t -> t =
    update_x ~f:abs
end

type player =
  { pl_pos : Posn.t
  ; pl_hp : int }
    [@@lens generate
         ~field_prefix:player]

let dmg ~by : player -> player =
  update_player_hp
    ~f:(fun hp -> hp - by)

let () =
  let open OUnit2 in
  run_test_tt_main
  @@ test_list
       [ "_1" >::
           begin fun ctxt ->
           assert_equal ~ctxt 5 ((5, 3) ^. _1) ;
           assert_equal ~ctxt (false, 3) (set _1 false (5, 3)) ;
           end

       ; "_id" >::
           begin fun ctxt ->
           assert_equal ~ctxt 5 (5 ^. _id) ;
           assert_equal ~ctxt 6 ((_id ^~ 6) 5) ;
           end

       ; "_hd, _tl" >::
           begin fun ctxt ->
           assert_equal ~ctxt 4 ([4;3;2] ^. _hd) ;
           assert_equal ~ctxt [4;2;1] (over _tl [4;3;2] ~f:(List.map pred)) ;
           try ignore ([] ^. _hd) ; assert_failure "(view _hd) did not raise exn"
           with (Match_failure _) -> () ;
           end

       ; "bounce_left_wall" >::
           begin fun ctxt ->
           Posn.(
             assert_equal ~ctxt
               { x = 2 ; y = 4 }
               (bounce_left_wall { x = -2 ; y = 4 }))
           end

       ; "dmg" >::
           begin fun ctxt ->
           let p1 = { pl_pos = { x = 3 ; y = 4 } ; pl_hp = 10 } in
           let p2 = { pl_pos = { x = 3 ; y = 4 } ; pl_hp = 8 } in
           assert_equal ~ctxt p2 (dmg ~by:2 p1) ;
           end

       ; "thrush1" >::
           begin fun ctxt ->
           let tup1 = (3, 5),   "a" in
           let tup2 = (3, "5"), "a" in
           assert_equal ~ctxt tup2
             (over (_1 ^> _2) tup1
                ~f:string_of_int) ;
           end

       ; "thrush2" >::
           begin fun ctxt ->
           let p1 = { pl_pos = { x = 0 ; y = 0 } ; pl_hp = 10 } in
           let p2 = { pl_pos = { x = 2 ; y = 0 } ; pl_hp = 10 } in
           assert_equal ~ctxt p2
             (set (_player_pos ^> Posn._x) 2 p1) ;
           end ]
