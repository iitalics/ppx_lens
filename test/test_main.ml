
module Posn = struct
  type t =
    { x : int
    ; y : int }
      [@@lens.no_prefix]
      [@@lens.generate]

  let to_string { x ; y } =
    Printf.sprintf "{%d,%d}" x y

  let bounce_left_wall : t -> t =
    update_x ~f:abs

  (* *)

  let assert_ ~ctxt a b =
    OUnit2.assert_equal ~ctxt ~printer:to_string
      a b

  let test =
    let open OUnit2 in
    "Posn tests" >:::
      [ "bounce_left_wall" >::
          begin fun ctxt ->
          assert_ ~ctxt
            (bounce_left_wall { x = -2 ; y = 4 })
            { x = 2 ; y = 4 }
          end
      ]
end

type ('a, 'b, 'c) trip =
  { fst : 'a ; snd : 'b ; thd : 'c }
    [@@lens.prefix_from_type]
    [@@lens.update_prefix "modify"]
    [@@lens.no_named_arg]
    [@@lens.generate]

let foo : (int, 'a, 'b) trip -> (string, 'a, 'b) trip
  = modify_trip_fst string_of_int

let () =
  let open OUnit2 in
  run_test_tt_main
  @@ Posn.test
