# ppx_lens

`ppx_lens` is an OCaml PPX for generating "lens" operations on records, which
helps to facilitate functional updates on deeply nested data structures.

## Examples

Generate lens functions for a record:

```ocaml
# type glasses =
    { len : int;
      bridge : int;
      diam : int }
        [@@lens generate] ;;
type glasses = { len : int; bridge : int; diam : int; }
val len : glasses -> int = <fun>
val bridge : glasses -> int = <fun>
val diam : glasses -> int = <fun>
val set_len : int -> glasses -> glasses = <fun>
val set_bridge : int -> glasses -> glasses = <fun>
val set_diam : int -> glasses -> glasses = <fun>
val update_len : f:(int -> int) -> glasses -> glasses = <fun>
val update_bridge : f:(int -> int) -> glasses -> glasses = <fun>
val update_diam : f:(int -> int) -> glasses -> glasses = <fun>
val _len : (glasses -> int) * (int -> glasses -> glasses) = (<fun>, <fun>)
val _bridge : (glasses -> int) * (int -> glasses -> glasses) = (<fun>, <fun>)
val _diam : (glasses -> int) * (int -> glasses -> glasses) = (<fun>, <fun>)

# let g = set_diam 56 {len = 145; bridge = 17; diam = 54} ;;
val g : glasses = {len = 145; bridge = 17; diam = 56}
```

`[@@lens generate]` generates a few functions for each field of the record, specifically:

  - `FIELDNAME`: function for getting the value of a field
  - `set_FIELDNAME`: function for changing the value of a field
  - `update_FIELDNAME`: function for changing the value of a field with a function

Additionally it generates "lens" values (denoted with underscore prefix), which are just
pairs consisting of the getter and setter functions. See `lenslib` below for ways to
manipulate these values.

## Installation

`ppx_lens` is best installed with OPAM and dune.

```shell
$ git clone <url>/ppx_lens.git
$ opam pin add ppx_lens ppx_lens/
$ opam pin add lenslib ppx_lens/    # optional
```

**TODO:** publish to OPAM servers

dune (jbuilder) configuration:

```
(library
 ((name ...)
  ....
  (preprocess
   (pps (... ppx_lens ...)))))
```

## Configuration

The functions generated by `ppx_lens` can be configured by supplying named arguments.

**Usage:**

```ocaml
type t = { <record field> ... }
    [@@lens generate <parameter> ...]
```

Availabe parameters:

  - `~field_prefix:<prefix>`: change what prefix to place before the field names
  - `~field_prefix_from_type`: use the name of the type to determine the prefix
  - `~self_arg_first`: place the argument for the structure being modified
    _before_ the other arguments, in setter / updater functions. By default, the
    structure comes after the other argument.
  - `~get_prefix:<prefix>`: change what prefix to use for getter functions. By
    default, getter functions do not have a prefix.
  - `~set_prefix:<prefix>`: change what prefix to use for setter functions. By
    default, setter functions use the prefix `set`.
  - `~update_prefix:<prefix>`: change what prefix to use for updater functions. By
    default, updater functions use the prefix `update`.
  - `~func_named_arg:<name>`: change the named argument in the updater function. By
    default, the argument is named `f`.
  - `~func_no_named_arg`: don't name the argument in the updater function. The position
    of the argument may then be controlled with `~self_arg_first`.
  - `~no_get`: don't generate getter functions.
  - `~no_set`: don't generate setter functions.
  - `~no_update`: don't generate updater functions.
  - `~no_lens`: don't generate lens values.
  - `~just_lens`: don't generate getter / setter / updater functions, just lens values.

**TODO:** example here

# Lenslib

This repository contains a small library called `lenslib` with functions for manipulating
the "lens" values generated by `ppx_lens`. Note that `lenslib` is not a dependency for
`ppx_lens`.

```ocaml
sig
  type ('s, 't, 'a, 'b) lens = ('s -> 'a) * ('b -> 's -> 't)
  type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens

  val lens    : view:('s -> 'a) -> set:('b -> 's -> 't) -> ('s, 't, 'a, 'b) lens
  val view    : ('s, _ , 'a, _ ) lens -> ('s -> 'a)
  val set     : ('s, 't, _ , 'b) lens -> ('b -> 's -> 't)
  val over    : ('s, 't, 'a, 'b) lens -> f:('a -> 'b) -> ('s -> 't)
  val compose : ('ss, 'tt, 's, 't) lens -> ('s, 't, 'a, 'b) lens -> ('ss, 'tt, 'a, 'b) lens

  val _1  : ('x * 'a, 'y * 'a, 'x, 'y) lens
  val _2  : ('a * 'x, 'a * 'y, 'x, 'y) lens
  val _id : ('a, 'b, 'a, 'b) lens
  val _hd : ('a list, 'a) lens'
  val _tl : ('a list, 'a list) lens'

  module Infix :
    sig
      val ( ^. ) : 's -> ('s, _, 'a, _) lens -> 'a
      val ( ^~ ) : ('s, 't, _, 'b) lens -> ('b -> 's -> 't)
      val ( ^% ) : ('s, 't, 'a, 'b) lens -> ('a -> 'b) -> ('s -> 't)
      val ( ^> ) : ('ss, 'tt, 's, 't) lens -> ('s, 't, 'a, 'b) lens -> ('ss, 'tt, 'a, 'b) lens
      val ( ^< ) : ('s, 't, 'a, 'b) lens -> ('ss, 'tt, 's, 't) lens -> ('ss, 'tt, 'a, 'b) lens
    end
end
```

## Examples

```ocaml
# open Lenslib
# open Lenslib.Infix
# g ^. _bridge ;;
- : int = 17
# set (_2 ^> _len) 160 (true, {len = 150; bridge = 17; diam = 56}) ;;
- : bool * glasses = (true, {len = 160; bridge = 17; diam = 56})
```

**TODO:** create ocamldoc
