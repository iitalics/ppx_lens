open Ppxlib
open Ast_helper

module Lid = struct
  include Longident
  let mk ~loc last = Loc.make ~loc (Lident last)
end

(****************************************)

type prefix
  = Pre_never
  | Pre_always of string
  | Pre_from_type

type arg_order
  = Self_first
  | Self_last

module Options = struct

  type t =
    { prefix : prefix            (* prefix to all generated functions *)
    ; get_prefix : string option (* prefix for getter funcs *)
    ; set_prefix : string        (* prefix for setter funcs *)
    ; set_arg_order : arg_order  (* order of args for setter func *)
    ; gen_setter : bool          (* generate setter function? *)
    ; upd_prefix : string        (* prefix for updater funcs *)
    ; upd_named : string option  (* named function argument to updater funcs *)
    ; gen_updater : bool         (* generate updater function? *)
    }

  let _DEFAULT_GET_PREFIX = None
  let _DEFAULT_SET_PREFIX = "set"
  let _DEFAULT_SET_ORDER = Self_last
  let _DEFAULT_UPD_PREFIX = "update"
  let _DEFAULT_UPD_NAMED = Some "f"

  let default =
    { prefix = Pre_from_type
    ; get_prefix = _DEFAULT_GET_PREFIX
    ; set_prefix = _DEFAULT_SET_PREFIX
    ; set_arg_order = _DEFAULT_SET_ORDER
    ; gen_setter = true
    ; upd_prefix = _DEFAULT_UPD_PREFIX
    ; upd_named = _DEFAULT_UPD_NAMED
    ; gen_updater = true
    }

end

(****************************************)

let loc_lid ~loc s =
  Loc.make ~loc s,
  Loc.make ~loc (Longident.Lident s)

let gen_loc_lid ?(prefix="") ~loc =
  let tmp = gen_symbol ~prefix () in
  let loc, lid = loc_lid ~loc tmp in
  tmp, loc, lid

(** generate [fun] expression, with "self" argument placed according to
    [arg_order]. *)
let generate_self_fun ~loc arg_order other_label (other : str) (self : str) body =
  let self_fn body =
    Exp.(fun_ ~loc Nolabel None
           Pat.(var ~loc self)
           body)
  in
  let other_fn body =
    Exp.(fun_ ~loc other_label None
           Pat.(var ~loc other)
           body)
  in
  match arg_order with
  | Self_first -> self_fn (other_fn body)
  | Self_last  -> other_fn (self_fn body)

(** generate value binding for "getter" function, e.g.
    [let posn_x { x } = x].  *)
let generate_getter_vb ~loc (name : str) (key : lid) =
  let _, tmp, tmp_lid = gen_loc_lid ~prefix:"tmp" ~loc in
  Vb.mk ~loc
    Pat.(var name)
    Exp.(fun_ ~loc Nolabel None
           Pat.(record ~loc [ key, var tmp ] Open)
           Exp.(ident ~loc tmp_lid))

(** generate value binding for "setter" function, e.g.
    [let set_posn_x newval self = { self with x = newval }]. *)
let generate_setter_vb ~loc arg_order (name : str) (key : lid) =
  let _, self, self_lid = gen_loc_lid ~prefix:"self" ~loc in
  let _, newval, newval_lid = gen_loc_lid ~prefix:"newval" ~loc in
  Vb.mk ~loc
    Pat.(var name)
    (generate_self_fun ~loc arg_order Nolabel newval self
       Exp.(record ~loc [ key, ident ~loc newval_lid ]
              (Some (ident ~loc self_lid))))

(** generate value inding for "update" function, e.g.
    [let update_posn_x ~f self = { p with x = f self.x }]. *)
let generate_update_vb ~loc arg_order label (name : str) (key : lid) =
  let _, self, self_lid = gen_loc_lid ~prefix:"self" ~loc in
  let _, func, func_lid = gen_loc_lid ~prefix:"func" ~loc in
  let func_application =
    Exp.(apply ~loc
           (ident ~loc func_lid)
           [ Nolabel, field ~loc (ident ~loc self_lid) key ])
  in
  Vb.mk ~loc
    Pat.(var name)
    (generate_self_fun ~loc arg_order label func self
       Exp.(record ~loc
              [ key, func_application ]
              (Some (ident ~loc self_lid))))
