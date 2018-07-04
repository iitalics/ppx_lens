open Ppxlib
open Ast_helper

module List = struct
  include List

  let rec flat_map ~f = function
    | [] -> []
    | x :: xs -> f x @ flat_map ~f xs
end

module Lid = struct
  include Longident
  let mk ~loc last = Loc.make ~loc (Lident last)
end

(****************************************)

type prefix
  = Pre_never
  | Pre_always of string
  | Pre_from_type

module Prefix = struct
  type t = prefix
  let apply type_name pr field_name =
    match pr with
    | Pre_never -> field_name
    | Pre_always s -> s ^ "_" ^ field_name
    | Pre_from_type -> type_name ^ "_" ^ field_name
end

(* *)

type arg_order = Self_first | Self_last

module Arg_order = struct
  type t = arg_order

  (** generate [fun] expression, with "self" argument placed according to
    [arg_order]. *)
  let fun2 ~loc arg_order other_label (other : str) (self : str) body =
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

end

(* *)

type options =
  { o_field_prefix : prefix       (* prefix for field name *)
  ; o_arg_order : arg_order      (* order of self arg *)
  ; o_get_prefix : string option (* prefix for getter funcs *)
  ; o_set_prefix : string        (* prefix for setter funcs *)
  ; o_upd_prefix : string        (* prefix for updater funcs *)
  ; o_upd_named : string option  (* named function argument to updater funcs *)
  ; o_gen_getter : bool          (* generate getter function? *)
  ; o_gen_setter : bool          (* generate setter function? *)
  ; o_gen_updater : bool         (* generate updater function? *)
  ; o_gen_lenses : bool          (* generate lens values? *)
  }

module Options = struct
  type t = options

  let default =
    { o_field_prefix = Pre_from_type
    ; o_arg_order = Self_last
    ; o_get_prefix = None
    ; o_set_prefix = "set"
    ; o_upd_prefix = "update"
    ; o_upd_named = Some "f"
    ; o_gen_getter = true
    ; o_gen_setter = true
    ; o_gen_updater = true
    ; o_gen_lenses = true
    }

  let update_from_arg (label, expr) (o : options) =
    let loc = expr.pexp_loc in
    let expr_ident () =
      match expr with
      | { pexp_desc = Pexp_ident { txt = Lident x } } ->
         x
      | _ ->
         raise Syntaxerr.(Error (Expecting (loc, "identifier")))
    in
    match label with
    | Labelled "field_prefix" -> { o with o_field_prefix = Pre_always (expr_ident ()) }
    | Labelled "no_field_prefix" -> { o with o_field_prefix = Pre_never }
    | Labelled "self_arg_first" -> { o with o_arg_order = Self_first }

    | Labelled "get_prefix" -> { o with o_get_prefix = Some (expr_ident ()) }
    | Labelled "set_prefix" -> { o with o_set_prefix = expr_ident () }
    | Labelled "update_prefix" -> { o with o_upd_prefix = expr_ident () }

    | Labelled "func_named_arg" -> { o with o_upd_named = Some (expr_ident ()) }
    | Labelled "func_no_named_arg" -> { o with o_upd_named = None }

    | Labelled "no_get" -> { o with o_gen_getter = false }
    | Labelled "no_set" -> { o with o_gen_setter = false }
    | Labelled "no_update" -> { o with o_gen_updater = false }
    | Labelled "no_lens" -> { o with o_gen_lenses = false }
    | Labelled "just_lens" -> { o with o_gen_getter = false
                                     ; o_gen_setter = false
                                     ; o_gen_updater = false }

    | _ ->
       raise Syntaxerr.(Error (Ill_formed_ast (loc, "unknown lens configuration")))

end

(****************************************)

let gen_symbol_loc_lid ?(prefix="") ~loc =
  let tmp = gen_symbol ~prefix () in
  Loc.make ~loc tmp,
  Loc.make ~loc (Longident.Lident tmp)

(** [[@@ocaml.inline]] attribute *)
let _INLINE_ATTR =
  let loc = Location.none in
  Loc.make ~loc "ocaml.inline", PStr []

(** [[@@ocaml.warning "-23"]] attribute;
    disables warnings arising from:
      { x with k = v } when x has only one field
 *)
let _NO_WARNING_ATTR =
  let loc = Location.none in
  Loc.make ~loc "ocaml.warning",
  PStr [ Str.eval (Exp.constant (Const.string "-23")) ]

(** generate "getter" lambda function:
    [fun { key } -> key]. *)
let generate_getter_fun ~loc key =
  let tmp, tmp_lid = gen_symbol_loc_lid ~loc ~prefix:"tmp" in
    Exp.(fun_ ~loc Nolabel None
           Pat.(record ~loc [ key, var tmp ] Open)
           Exp.(ident ~loc tmp_lid))

let generate_getter_vb ~loc name key =
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_getter_fun ~loc key)

(** generate "setter" lambda function:
    [fun newval self -> { self with key = newval }]. *)
let generate_setter_fun ~loc arg_order key =
  let self, self_lid = gen_symbol_loc_lid ~loc ~prefix:"self" in
  let newval, newval_lid = gen_symbol_loc_lid ~loc ~prefix:"newval" in
  Arg_order.fun2 ~loc arg_order Nolabel newval self
    Exp.(record ~loc ~attrs:[ _NO_WARNING_ATTR ]
           [ key, ident ~loc newval_lid ]
           (Some (ident ~loc self_lid)))

let generate_setter_vb ~loc arg_order name key =
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_setter_fun ~loc arg_order key)

(** generate "updater" lambda function:
    [fun ~f self -> { self with key = f self.key }]. *)
let generate_update_fun ~loc arg_order label key =
  let self, self_lid = gen_symbol_loc_lid ~loc ~prefix:"self" in
  let func, func_lid = gen_symbol_loc_lid ~loc ~prefix:"func" in
  let func_application =
    Exp.(apply ~loc
           (ident ~loc func_lid)
           [ Nolabel, field ~loc (ident ~loc self_lid) key ])
  in
  Arg_order.fun2 ~loc arg_order label func self
    Exp.(record ~loc ~attrs:[ _NO_WARNING_ATTR ]
           [ key, func_application ]
           (Some (ident ~loc self_lid)))

let generate_update_vb ~loc arg_order label name key =
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_update_fun ~loc arg_order label key)

(** generate lens expression (paired encoding):
    [<getter>, <setter>]. *)
let generate_lens_expr ~loc key =
  Exp.tuple ~loc
    [ generate_getter_fun ~loc key
    ; generate_setter_fun ~loc Self_last key ]

let generate_lens_vb ~loc name key =
  Vb.mk ~loc
    Pat.(var name)
    (generate_lens_expr ~loc key)

(******************************)

(** generate all lens bindings for a type with the given field names. *)
let generate_all_vbs ~options ~loc type_name field_idents =

  let field_names =
    List.map Lid.last_exn field_idents
  in

  let prefixed specific_prefix field_name =
    let name = Prefix.apply type_name options.o_field_prefix field_name in
    match specific_prefix with
    | Some p -> p ^ "_" ^ name
    | None -> name
  in

  let gen_get_vbs () =
    List.map2 (fun field_id field_name ->
        generate_getter_vb ~loc
          (Loc.make ~loc (prefixed options.o_get_prefix field_name))
          (Loc.make ~loc field_id))
      field_idents
      field_names in

  let gen_set_vbs () =
    List.map2 (fun field_id field_name ->
        generate_setter_vb ~loc
          options.o_arg_order
          (Loc.make ~loc (prefixed (Some options.o_set_prefix) field_name))
          (Loc.make ~loc field_id))
      field_idents
      field_names in

  let gen_upd_vbs () =
    List.map2 (fun field_id field_name ->
        let label = match options.o_upd_named with
          | None -> Nolabel
          | Some k -> Labelled k
        in
        generate_update_vb ~loc
          options.o_arg_order
          label
          (Loc.make ~loc (prefixed (Some options.o_upd_prefix) field_name))
          (Loc.make ~loc field_id))
      field_idents field_names in

  let gen_lens_vbs () =
    List.map2 (fun field_id field_name ->
        generate_lens_vb ~loc
          (Loc.make ~loc (prefixed (Some "") field_name))
          (Loc.make ~loc field_id))
      field_idents
      field_names in

  (if options.o_gen_getter then gen_get_vbs () else [])
  @ (if options.o_gen_setter then gen_set_vbs () else [])
  @ (if options.o_gen_updater then gen_upd_vbs () else [])
  @ (if options.o_gen_lenses then gen_lens_vbs () else [])

let type_decl_field_idents = function
  | { ptype_name
    ; ptype_loc
    ; ptype_attributes
    ; ptype_kind = Ptype_record lab_decls } ->
     List.map (fun { pld_name = { txt } } -> Lident txt)
       lab_decls

  | { ptype_loc } -> raise Syntaxerr.(Error (Expecting (ptype_loc, "record type")))

let generate_vbs_from_type_decl ~options ~loc type_decl =
  let type_name = type_decl.ptype_name.txt in
  generate_all_vbs ~options ~loc type_name
    (type_decl_field_idents type_decl)

let generate_str_item_from_type_decl ~options ~loc type_decl =
  Str.value ~loc Nonrecursive
    (generate_vbs_from_type_decl ~options ~loc type_decl)

(******************************)

module Ast_pattern = struct
  include Ast_pattern
  let pexp_apply_ident ident : (expression, _, _) t =
    let ident_pat () = pexp_ident (lident (string ident)) in
    map (alt_option
           (pexp_apply (ident_pat ()) __)
           (ident_pat ()))
      ~f:(fun f -> function
        | None   -> f []
        | Some y -> f y)
end

let lens_generate_attr_rule =
  let module A = Attribute in
  let open Ast_pattern in
  (* [@lens generate <args ...>] *)
  Context_free.Rule.attr_str_type_decl
    (A.declare "lens"
       A.Context.type_declaration
       (pstr (pstr_eval (pexp_apply_ident "generate") __ ^:: nil))
       (fun args _ ->
         List.fold_right
           Options.update_from_arg
           args))
    (fun ~loc ~path _ type_decls option_updaters ->
      let update_options o =
        List.fold_left (fun o -> function
                      | Some f -> f o
                      | None -> o)
          o
          option_updaters
      in
      let options = update_options Options.default in
      List.map (generate_str_item_from_type_decl ~options ~loc)
        type_decls)

let () =
  Driver.register_transformation
    "ppx_lens"
    ~rules: [ lens_generate_attr_rule ]
