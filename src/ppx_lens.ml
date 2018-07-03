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
    }

  let update o =
    o

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
    Exp.(record ~loc [ key, ident ~loc newval_lid ]
           (Some (ident ~loc self_lid)))

let generate_setter_vb ~loc arg_order name key =
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_setter_fun ~loc arg_order key)

(** generate value inding for "update" function, e.g.
    [let update_posn_x ~f self = { p with x = f self.x }]. *)
let generate_update_fun ~loc arg_order label key =
  let self, self_lid = gen_symbol_loc_lid ~loc ~prefix:"self" in
  let func, func_lid = gen_symbol_loc_lid ~loc ~prefix:"func" in
  let func_application =
    Exp.(apply ~loc
           (ident ~loc func_lid)
           [ Nolabel, field ~loc (ident ~loc self_lid) key ])
  in
  Arg_order.fun2 ~loc arg_order label func self
    Exp.(record ~loc
           [ key, func_application ]
           (Some (ident ~loc self_lid)))

let generate_update_vb ~loc arg_order label name key =
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_update_fun ~loc arg_order label key)

(** generate all lens functions for a type with the given field names. *)
let generate_lens_vbs ~options ~loc type_name field_idents =
  (* TODO: per-field attributes *)

  let prefixed specific_prefix field_name =
    let name = Prefix.apply type_name options.o_field_prefix field_name in
    match specific_prefix with
    | Some p -> p ^ "_" ^ name
    | None -> name
  in

  let gen_get_vbs () =
    List.map (fun field_id ->
        let field_name = Lid.last_exn field_id in
        generate_getter_vb ~loc
          (Loc.make ~loc (prefixed options.o_get_prefix field_name))
          (Loc.make ~loc field_id))
      field_idents in

  let gen_set_vbs () =
    List.map (fun field_id ->
        let field_name = Lid.last_exn field_id in
        generate_setter_vb ~loc
          options.o_arg_order
          (Loc.make ~loc (prefixed (Some options.o_set_prefix) field_name))
          (Loc.make ~loc field_id))
      field_idents in

  let gen_upd_vbs () =
    List.map (fun field_id ->
        let field_name = Lid.last_exn field_id in
        let label = match options.o_upd_named with
          | None -> Nolabel
          | Some k -> Labelled k
        in
        generate_update_vb ~loc
          options.o_arg_order
          label
          (Loc.make ~loc (prefixed (Some options.o_upd_prefix) field_name))
          (Loc.make ~loc field_id))
      field_idents in

  (if options.o_gen_getter then gen_get_vbs () else [])
  @ (if options.o_gen_setter then gen_set_vbs () else [])
  @ (if options.o_gen_updater then gen_upd_vbs () else [])

(** generate list of [value_bindings]s for the given [type_declaration]. *)
let generate_vbs_from_type_decl ~options ~loc = function
  | { ptype_name
    ; ptype_loc
    ; ptype_attributes
    ; ptype_kind = Ptype_record lab_decls }
    ->
     (*
     let options =
       List.fold_left
         Options.update_from_attr
         options
         ptype_attributes
     in
      *)
     generate_lens_vbs ~options ~loc
       ptype_name.txt
       (List.map (fun { pld_name = { txt } } -> Lident txt)
          lab_decls)

  | _ ->
     raise Syntaxerr.(Error (Expecting (loc, "record type")))

let generate_str_items_from_type_decl ~options ~loc type_decl =
  Str.value ~loc Nonrecursive
    (generate_vbs_from_type_decl ~options ~loc type_decl)

(******************************)

let () =
  let options =
    Options.default
  in
  let lens_attr =
    Attribute.(
      declare "@lens.generate"
        Context.type_declaration
        Ast_pattern.(pstr nil)
        ())
  in
  let type_decl_rule =
    Context_free.Rule.attr_str_type_decl
      lens_attr
      (fun ~loc ~path rec_flag type_decls _ ->
        List.map (generate_str_items_from_type_decl ~options ~loc)
          type_decls)
  in
  Driver.register_transformation
    "ppx_lens"
    ~rules:[ type_decl_rule ]
