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

type arg_order
  = Self_first
  | Self_last

type options =
  { o_prefix : prefix            (* prefix to all generated functions *)
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

  let _DEFAULT_PREFIX = Pre_from_type
  let _DEFAULT_ARG_ORDER = Self_last
  let _DEFAULT_GET_PREFIX = None
  let _DEFAULT_SET_PREFIX = "set"
  let _DEFAULT_UPD_PREFIX = "update"
  let _DEFAULT_UPD_NAMED = Some "f"

  let default =
    { o_prefix = _DEFAULT_PREFIX
    ; o_arg_order = _DEFAULT_ARG_ORDER
    ; o_get_prefix = _DEFAULT_GET_PREFIX
    ; o_set_prefix = _DEFAULT_SET_PREFIX
    ; o_upd_prefix = _DEFAULT_UPD_PREFIX
    ; o_upd_named = _DEFAULT_UPD_NAMED
    ; o_gen_getter = true
    ; o_gen_setter = true
    ; o_gen_updater = true
    }

  let update_from_attr o attribute =
    let { loc ; txt }, payload = attribute in
    let string_of_payload_opt () =
      match payload with
      | PStr [ { pstr_desc = Pstr_eval (e, []) } ] ->
         begin match e with
         | { pexp_desc = Pexp_constant (Pconst_string (s, None)) } ->
            Some s
         | _ -> None
         end
      | _ -> None
    in
    let string_of_payload () =
      match string_of_payload_opt () with
       | Some pre -> pre
       | _ -> raise Syntaxerr.(Error (Expecting (loc, "string constant")))
    in
    match txt with
    | "lens.no_get" -> { o with o_gen_getter = false }
    | "lens.no_set" -> { o with o_gen_setter = false }
    | "lens.no_update" -> { o with o_gen_updater = false }

    | "lens.no_prefix" -> { o with o_prefix = Pre_never }
    | "lens.prefix_from_type" -> { o with o_prefix = Pre_from_type }
    | "lens.prefix" -> { o with o_prefix = Pre_always (string_of_payload ()) }
    | "lens.no_get_prefix" -> { o with o_get_prefix = None }
    | "lens.get_prefix" -> { o with o_get_prefix = Some (string_of_payload ()) }
    | "lens.set_prefix" -> { o with o_set_prefix = string_of_payload () }
    | "lens.update_prefix" -> { o with o_upd_prefix = string_of_payload () }

    | "lens.self_arg_first" -> { o with o_arg_order = Self_first }
    | "lens.self_arg_last" -> { o with o_arg_order = Self_last }
    | "lens.named_arg" -> { o with o_upd_named = Some (string_of_payload ()) }
    | "lens.no_named_arg" -> { o with o_upd_named = None }

    | _ -> o

end

(****************************************)

let loc_lid ~loc s =
  Loc.make ~loc s,
  Loc.make ~loc (Longident.Lident s)

let gen_loc_lid ?(prefix="") ~loc =
  let tmp = gen_symbol ~prefix () in
  let loc, lid = loc_lid ~loc tmp in
  tmp, loc, lid

(** [[@@ocaml.inline]] attribute *)
let _INLINE_ATTR =
  let loc = Location.none in
  Loc.make ~loc "ocaml.inline", PStr []

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
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    Exp.(fun_ ~loc Nolabel None
           Pat.(record ~loc [ key, var tmp ] Open)
           Exp.(ident ~loc tmp_lid))

(** generate value binding for "setter" function, e.g.
    [let set_posn_x newval self = { self with x = newval }]. *)
let generate_setter_vb ~loc arg_order (name : str) (key : lid) =
  let _, self, self_lid = gen_loc_lid ~prefix:"self" ~loc in
  let _, newval, newval_lid = gen_loc_lid ~prefix:"newval" ~loc in
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
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
  Vb.mk ~loc ~attrs:[ _INLINE_ATTR ]
    Pat.(var name)
    (generate_self_fun ~loc arg_order label func self
       Exp.(record ~loc
              [ key, func_application ]
              (Some (ident ~loc self_lid))))

(** generate all lens functions for a type with the given field names. *)
let generate_lens_vbs ~options ~loc type_name field_idents =
  (* TODO: per-field attributes *)

  let prefixed specific_prefix name =
    let name =
      match options.o_prefix with
      | Pre_never -> name
      | Pre_always p -> p ^ "_" ^ name
      | Pre_from_type -> type_name ^ "_" ^ name
    in
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
     let options =
       List.fold_left
         Options.update_from_attr
         options
         ptype_attributes
     in
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
