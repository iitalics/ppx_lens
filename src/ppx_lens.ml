open Ppxlib

module Options = struct

  type prefix
    = Pre_never
    | Pre_always of string
    | Pre_from_type

  type t =
    { prefix : prefix           (* prefix to all generated functions *)
    ; set_prefix : string       (* prefix for setter funcs *)
    ; upd_prefix : string       (* prefix for updater funcs *)
    ; upd_named : string option (* named function argument to updater funcs *)
    ; gen_updater : bool        (* generate updater function? *)
    ; gen_setter : bool         (* generate setter function? *)
    }

  let _DEFAULT_SET_PREFIX = "set"
  let _DEFAULT_UPD_PREFIX = "update"
  let _DEFAULT_UPD_NAMED = Some "f"

  let default =
    { prefix = Pre_from_type
    ; set_prefix = _DEFAULT_SET_PREFIX
    ; upd_prefix = _DEFAULT_UPD_PREFIX
    ; upd_named = _DEFAULT_UPD_NAMED
    ; gen_updater = true
    ; gen_setter = true
    }

end
