(** Adding a few functions to {!module:Fpath} to use alongside Base.

    - sexp serializer
    - enough for compatibility with Base style containers, such as:

    {[
      let create_fpath_table () = Hashtbl.create (module Fpath)
    ]} *)

type t = Fpath.t [@@deriving compare, equal, hash, sexp_of]

include Comparable.S with type t := t
