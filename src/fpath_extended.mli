(** Adding a few functions to FPath to use alongside Base.

    - sexp serializer
    - enough for compatibility with Base style containers, such as:

    {v
  let hashtbl = Hashtbl.create (module Fpath_extended) in
  ...
    v} *)

type t = Fpath.t [@@deriving compare, equal, hash, sexp_of]
