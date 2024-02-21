module T = struct
  type t = Fpath.t

  let compare = Fpath.compare
  let sexp_of_t t = Sexp.Atom (Fpath.to_string t)
  let hash_fold_t state t = String.hash_fold_t state (Fpath.to_string t)
  let hash t = Hash.of_fold hash_fold_t t
end

include T
include Comparable.Make (T)
