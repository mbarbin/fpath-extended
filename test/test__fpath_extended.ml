let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fpath_extended) in
  Hashtbl.set t ~key:(Fpath.v "my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpath_extended).t)];
  [%expect {| ((my-file 42)) |}]
;;

module Fpath_pair : sig
  type t =
    { a : Fpath.t
    ; b : Fpath.t
    }
  [@@deriving compare, hash, sexp_of]
end = struct
  type t =
    { a : Fpath_extended.t
    ; b : Fpath_extended.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "hashtbl2" =
  let t = Hashtbl.create (module Fpath_pair) in
  Hashtbl.set t ~key:{ Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpath_pair).t)];
  [%expect {|
    ((
      ((a file-a)
       (b file-b))
      42)) |}]
;;
