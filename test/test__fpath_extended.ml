let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fpath_extended) in
  Hashtbl.set t ~key:(Fpath.v "my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpath_extended).t)];
  [%expect {| ((my-file 42)) |}]
;;
