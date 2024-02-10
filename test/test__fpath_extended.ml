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

let%expect_test "compare" =
  let test a b =
    print_s
      [%sexp
        (a : Fpath_pair.t)
        , (Fpath_pair.compare a b |> Ordering.of_int : Ordering.t)
        , (b : Fpath_pair.t)]
  in
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-a)
      (b file-b))
     Equal
     ((a file-a)
      (b file-b))) |}];
  let t = { Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } in
  test t t;
  [%expect
    {|
    (((a file-a)
      (b file-b))
     Equal
     ((a file-a)
      (b file-b))) |}];
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-a)
      (b file-a))
     Less
     ((a file-a)
      (b file-b))) |}];
  test
    { a = Fpath.v "file-b"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-b)
      (b file-a))
     Greater
     ((a file-a)
      (b file-b))) |}];
  ()
;;

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
