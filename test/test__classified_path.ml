let%expect_test "of_string" =
  let test str =
    print_s [%sexp (Classified_path.of_string str : Classified_path.t Or_error.t)]
  in
  test "";
  [%expect {| (Error (Classified_path.of_string "\"\": invalid path")) |}];
  test ".";
  [%expect {| (Ok (Relative ./)) |}];
  test "/";
  [%expect {| (Ok (Absolute /)) |}];
  test "/a";
  [%expect {| (Ok (Absolute /a)) |}];
  test "a";
  [%expect {| (Ok (Relative a)) |}];
  test "a/b/../..";
  [%expect {| (Ok (Relative ./)) |}];
  test "/a/b/../..";
  [%expect {| (Ok (Absolute /)) |}];
  ()
;;

module Hide_error = struct
  type 'a t = 'a Or_error.t

  let sexp_of_t sexp_of_a : _ t -> Sexp.t = function
    | Ok a -> [%sexp Ok (a : a)]
    | Error _ -> [%sexp Error "_"]
  ;;
end

let%expect_test "chop_prefix and chop_suffix" =
  List.iter
    ~f:(fun (a, b) ->
      let a = Classified_path.of_fpath (Fpath.v a) in
      let b = Classified_path.of_fpath (Fpath.v b) in
      match a, b with
      | Absolute a, Absolute b ->
        print_s
          [%sexp
            (a : Absolute_path.t)
            , (b : Absolute_path.t)
            , "==> chop_prefix"
            , (Absolute_path.chop_prefix ~prefix:a b : Relative_path.t Hide_error.t)]
      | Relative _, Absolute _ -> assert false
      | Absolute a, Relative b ->
        print_s
          [%sexp
            (a : Absolute_path.t)
            , (b : Relative_path.t)
            , "==> chop_suffix"
            , (Absolute_path.chop_suffix a ~suffix:b : Absolute_path.t Hide_error.t)]
      | Relative a, Relative b ->
        print_s
          [%sexp
            (a : Relative_path.t)
            , (b : Relative_path.t)
            , "==> chop_prefix"
            , (Relative_path.chop_prefix ~prefix:a b : Relative_path.t Hide_error.t)])
    [ "/", "."
    ; "/", "a"
    ; "/a", "a"
    ; "/a", "b"
    ; "/a/b", "b"
    ; "/a/b", "a/b"
    ; "/", "/"
    ; "/", "/a"
    ; "/a", "/"
    ; "/a/b", "/a/b/c"
    ; "/a/b", "/a/bc"
    ; "/a/b", "/a/b/c/d"
    ; "/a/b/c", "/a/b/c"
    ; "/a/b/c", "/a/b"
    ; ".", "."
    ; ".", "a"
    ; "a", "."
    ; "a/b", "a/b/c"
    ; "a/b", "a/bc"
    ; "a/b", "a/b/c/d"
    ; "a/b/c", "a/b/c"
    ; "a/b/c", "a/b"
    ];
  [%expect
    {|
    (/ ./ "==> chop_suffix" (Error _))
    (/ a "==> chop_suffix" (Error _))
    (/a a "==> chop_suffix" (Ok /))
    (/a b "==> chop_suffix" (Error _))
    (/a/b b "==> chop_suffix" (Ok /a))
    (/a/b a/b "==> chop_suffix" (Ok /))
    (/ / "==> chop_prefix" (Ok .))
    (/ /a "==> chop_prefix" (Ok a))
    (/a / "==> chop_prefix" (Error _))
    (/a/b /a/b/c "==> chop_prefix" (Ok c))
    (/a/b /a/bc "==> chop_prefix" (Error _))
    (/a/b /a/b/c/d "==> chop_prefix" (Ok c/d))
    (/a/b/c /a/b/c "==> chop_prefix" (Ok .))
    (/a/b/c /a/b "==> chop_prefix" (Error _))
    (./ ./ "==> chop_prefix" (Ok .))
    (./ a "==> chop_prefix" (Error _))
    (a ./ "==> chop_prefix" (Error _))
    (a/b a/b/c "==> chop_prefix" (Ok c))
    (a/b a/bc "==> chop_prefix" (Error _))
    (a/b a/b/c/d "==> chop_prefix" (Ok c/d))
    (a/b/c a/b/c "==> chop_prefix" (Ok .))
    (a/b/c a/b "==> chop_prefix" (Error _)) |}];
  ()
;;

let%expect_test "v" =
  require_does_raise [%here] (fun () -> Classified_path.v "");
  [%expect {| (Classified_path.of_string "\"\": invalid path") |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Classified_path.of_fpath f in
    print_endline (Classified_path.to_string t);
    let f' = Classified_path.to_fpath t in
    if Fpath.equal f f'
    then print_s [%sexp "does roundtrip", { f : Fpath.t }]
    else print_s [%sexp "does not roundtrip", { f : Fpath.t; f' : Fpath.t }]
  in
  (* Absolute paths. *)
  test_fpath (Fpath.v "/foo/bar");
  [%expect {|
    /foo/bar
    ("does roundtrip" ((f /foo/bar))) |}];
  test_fpath (Fpath.v "/foo/bar/");
  [%expect {|
      /foo/bar/
      ("does roundtrip" ((f /foo/bar/))) |}];
  test_fpath (Fpath.v "/");
  [%expect {|
    /
    ("does roundtrip" ((f /))) |}];
  test_fpath (Fpath.v "/.");
  [%expect {|
      /
      ("does not roundtrip" (
        (f  /.)
        (f' /))) |}];
  test_fpath (Fpath.v "/an/claolute/path");
  [%expect {|
    /an/claolute/path
    ("does roundtrip" ((f /an/claolute/path))) |}];
  (* Relative paths. *)
  test_fpath (Fpath.v "a/relative/path");
  [%expect {|
    a/relative/path
    ("does roundtrip" ((f a/relative/path))) |}];
  test_fpath (Fpath.v "foo/bar");
  [%expect {|
    foo/bar
    ("does roundtrip" ((f foo/bar))) |}];
  test_fpath (Fpath.v "foo/bar/");
  [%expect {|
      foo/bar/
      ("does roundtrip" ((f foo/bar/))) |}];
  test_fpath (Fpath.v ".");
  [%expect {|
    ./
    ("does not roundtrip" (
      (f  .)
      (f' ./))) |}];
  test_fpath (Fpath.v "./");
  [%expect {|
    ./
    ("does roundtrip" ((f ./))) |}];
  require_does_raise [%here] (fun () -> Fpath.v "");
  [%expect {| (Invalid_argument "\"\": invalid path") |}];
  ()
;;

let%expect_test "append" =
  let v str = str |> Classified_path.v in
  let test a b =
    print_endline (Classified_path.append a b |> Classified_path.to_string)
  in
  test (v "/") (v "a/b");
  [%expect {| /a/b |}];
  test Classified_path.root (v ".");
  [%expect {| / |}];
  test Classified_path.root (v "./a/b/c");
  [%expect {| /a/b/c |}];
  test (v "/") (v "./a/b/../c/.");
  [%expect {| /a/c/ |}];
  test (v "/a/c") (v "./../b/d/../c/.");
  [%expect {| /a/b/c/ |}];
  test (v "/a/c") (v "./../../../b/d/../c/.");
  [%expect {| /b/c/ |}];
  test (v "a/b") (v "c/d");
  [%expect {| a/b/c/d |}];
  test (v ".") (v "a/b/c");
  [%expect {| a/b/c |}];
  test (v "./a/b/../c/.") (v "d/e");
  [%expect {| a/c/d/e |}];
  test (v "./../b/d/../c/.") (v "e/f");
  [%expect {| ../b/c/e/f |}];
  test (v "./../../../b/d/../c/.") (v "f/g");
  [%expect {| ../../../b/c/f/g |}];
  test (v "x/y/z") (v "../../a/b/c");
  [%expect {| x/a/b/c |}];
  test (v "a/b/c") (v "../../../x/y/z");
  [%expect {| x/y/z |}];
  test (v "a/b/c") (v "../../../../x/y/z");
  [%expect {| ../x/y/z |}];
  test (v "a/b/c") (v "../../../../../../x/y/z");
  [%expect {| ../../../x/y/z |}];
  test (v "/a/b/c") (v "/a/foo/bar");
  [%expect {| /a/foo/bar |}];
  test (v "a/b/c") (v "/a/foo/bar");
  [%expect {| /a/foo/bar |}];
  ()
;;

let%expect_test "extend" =
  let v str = str |> Classified_path.v in
  let file str = str |> File_name.v in
  let test a b =
    print_endline (Classified_path.extend a b |> Classified_path.to_string)
  in
  require_does_raise [%here] (fun () : File_name.t -> file "a/b");
  [%expect {| ("File_name.of_string: invalid file name" a/b) |}];
  require_does_not_raise [%here] (fun () -> ignore (file ".." : File_name.t));
  [%expect {| |}];
  test (v "/") (file "a");
  [%expect {| /a |}];
  test Classified_path.root (file "..");
  [%expect {| / |}];
  test Classified_path.root (file ".");
  [%expect {| / |}];
  test Classified_path.root (file ".a");
  [%expect {| /.a |}];
  test (v "/a") (file "b");
  [%expect {| /a/b |}];
  test (v "/a/b") (file ".");
  [%expect {| /a/b/ |}];
  test (v "/a/b") (file "c");
  [%expect {| /a/b/c |}];
  test (v "/a/b") (file "..");
  [%expect {| /a/ |}];
  test (v "/a/bar/foo") (file "..");
  [%expect {| /a/bar/ |}];
  test (v ".") (file "a");
  [%expect {| a |}];
  test Classified_path.dot (file ".a");
  [%expect {| .a |}];
  test (v "a") (file "b");
  [%expect {| a/b |}];
  test (v "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (v "a") (file "b");
  [%expect {| a/b |}];
  test (v "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (v "a/b") (file "c");
  [%expect {| a/b/c |}];
  test (v "a/b") (file "..");
  [%expect {| a/ |}];
  test (v "a/bar/foo") (file "..");
  [%expect {| a/bar/ |}];
  ()
;;

let%expect_test "to_absolute_path" =
  let v str = str |> Classified_path.v in
  let abs str = str |> Absolute_path.v in
  let test a b =
    print_endline
      (Classified_path.to_absolute_path a ~base_path:b |> Absolute_path.to_string)
  in
  test (v "/") (abs "/a/b");
  [%expect {| / |}];
  test (v ".") (abs "/a/b");
  [%expect {| /a/b/ |}];
  test (v "../foo/bar") (abs "/a/b");
  [%expect {| /a/foo/bar |}];
  test (v "../foo/bar") (abs "/");
  [%expect {| /foo/bar |}];
  ()
;;

let%expect_test "parent" =
  let cla str = str |> Classified_path.v in
  let test path =
    let result = Classified_path.parent path in
    print_s [%sexp (result : Classified_path.t option)]
  in
  test (cla "/foo/bar");
  [%expect {| ((Absolute /foo/)) |}];
  test (cla "/foo/bar/");
  [%expect {| ((Absolute /foo/)) |}];
  test (cla "/foo/bar/../baz/../foo/");
  [%expect {| ((Absolute /foo/)) |}];
  test (cla "/foo");
  [%expect {| ((Absolute /)) |}];
  test (cla "/");
  [%expect {| () |}];
  test (cla "/.");
  [%expect {| () |}];
  test (cla "foo/bar");
  [%expect {| ((Relative foo/)) |}];
  test (cla "foo/bar/");
  [%expect {| ((Relative foo/)) |}];
  test (cla "foo");
  [%expect {| ((Relative ./)) |}];
  test (cla ".");
  [%expect {| () |}];
  test (cla "./");
  [%expect {| () |}];
  ()
;;
