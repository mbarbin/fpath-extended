let%expect_test "of_string" =
  let test str =
    print_s [%sexp (Relative_path.of_string str : Relative_path.t Or_error.t)]
  in
  test "";
  [%expect {| (Error (Relative_path.of_string "\"\": invalid path")) |}];
  test ".";
  [%expect {| (Ok ./) |}];
  test "/a";
  [%expect {| (Error ("Relative_path.of_fpath: not a relative path" /a)) |}];
  test "a/b/../..";
  [%expect {| (Ok ./) |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Relative_path.of_fpath f in
    if Result.is_error t then print_s [%sexp (t : Relative_path.t Or_error.t)];
    Or_error.iter t ~f:(fun t ->
      print_endline (Relative_path.to_string t);
      let f' = Relative_path.to_fpath t in
      if Fpath.equal f f'
      then print_s [%sexp "does roundtrip", { f : Fpath.t }]
      else print_s [%sexp "does not roundtrip", { f : Fpath.t; f' : Fpath.t }])
  in
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
  test_fpath (Fpath.v "/an/absolute/path");
  [%expect
    {|
    (Error ("Relative_path.of_fpath: not a relative path" /an/absolute/path)) |}];
  ()
;;

let%expect_test "append" =
  let rel str = str |> Relative_path.v in
  let test a b = print_s [%sexp (Relative_path.append a b : Relative_path.t)] in
  test (rel "a/b") (rel "c/d");
  [%expect {| a/b/c/d |}];
  test (rel "a/b") (rel "c/d/");
  [%expect {| a/b/c/d/ |}];
  test (rel "a/b/") (rel "c/d");
  [%expect {| a/b/c/d |}];
  test (rel ".") (rel "a/b/c");
  [%expect {| a/b/c |}];
  test (rel "./a/b/../c/.") (rel "d/e");
  [%expect {| a/c/d/e |}];
  test (rel "./../b/d/../c/.") (rel "e/f");
  [%expect {| ../b/c/e/f |}];
  test (rel "./../../../b/d/../c/.") (rel "f/g");
  [%expect {| ../../../b/c/f/g |}];
  test (rel "x/y/z") (rel "../../a/b/c");
  [%expect {| x/a/b/c |}];
  test (rel "a/b/c") (rel "../../../x/y/z");
  [%expect {| x/y/z |}];
  test (rel "a/b/c") (rel "../../../../x/y/z");
  [%expect {| ../x/y/z |}];
  test (rel "a/b/c") (rel "../../../../../../x/y/z");
  [%expect {| ../../../x/y/z |}];
  ()
;;

let%expect_test "extend" =
  let rel str = str |> Relative_path.v in
  let file str = str |> File_name.v in
  let test a b = print_s [%sexp (Relative_path.extend a b : Relative_path.t)] in
  require_does_raise [%here] (fun () : File_name.t -> file "a/b");
  [%expect {| ("File_name.of_string: invalid file name" a/b) |}];
  require_does_not_raise [%here] (fun () -> ignore (file ".." : File_name.t));
  [%expect {| |}];
  test (rel ".") (file "a");
  [%expect {| a |}];
  test Relative_path.dot (file ".a");
  [%expect {| .a |}];
  test (rel "a") (file "b");
  [%expect {| a/b |}];
  test (rel "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a/b/") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a") (file "b");
  [%expect {| a/b |}];
  test (rel "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a/b") (file "c");
  [%expect {| a/b/c |}];
  test (rel "a/b/") (file "c");
  [%expect {| a/b/c |}];
  test (rel "a/b") (file "..");
  [%expect {| a/ |}];
  test (rel "a/bar/foo") (file "..");
  [%expect {| a/bar/ |}];
  ()
;;

let%expect_test "parent" =
  let rel str = str |> Relative_path.v in
  let test path =
    let result = Relative_path.parent path in
    print_s [%sexp (result : Relative_path.t option)]
  in
  test (rel "foo/bar");
  [%expect {| (foo/) |}];
  test (rel "foo/bar/");
  [%expect {| (foo/) |}];
  test (rel "foo");
  [%expect {| (./) |}];
  test (rel ".");
  [%expect {| () |}];
  test (rel "./");
  [%expect {| () |}];
  ()
;;

let%expect_test "of_list" =
  let test files =
    let result = Relative_path.of_list (List.map files ~f:File_name.v) in
    print_s [%sexp (result : Relative_path.t)]
  in
  test [];
  [%expect {| . |}];
  test [ "a" ];
  [%expect {| a |}];
  test [ "." ];
  [%expect {| ./ |}];
  test [ ".." ];
  [%expect {| ../ |}];
  test [ "a"; ".." ];
  [%expect {| ./ |}];
  test [ "a"; "." ];
  [%expect {| a/ |}];
  test [ "a"; "b"; ".."; "c" ];
  [%expect {| a/c |}];
  test [ "a"; "b"; "c"; "d" ];
  [%expect {| a/b/c/d |}];
  ()
;;

let%expect_test "chop_prefix" =
  let rel str = str |> Relative_path.v in
  let test prefix path =
    let result = Relative_path.chop_prefix ~prefix path in
    print_s [%sexp (result : Relative_path.t Or_error.t)]
  in
  test (rel "foo") (rel "foo/bar");
  [%expect {| (Ok bar) |}];
  test (rel "foo/") (rel "foo");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo/)
        (t      foo)))) |}];
  test (rel "foo") (rel "foo/bar/baz");
  [%expect {| (Ok bar/baz) |}];
  test (rel "foo") (rel "bar/baz");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo)
        (t      bar/baz)))) |}];
  test (rel "foo/bar") (rel "foo");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo/bar)
        (t      foo)))) |}];
  test (rel "foo/bar") (rel "foo");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo/bar)
        (t      foo)))) |}];
  test (rel "foo/bar") (rel "foo/bar/baz");
  [%expect {| (Ok baz) |}];
  test (rel "foo/bar") (rel "foo/bar/baz/qux");
  [%expect {| (Ok baz/qux) |}];
  (* Paths are normalized before the function call. *)
  test (rel "foo/bar") (rel "foo/bar/../baz");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo/bar)
        (t      foo/baz)))) |}];
  test (rel "foo/bar") (rel "foo/sna/../bar/baz");
  [%expect {|(Ok baz) |}];
  (* Beware of string prefix vs path prefix *)
  test (rel "foo/bar") (rel "foo/bar-baz");
  [%expect
    {|
    (Error (
      "Relative_path.chop_prefix: not a prefix" (
        (prefix foo/bar)
        (t      foo/bar-baz)))) |}];
  ()
;;

let%expect_test "chop_suffix" =
  let rel str = str |> Relative_path.v in
  let test path suffix =
    let result = Relative_path.chop_suffix path ~suffix in
    print_s [%sexp (result : Relative_path.t Or_error.t)]
  in
  test (rel "foo/bar") (rel "bar");
  [%expect {| (Ok foo) |}];
  test (rel "foo/bar") (rel "baz");
  [%expect
    {|
    (Error (
      "Relative_path.chop_suffix: not a suffix" (
        (t      foo/bar)
        (suffix baz)))) |}];
  test (rel "foo/bar/baz") (rel "bar/baz");
  [%expect {| (Ok foo) |}];
  test (rel "foo/bar/baz") (rel "baz/qux");
  [%expect
    {|
    (Error (
      "Relative_path.chop_suffix: not a suffix" (
        (t      foo/bar/baz)
        (suffix baz/qux)))) |}];
  test (rel "foo/bar/baz") (rel ".");
  [%expect
    {|
    (Error (
      "Relative_path.chop_suffix: not a suffix" (
        (t      foo/bar/baz)
        (suffix ./)))) |}];
  test (rel "foo/bar/baz") (rel "..");
  [%expect
    {|
    (Error (
      "Relative_path.chop_suffix: not a suffix" (
        (t      foo/bar/baz)
        (suffix ../)))) |}];
  test (rel "foo/bar/baz") (rel "foo/../baz");
  [%expect {| (Ok foo/bar) |}];
  (* Beware of string suffix vs path suffix *)
  test (rel "foo/bar-baz") (rel "-baz");
  [%expect
    {|
    (Error (
      "Relative_path.chop_suffix: not a suffix" (
        (t      foo/bar-baz)
        (suffix -baz)))) |}];
  ()
;;
