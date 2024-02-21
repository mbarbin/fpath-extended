let%expect_test "of_string" =
  let test str =
    print_s [%sexp (Absolute_path.of_string str : Absolute_path.t Or_error.t)]
  in
  test "";
  [%expect {| (Error (Absolute_path.of_string "\"\": invalid path")) |}];
  test "/";
  [%expect {| (Ok /) |}];
  test "a";
  [%expect {| (Error ("Absolute_path.of_fpath: not an absolute path" a)) |}];
  test "/a/b/../..";
  [%expect {| (Ok /) |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Absolute_path.of_fpath f in
    if Result.is_error t then print_s [%sexp (t : Absolute_path.t Or_error.t)];
    Or_error.iter t ~f:(fun t ->
      print_endline (Absolute_path.to_string t);
      let f' = Absolute_path.to_fpath t in
      if Fpath.equal f f'
      then print_s [%sexp "does roundtrip", { f : Fpath.t }]
      else print_s [%sexp "does not roundtrip", { f : Fpath.t; f' : Fpath.t }])
  in
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
  test_fpath (Fpath.v "a/relative/path");
  [%expect {| (Error ("Absolute_path.of_fpath: not an absolute path" a/relative/path)) |}];
  require_does_raise [%here] (fun () -> test_fpath (Fpath.v ""));
  [%expect {| (Invalid_argument "\"\": invalid path") |}];
  ()
;;

let%expect_test "append" =
  let abs str = str |> Absolute_path.v in
  let rel str = str |> Relative_path.v in
  let test a b = print_s [%sexp (Absolute_path.append a b : Absolute_path.t)] in
  test (abs "/a") (rel "b");
  [%expect {| /a/b |}];
  test (abs "/a") (rel "b/");
  [%expect {| /a/b/ |}];
  test (abs "/a") (rel ".");
  [%expect {| /a/ |}];
  test (abs "/a/") (rel ".");
  [%expect {| /a/ |}];
  test (abs "/a") (rel "./");
  [%expect {| /a/ |}];
  test (abs "/") (rel "a/b");
  [%expect {| /a/b |}];
  test Absolute_path.root (rel ".");
  [%expect {| / |}];
  test Absolute_path.root (rel "./a/b/c");
  [%expect {| /a/b/c |}];
  test (abs "/") (rel "./a/b/../c/.");
  [%expect {| /a/c/ |}];
  test (abs "/a/c") (rel "./../b/d/../c/.");
  [%expect {| /a/b/c/ |}];
  test (abs "/a/c") (rel "./../../../b/d/../c/.");
  [%expect {| /b/c/ |}];
  ()
;;

let%expect_test "extend" =
  let abs str = str |> Absolute_path.v in
  let file str = str |> File_name.v in
  let test a b = print_s [%sexp (Absolute_path.extend a b : Absolute_path.t)] in
  require_does_raise [%here] (fun () : File_name.t -> file "a/b");
  [%expect {| ("File_name.of_string: invalid file name" a/b) |}];
  require_does_not_raise [%here] (fun () -> ignore (file ".." : File_name.t));
  [%expect {||}];
  test (abs "/") (file "a");
  [%expect {| /a |}];
  test Absolute_path.root (file "..");
  [%expect {| / |}];
  test Absolute_path.root (file ".");
  [%expect {| / |}];
  test Absolute_path.root (file ".a");
  [%expect {| /.a |}];
  test (abs "/a") (file "b");
  [%expect {| /a/b |}];
  test (abs "/a/b") (file ".");
  [%expect {| /a/b/ |}];
  test (abs "/a/b") (file "c");
  [%expect {| /a/b/c |}];
  test (abs "/a/b") (file "..");
  [%expect {| /a/ |}];
  test (abs "/a/bar/foo") (file "..");
  [%expect {| /a/bar/ |}];
  ()
;;

let%expect_test "parent" =
  let abs str = str |> Absolute_path.v in
  let test path =
    let result = Absolute_path.parent path in
    print_s [%sexp (result : Absolute_path.t option)]
  in
  test (abs "/foo/bar");
  [%expect {| (/foo/) |}];
  test (abs "/foo/bar/");
  [%expect {| (/foo/) |}];
  test (abs "/foo/bar/../baz/../foo/");
  [%expect {| (/foo/) |}];
  test (abs "/foo");
  [%expect {| (/) |}];
  test (abs "/");
  [%expect {| () |}];
  test (abs "/.");
  [%expect {| () |}];
  ()
;;

let%expect_test "chop_prefix" =
  let abs str = str |> Absolute_path.v in
  let test prefix path =
    let result = Absolute_path.chop_prefix ~prefix path in
    print_s [%sexp (result : Relative_path.t Or_error.t)]
  in
  test (abs "/foo") (abs "/foo/bar");
  [%expect {| (Ok bar) |}];
  test (abs "/foo/") (abs "/foo/bar");
  [%expect {| (Ok bar) |}];
  test (abs "/foo/") (abs "/foo/");
  [%expect {| (Ok .) |}];
  test (abs "/foo") (abs "/foo/");
  [%expect {| (Ok ./) |}];
  test (abs "/foo") (abs "/foo");
  [%expect {| (Ok .) |}];
  test (abs "/foo/") (abs "/foo");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_prefix: not a prefix" (
        (prefix /foo/)
        (t      /foo)))) |}];
  test (abs "/foo") (abs "/foo/bar/baz");
  [%expect {| (Ok bar/baz) |}];
  test (abs "/foo") (abs "/bar/baz");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_prefix: not a prefix" (
        (prefix /foo)
        (t      /bar/baz)))) |}];
  test (abs "/foo/bar") (abs "/foo");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_prefix: not a prefix" (
        (prefix /foo/bar)
        (t      /foo)))) |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz");
  [%expect {| (Ok baz) |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz/qux");
  [%expect {| (Ok baz/qux) |}];
  (* Paths are normalized before the function call. *)
  test (abs "/foo/bar") (abs "/foo/bar/../baz");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_prefix: not a prefix" (
        (prefix /foo/bar)
        (t      /foo/baz)))) |}];
  test (abs "/foo/bar") (abs "/foo/sna/../bar/baz");
  [%expect {|(Ok baz) |}];
  (* Beware of string prefix vs path prefix *)
  test (abs "/foo/bar") (abs "/foo/bar-baz");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_prefix: not a prefix" (
        (prefix /foo/bar)
        (t      /foo/bar-baz)))) |}];
  ()
;;

let%expect_test "chop_suffix" =
  let abs str = str |> Absolute_path.v in
  let rel str = str |> Relative_path.v in
  let test path suffix =
    let result = Absolute_path.chop_suffix path ~suffix in
    print_s [%sexp (result : Absolute_path.t Or_error.t)]
  in
  test (abs "/foo/bar") (rel "bar");
  [%expect {| (Ok /foo) |}];
  test (abs "/foo/bar") (rel "bar/");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar)
        (suffix bar/)))) |}];
  test (abs "/foo/bar/") (rel "bar");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/)
        (suffix bar)))) |}];
  test (abs "/foo/bar/") (rel "bar/");
  [%expect {| (Ok /foo) |}];
  test (abs "/foo/bar") (rel "baz");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar)
        (suffix baz)))) |}];
  test (abs "/foo/bar/baz") (rel "bar/baz");
  [%expect {| (Ok /foo) |}];
  test (abs "/foo/bar/baz") (rel "baz/qux");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz)
        (suffix baz/qux)))) |}];
  test (abs "/foo/bar/baz") (rel ".");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz)
        (suffix ./)))) |}];
  test (abs "/foo/bar/baz/") (rel ".");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz/)
        (suffix ./)))) |}];
  test (abs "/foo/bar/baz") (rel "./");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz)
        (suffix ./)))) |}];
  test (abs "/foo/bar/baz/") (rel "./");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz/)
        (suffix ./)))) |}];
  test (abs "/foo/bar/baz") (rel "..");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar/baz)
        (suffix ../)))) |}];
  test (abs "/foo/bar/baz") (rel "foo/../baz");
  [%expect {| (Ok /foo/bar) |}];
  (* Beware of string suffix vs path suffix *)
  test (abs "/foo/bar-baz") (rel "-baz");
  [%expect
    {|
    (Error (
      "Absolute_path.chop_suffix: not a suffix" (
        (t      /foo/bar-baz)
        (suffix -baz)))) |}];
  ()
;;
