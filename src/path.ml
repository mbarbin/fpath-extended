type absolute_path = Fpath.t
type relative_path = Fpath.t

let append a r = Fpath.(a // r) |> Fpath.normalize
let extend t f = Fpath.(t / File_name.to_string f) |> Fpath.normalize

let parent t =
  let p, _ = Fpath.split_base t in
  let t' = Fpath.normalize p in
  if Fpath.equal t t' then None else Some t'
;;

let chop_prefix ~module_name ~prefix t =
  match Fpath.rem_prefix prefix t with
  | Some t -> Ok t
  | None ->
    if Fpath.equal prefix t
    then Ok Fpath.(v ".")
    else
      Or_error.error_s
        [%sexp
          (Printf.sprintf "%s.chop_prefix: not a prefix" module_name : string)
          , { prefix : Fpath0.t; t : Fpath0.t }]
;;

let chop_suffix ~module_name ~empty t ~suffix =
  let suffix = Fpath.to_string suffix in
  match String.chop_suffix (Fpath.to_string t) ~suffix:(Fpath.dir_sep ^ suffix) with
  | Some t -> Ok (if String.is_empty t then empty else t |> Fpath.v)
  | None ->
    Or_error.error_s
      [%sexp
        (Printf.sprintf "%s.chop_suffix: not a suffix" module_name : string)
        , { t : Fpath0.t; suffix : string }]
;;

module Absolute_path = struct
  include Fpath0

  let to_fpath t = t
  let to_string = Fpath.to_string

  let of_fpath f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f
    then Ok f
    else
      Or_error.error_s
        [%sexp "Absolute_path.of_fpath: not an absolute path", (f : Fpath0.t)]
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg m) -> Or_error.error_s [%sexp "Absolute_path.of_string", (m : string)]
    | Ok f -> of_fpath f
  ;;

  let v str = str |> of_string |> Or_error.ok_exn
  let root = Fpath.v Fpath.dir_sep
  let append = append
  let extend = extend
  let parent = parent
  let chop_prefix ~prefix t = chop_prefix ~module_name:"Absolute_path" ~prefix t

  let chop_suffix t ~suffix =
    chop_suffix ~module_name:"Absolute_path" ~empty:root t ~suffix
  ;;
end

module Relative_path = struct
  include Fpath0

  let to_fpath t = t
  let to_string = Fpath.to_string

  let of_fpath f =
    let f = Fpath.normalize f in
    if Fpath.is_rel f
    then Ok f
    else
      Or_error.error_s
        [%sexp "Relative_path.of_fpath: not a relative path", (f : Fpath0.t)]
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg m) -> Or_error.error_s [%sexp "Relative_path.of_string", (m : string)]
    | Ok f -> of_fpath f
  ;;

  let v str = str |> of_string |> Or_error.ok_exn
  let dot = Fpath.v "."
  let dot_slash = Fpath.v "./"
  let append = append
  let extend = extend
  let parent = parent
  let of_list files = List.fold files ~init:dot ~f:extend
  let chop_prefix ~prefix t = chop_prefix ~module_name:"Relative_path" ~prefix t

  let chop_suffix t ~suffix =
    chop_suffix ~module_name:"Relative_path" ~empty:dot t ~suffix
  ;;
end

module Classified_path = struct
  module T = struct
    [@@@coverage off]

    type t =
      | Absolute of Absolute_path.t
      | Relative of Relative_path.t
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparable.Make (T)

  let to_fpath = function
    | Absolute a -> a
    | Relative r -> r
  ;;

  let root = Absolute Absolute_path.root
  let dot = Relative Relative_path.dot

  let of_fpath f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then Absolute f else Relative f
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg m) -> Or_error.error_s [%sexp "Classified_path.of_string", (m : string)]
    | Ok f -> Ok (of_fpath f)
  ;;

  let v str = str |> of_string |> Or_error.ok_exn
  let to_string t = t |> to_fpath |> Fpath.to_string

  let append p1 p2 =
    match p2 with
    | Absolute _ -> p2
    | Relative r2 ->
      (match p1 with
       | Relative r1 -> Relative (Relative_path.append r1 r2)
       | Absolute a1 -> Absolute (Absolute_path.append a1 r2))
  ;;

  let extend p f =
    match p with
    | Absolute p -> Absolute (Absolute_path.extend p f)
    | Relative r -> Relative (Relative_path.extend r f)
  ;;

  let parent = function
    | Absolute p -> Option.map (Absolute_path.parent p) ~f:(fun p -> Absolute p)
    | Relative r -> Option.map (Relative_path.parent r) ~f:(fun r -> Relative r)
  ;;

  let to_absolute_path t ~base_path =
    match t with
    | Relative rel -> Absolute_path.append base_path rel
    | Absolute abs -> abs
  ;;
end
