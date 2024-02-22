type absolute_path = Fpath.t
type relative_path = Fpath.t

let append a r = Fpath.(a // r) |> Fpath.normalize
let extend t f = Fpath.(t / File_name.to_string f) |> Fpath.normalize

let parent t =
  let t' = Fpath.normalize t |> Fpath.parent in
  if Fpath.equal t t' then None else Some t'
;;

let chop_prefix ~module_name ~prefix t =
  match Fpath.rem_prefix prefix t with
  | Some t -> Ok t
  | None ->
    if Fpath.equal prefix t
    then Ok Fpath.(v "./")
    else
      Or_error.error_s
        [%sexp
          (Printf.sprintf "%s.chop_prefix: not a prefix" module_name : string)
          , { prefix : Fpath0.t; t : Fpath0.t }]
;;

let chop_suffix ~module_name ~empty t ~suffix =
  let rec aux t suffix =
    match t, suffix with
    | _, [] -> Ok t
    | [], _ :: _ -> Error ()
    | hd :: t, hd2 :: suffix -> if String.equal hd hd2 then aux t suffix else Error ()
  in
  match aux (Fpath.segs t |> List.rev) (Fpath.segs suffix |> List.rev) with
  | Ok ([] | [ "" ]) -> Ok empty
  | Ok (_ :: _ as segs) ->
    let result = String.concat ~sep:Fpath.dir_sep (List.rev segs) in
    Ok (Fpath.v result)
  | Error () ->
    Or_error.error_s
      [%sexp
        (Printf.sprintf "%s.chop_suffix: not a suffix" module_name : string)
        , { t : Fpath0.t; suffix : Fpath0.t }]
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

  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path

  let relativize ~root f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then f else append root f
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
  let empty = Fpath.v "./"
  let append = append
  let extend = extend
  let parent = parent
  let of_list files = List.fold files ~init:empty ~f:extend
  let chop_prefix ~prefix t = chop_prefix ~module_name:"Relative_path" ~prefix t
  let chop_suffix t ~suffix = chop_suffix ~module_name:"Relative_path" ~empty t ~suffix
  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path
end

module Export = struct
  let classify f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then `Absolute f else `Relative f
  ;;
end
