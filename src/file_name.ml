include String

let invariant t =
  (not (String.is_empty t))
  && String.for_all t ~f:(fun c -> not (Char.equal c '/' || Char.equal c '\000'))
;;

let to_string t = t

let of_string s =
  if invariant s
  then Ok s
  else Or_error.error_s [%sexp "File_name.of_string: invalid file name", (s : string)]
;;

let v t = of_string t |> Or_error.ok_exn
let dot = v "."
let dot_dot = v ".."
let dot_git = v ".git"
let dot_hg = v ".hg"
