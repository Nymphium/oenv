open Shapes

let create ?(secret = true) shape name validate =
  conceal @@ field { shape = Shape shape; name; validate; secret }
;;

let string ?secret name = create ?secret String name Result.ok

let int ?secret name =
  let validate s =
    try Ok (int_of_string s) with
    | Failure _ -> Error (`Parse ("int", s))
  in
  create ?secret Int name validate
;;

let bool ?secret name =
  create ?secret Bool name @@ function
  | "true" -> Ok true
  | "false" -> Ok false
  | v -> Error (`Parse ("bool", v))
;;

let optional s =
  map s @@ function
  | Ok v -> Ok (Some v)
  | Error (`Missing _) -> Ok None
  | Error e -> Error e
;;

let default value opt =
  map opt @@ function
  | Ok (Some v) -> Ok v
  | Ok None | Error (`Missing _) -> Ok value
  | Error e -> Error e
;;

let custom validate ?secret name = create ?secret Custom name validate

let list ?secret ?(sep = ',') name validate =
  create ?secret Custom name @@ fun s ->
  if String.length s = 0
  then Ok []
  else (
    let items = String.split_on_char sep s in
    let[@tmc] rec aux acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs -> validate x |> Fun.flip Result.bind @@ fun v -> aux (v :: acc) xs
    in
    aux [] items)
;;
