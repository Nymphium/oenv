type config =
  { host : string
  ; port : int
  }

let show { host; port } = Printf.sprintf "%s:%d" host port
let make : string -> int -> config = fun host -> fun port -> { host; port }

let source = function
  | "HOST" -> Some "localhost"
  | "PORT" -> Some "8080"
  | _ -> None
;;

let () =
  ignore
  @@
  let open Let in
  let env = Oenv.(Product.(v make +: string "HOST" +: int "PORT" |> close)) in
  let* config = Oenv.read_source env source in
  print_endline @@ show config;
  Result.ok ()
;;
