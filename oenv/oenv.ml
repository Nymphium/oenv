include Shapes
include Std
module Errors = Errors
module Product = Product
module Logging = Logging

let%test_unit "read" =
  let var = "OENV_TEST_READ_STRING" in
  let getter = function
    | x when x = var -> Some "hello"
    | _ -> None
  in
  let r = read_source (string var) getter in
  let open Base in
  match r with
  | Ok v -> [%test_eq: string] v "hello"
  | Error _ -> failwith "test failed"
;;

let%expect_test "log" =
  let default_reporter = Logs.reporter () in
  let default_level = Logs.level () in
  Logs.(
    set_reporter @@ format_reporter ();
    set_level @@ Some Info);
  let var = "OENV_TEST_LOG" in
  let getter = function
    | x when x = var -> Some "hello"
    | _ -> None
  in
  let _ = read_source (string var) getter in
  [%expect {| inline-test-runner.exe: [INFO] Found OENV_TEST_LOG: <secret> |}];
  Logs.(
    set_reporter default_reporter;
    set_level default_level)
;;

let%test_unit "optional" =
  let var1 = "OENV_TEST_OPTIONAL1" in
  let var2 = "OENV_TEST_OPTIONAL2" in
  let getter = function
    | x when x = var2 -> Some "hello"
    | _ -> None
  in
  let r1 = read_source (optional (string var1)) getter in
  let r2 = read_source (optional (string var2)) getter in
  let open Base in
  match r1, r2 with
  | Ok v1, Ok v2 ->
    [%test_eq: string option] v1 None;
    [%test_eq: string option] v2 (Some "hello")
  | _ -> failwith "test failed"
;;

let%test_unit "custom" =
  let module M = struct
    type t =
      | A
      | B

    let show = function
      | A -> "A"
      | B -> "B"
    ;;

    let validate = function
      | "A" -> Ok A
      | "B" -> Ok B
      | s -> Error (`Parse ("A|B", s))
    ;;
  end
  in
  let var = "OENV_TEST_CUSTOM" in
  let getter = function
    | x when x = var -> Some "A"
    | _ -> None
  in
  let env = custom var M.validate in
  let r = read_source env getter in
  let open Base in
  match r with
  | Ok v -> [%test_eq: string] (M.show v) "A"
  | Error _ -> failwith "test failed"
;;

let%test_unit "list" =
  let module M = struct
    let show = String.concat "!"
    let validate = Result.ok
  end
  in
  let var = "OENV_TEST_LIST" in
  let getter = function
    | x when x = var -> Some "a,b,c"
    | _ -> None
  in
  let env = list var M.validate in
  let r = read_source env getter in
  let open Base in
  match r with
  | Ok v -> [%test_eq: string] (M.show v) "a!b!c"
  | Error _ -> failwith "test failed"
;;

let%test_unit "record" =
  let module M = struct
    type t =
      { host : string
      ; port : int
      ; user : string option
      }

    let show t =
      let u = Option.value ~default:"" t.user in
      Printf.sprintf "%s:%s@%d" u t.host t.port
    ;;

    let make host port user = { host; port; user }
  end
  in
  let var_host = "OENV_TEST_RECORD_HOST" in
  let var_port = "OENV_TEST_RECORD_PORT" in
  let var_user = "OENV_TEST_RECORD_USER" in
  let getter = function
    | x when x = var_host -> Some "host"
    | x when x = var_port -> Some "8888"
    | x when x = var_user -> Some "me"
    | _ -> None
  in
  let env =
    Product.(v M.make +: string var_host +: int var_port +: optional (string var_user))
    |> Product.close
  in
  let r = read_source env getter in
  let open Base in
  match r with
  | Ok v -> [%test_eq: string] (M.show v) "me:host@8888"
  | Error _ -> failwith "test failed"
;;
