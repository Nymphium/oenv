open Alcotest

let var = "envvar1"
let errors = (module Oenv.Errors : TESTABLE with type t = Oenv.Errors.t)
let validated t = Alcotest.(result t errors)

let basic =
  let string =
    test_case "string" `Quick @@ fun () ->
    let open Oenv in
    let getter = function
      | x when x = var -> Some "hello"
      | _ -> None
    in
    let actual = read_source (string var) getter in
    Alcotest.(check' (result string errors) ~msg:"string" ~expected:(Ok "hello") ~actual)
  in
  let int =
    test_case "int" `Quick @@ fun () ->
    let open Oenv in
    let getter = function
      | x when x = var -> Some "3"
      | _ -> None
    in
    let actual = read_source (int var) getter in
    Alcotest.(check' (result int errors) ~msg:"int" ~expected:(Ok 3) ~actual)
  in
  let int_literal =
    test_case "int_literal" `Quick @@ fun () ->
    let open Oenv in
    let getter = function
      | x when x = var -> Some "3_000"
      | _ -> None
    in
    let actual = read_source (int var) getter in
    Alcotest.(check' (result int errors) ~msg:"int_literal" ~expected:(Ok 3000) ~actual)
  in
  let bool =
    test_case "bool" `Quick @@ fun () ->
    let open Oenv in
    let getter = function
      | x when x = var -> Some "true"
      | _ -> None
    in
    let actual = read_source (bool var) getter in
    Alcotest.(check' (result bool errors) ~msg:"bool" ~expected:(Ok true) ~actual)
  in
  let bool_fail =
    test_case "bool_fail" `Quick @@ fun () ->
    let open Oenv in
    let getter = function
      | x when x = var -> Some "True"
      | _ -> None
    in
    let actual = read_source (bool var) getter in
    check'
      (validated Alcotest.bool)
      ~msg:"bool_fail"
      ~expected:(Error (`Parse ("bool", "True")))
      ~actual
  in
  let optional =
    test_case "optional" `Quick @@ fun () ->
    let open Oenv in
    let var2 = "envvar2" in
    let getter = function
      | x when x = var2 -> Some "hello"
      | _ -> None
    in
    let r1 = read_source (optional (string var)) getter in
    let r2 = read_source (optional (string var2)) getter in
    let actual = r1, r2 in
    Alcotest.(
      check'
        (pair (validated (option string)) (validated (option string)))
        ~msg:"optional"
        ~expected:(Ok None, Ok (Some "hello"))
        ~actual)
  in
  let optional_default =
    test_case "optional_default" `Quick @@ fun () ->
    let open Oenv in
    let getter = Fun.const None in
    let actual = read_source (optional (string var) |> default "hello") getter in
    check' (validated Alcotest.string) ~msg:"optional" ~expected:(Ok "hello") ~actual
  in
  let custom =
    test_case "custom" `Quick @@ fun () ->
    let open Oenv in
    let module M = struct
      type t =
        | A
        | B
      [@@deriving show, eq]

      let validate = function
        | "A" -> Ok A
        | "B" -> Ok B
        | s -> Error (`Parse ("A|B", s))
      ;;
    end
    in
    let m = (module M : Alcotest.TESTABLE with type t = M.t) in
    let getter = function
      | x when x = var -> Some "A"
      | _ -> None
    in
    let env = custom M.validate var in
    let actual = read_source env getter in
    check' (validated m) ~msg:"custom" ~expected:(Ok M.A) ~actual
  in
  let list =
    let open Oenv in
    test_case "list" `Quick @@ fun () ->
    let module M = struct
      let validate = Result.ok
    end
    in
    let var = "OENV_TEST_LIST" in
    let getter = function
      | x when x = var -> Some "a,b,c"
      | _ -> None
    in
    let env = list var M.validate in
    let actual = read_source env getter in
    check'
      (validated Alcotest.(list string))
      ~msg:"list"
      ~expected:(Ok [ "a"; "b"; "c" ])
      ~actual
  in
  ( "basic"
  , [ string
    ; int
    ; int_literal
    ; bool
    ; bool_fail
    ; optional
    ; optional_default
    ; custom
    ; list
    ] )
;;

let record =
  let record =
    let open Oenv in
    test_case "record" `Quick @@ fun () ->
    let module M = struct
      type t =
        { host : string
        ; port : int
        ; user : string option
        }
      [@@deriving show, eq]

      let make host port user = { host; port; user }
    end
    in
    let m = (module M : Alcotest.TESTABLE with type t = M.t) in
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
    let actual = read_source env getter in
    check'
      (validated m)
      ~msg:"record"
      ~expected:(Ok (M.make "host" 8888 (Some "me")))
      ~actual
  in
  "record", [ record ]
;;

let () = run "oenv" [ basic; record ]
