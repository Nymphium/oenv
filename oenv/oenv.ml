type getter = string -> string option

open struct
  module rec Field : sig
    type _ base =
      | String : string base
      | Int : int base
      | Bool : bool base
      | Custom : 'a base

    type ('k, 'a) shape =
      | Base : 'a base -> ('a, 'a) shape
      | Optional : 'a base -> ('a option, 'a) shape

    type ('k, 'a, 'err) t =
      { shape : ('k, 'a) shape
      ; validate : string -> ('a, 'err) result
      ; name : string
      }
      constraint 'err = [> Errors.t ]

    val read_source : ('k, 'a, 'err) t -> getter -> ('k, 'err) result
  end = struct
    type _ base =
      | String : string base
      | Int : int base
      | Bool : bool base
      | Custom : 'a base

    type ('k, 'a) shape =
      | Base : 'a base -> ('a, 'a) shape
      | Optional : 'a base -> ('a option, 'a) shape

    type ('k, 'a, 'err) t =
      { shape : ('k, 'a) shape
      ; validate : string -> ('a, 'err) result
      ; name : string
      }
      constraint 'err = [> Errors.t ]

    let read_source (type k a) (t : (k, a, 'err) t) getter : (k, 'err) result =
      let read_opt name validate =
        match getter name with
        | None -> Ok None
        | Some v -> Result.map Option.some (validate v)
      in
      match t.shape with
      | Optional _ -> read_opt t.name t.validate
      | Base _ ->
        read_opt t.name t.validate
        |> Result.(
             Fun.flip bind
             @@ (function
              | Some v -> Result.ok v
              | None -> Error (`Missing t.name)))
    ;;
  end

  and Schema : sig
    type ('a, 'err) t = getter -> ('a, 'err) result

    val field : ('k, 'a, 'err) Field.t -> ('k, 'err) t
  end = struct
    type ('a, 'err) t = getter -> ('a, 'err) result

    let field p getter = Field.read_source p getter
  end
end

type 'a validator = string -> ('a, Errors.t) Result.t
type ('a, 'err) schema = ('a, 'err) Schema.t
type _ t = S : ('a, 'err) schema * ('err -> Errors.t) -> 'a t

let conceal (s : ('a, [> Errors.t ]) schema) : 'a t =
  S (s, fun err -> (err :> [> Errors.t ]))
;;

let string name =
  conceal @@ Schema.field Field.{ shape = Base String; name; validate = Result.ok }
;;

let int name =
  let validate s =
    try Ok (int_of_string s) with
    | Failure _ -> Error (`Parse ("int", s))
  in
  conceal @@ Schema.field Field.{ shape = Base Int; name; validate }
;;

let bool name =
  let validate = function
    | "true" -> Ok true
    | "false" -> Ok false
    | v -> Error (`Parse ("bool", v))
  in
  conceal @@ Schema.field Field.{ shape = Base Bool; name; validate }
;;

let custom name validate =
  conceal @@ Schema.field Field.{ shape = Base Custom; name; validate }
;;

let list ?(sep = ',') name validate =
  let validate s =
    if String.length s = 0
    then Ok []
    else (
      let items = String.split_on_char sep s in
      let[@tmc] rec aux acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
          (match validate x with
           | Ok v -> aux (v :: acc) xs
           | Error e -> Error e)
      in
      aux [] items)
  in
  conceal @@ Schema.field Field.{ shape = Base Custom; name; validate }
;;

let optional s =
  let (S (s, upcast)) = s in
  conceal
  @@ fun getter ->
  s getter
  |> Result.map_error upcast
  |> function
  | Ok v -> Ok (Some v)
  | Error (`Missing _) -> Ok None
  | Error e -> Error e
;;

let default dv opt =
  let (S (opt, upcast)) = opt in
  conceal
  @@ fun getter ->
  opt getter
  |> Result.map_error upcast
  |> function
  | Ok (Some v) -> Ok v
  | Ok None | Error (`Missing _) -> Ok dv
  | Error e -> Error e
;;

module Record = struct
  type ('func, 'final) builder = 'func t

  let v x : ('func, 'final) builder = conceal @@ Fun.const @@ Ok x

  let apply pf px =
    fun getter ->
    match pf getter with
    | Error e -> Error e
    | Ok f ->
      (match px getter with
       | Error e -> Error e
       | Ok x -> Ok (f x))
  ;;

  let ( +: ) (b : ('a -> 'b, 'final) builder) (s : 'a t) : ('b, 'final) builder =
    let b =
      let (S (b, upcast)) = b in
      fun getter ->
        match b getter with
        | Error e -> Error (upcast e)
        | Ok v -> Ok v
    in
    let s =
      let (S (s, upcast')) = s in
      fun getter ->
        match s getter with
        | Error e -> Error (upcast' e)
        | Ok v -> Ok v
    in
    conceal @@ apply b s
  ;;

  let close : ('final, 'final) builder -> 'final t = Fun.id
end

let read_source s =
  let (S (s, upcast)) = s in
  fun getter -> s getter |> Result.map_error upcast
;;

let read s = read_source s Sys.getenv_opt

let%test_unit "read string" =
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
    Record.(v M.make +: string var_host +: int var_port +: optional (string var_user))
    |> Record.close
  in
  let r = read_source env getter in
  let open Base in
  match r with
  | Ok v -> [%test_eq: string] (M.show v) "me:host@8888"
  | Error _ -> failwith "test failed"
;;
