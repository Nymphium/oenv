open Oenv

let () =
  Logs.(
    set_reporter @@ format_reporter ();
    set_level @@ Some Info);
  let var1 = "var1" in
  let var2 = "var2" in
  let getter = function
    | x when x = var1 -> Some "hello"
    | x when x = var2 -> Some "world"
    | _ -> None
  in
  let reader =
    Product.(v (fun l r -> l, r) +: string var1 +: string ~secret:false var2 |> close)
  in
  ignore @@ read_source reader getter
;;
