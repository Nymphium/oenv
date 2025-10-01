let () =
  ignore
  @@
  let open Let in
  Unix.putenv "DEBUG" "true";
  Unix.putenv "API_KEY" "kjhasdjhfkjhfadjh";
  let* debug = Oenv.(bool "DEBUG" |> read) in
  let* api_key = Oenv.(string "API_KEY" |> read) in
  Printf.printf "settigns: debug: %b, api_key: %s\n" debug api_key;
  Result.ok ()
;;
