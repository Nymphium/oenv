let () =
  Logs.(
    set_reporter @@ format_reporter ();
    set_level @@ Some Info);
  ignore
  @@
  let open Let in
  Unix.putenv "DEBUG" "true";
  Unix.putenv "API_KEY" "kjhasdjhfkjhfadjh";
  let* _ = Oenv.(bool ~secret:false "DEBUG" |> read) in
  let* _ = Oenv.(string "API_KEY" |> read) in
  Result.ok ()
;;
