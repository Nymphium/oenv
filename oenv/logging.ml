let src = Logs.Src.create "oenv"
let set_level = Logs.Src.set_level src

let found name v secret =
  Logs.info ~src @@ fun m ->
  m "Found %s: %s" name (if secret then "<secret>" else String.escaped v)
;;

let not_found name = Logs.info ~src @@ fun m -> m "Not found %s" name
