open Shapes

type ('func, 'final) builder = 'func t

let v x : ('func, 'final) builder = conceal @@ Fun.const @@ Ok x

let ( +: ) (b : ('a -> 'b, 'final) builder) (s : 'a t) : ('b, 'final) builder =
  let open Result in
  conceal @@ fun source ->
  Shapes.value b source |> Fun.flip bind @@ fun f -> Shapes.value s source |> map f
;;

let close : ('final, 'final) builder -> 'final t = Fun.id
