
(* does not compile, cannot generalize
fun f x =
  let
    val y : 'a = x
  in
    x
  end *)

(* does compile
fun f (x : 'a) =
  let
    val y : 'a = x
  in
    x
  end *)

(* does compile *)
fun ('a, 'b) f x y =
  let
    val x1 = x
    val y1 = y
  in
    x
  end
