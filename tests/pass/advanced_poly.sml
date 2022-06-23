
val 'a f =
  fn x : 'a => x : 'a

val a = f 1
val b = f "hi"

val ('a, 'b, 'c) g : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c =
  fn f : 'b -> 'c =>
    fn g : 'a -> 'b =>
      fn x : 'a =>
        f (g x)

val 'a x : ('a -> 'a) -> 'a -> 'a =
  fn f : 'a -> 'a =>
    let
      val y : 'b = raise Div
    in
      f : 'a -> 'a
    end
