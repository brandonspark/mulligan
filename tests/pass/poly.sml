
val f = fn x => x

val x = f 1
val y = f "hi"

val x =
  fn f =>
    fn x =>
      fn y =>
        (f x, f y)

val a = (fn x => x + 1) (1, 2)
val b = f ("hi", "there")
