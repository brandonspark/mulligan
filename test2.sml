
val x = 2 + 3 + 5
val y = 11

val f = fn (x, y) => x

val _ =
  let
    val y = 1 * 3 * 2 * 4
    val z = x + x
    val x = x + y
  in
    f (x, y + 3)
  end

val _ = f (1 + 4 + x, 2 + 3 + 4)
