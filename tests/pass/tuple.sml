
val (x, y) = (1, "hi")

val (a, b) = (x + 3, y ^ " there")

val f = (fn (x, y) => (x orelse true, y / 1.0))

val (res1, res2) = f (false, 2.0)
