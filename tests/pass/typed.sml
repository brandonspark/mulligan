
val x : int = 1 + 2
val y : string = "hi"

val f = (fn (x : string, y : int) => (y, x))

val (a : int, b : string) = f (y, x)
