
fun f 0 = 1 div 0
  | f n = 1 + f (n - 1)

fun g 0 = 1 mod 0
  | g n = 1 + g (n - 1)


val _ = SOME (f 4) handle Div => NONE

val _ = SOME (g 4) handle Div => NONE