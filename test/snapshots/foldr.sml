
fun foldr f z [] = z
  | foldr f z (x::xs) =
      f (x, foldr f z xs)

val _ = foldr op^ "" (["1", "5", "0"])