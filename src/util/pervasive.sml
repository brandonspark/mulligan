(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

fun x |> f = f x

fun fst (x, _) = x
fun snd (_, y) = y 