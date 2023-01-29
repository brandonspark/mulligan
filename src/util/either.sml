(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure Either =
  struct
    datatype ('a, 'b) either = INL of 'a | INR of 'b
    datatype t = datatype either
  end
