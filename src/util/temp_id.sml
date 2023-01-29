(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure TempId :
  sig
    val new : unit -> Symbol.symbol
  end =
  struct
    val counter = ref 0

    fun new () =
      let
        val cur = !counter
      in
        counter := !counter + 1;
        Symbol.fromValue ("t" ^ Int.toString cur)
      end
  end
