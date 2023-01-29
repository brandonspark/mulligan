(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Generate fresh symbols.
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature FRESHSYM =
  sig
    val new : unit -> Symbol.symbol
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure FreshSym : FRESHSYM =
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
