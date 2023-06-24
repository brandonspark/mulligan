(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* IO helper functions.
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature IO_SIG =
  sig
    val cat : string -> string list
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure IO : IO_SIG =
  struct
    fun cat filename =
      let
        val instream = TextIO.openIn filename

        fun read_lines () =
          case TextIO.inputLine instream of
            NONE => []
          | SOME line => line :: read_lines ()
      in
        read_lines ()
      end
  end
