(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This library includes a wrapper over an abstract type which represents a
 * first-class continuation.
 * 
 * First-class continuations are used liberally throughout mulligan to be able
 * to facilitate a smooth interaction between the interactive top-level and the
 * code running the debugger. 
 * 
 * First-class continuations are notoriously tricky to work with, however, so 
 * we wrap it in an abstract type so that we can allow easy refactoring if we
 * want to augment these continuations with any other additional metadata.  
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature CONT =
  sig
    type 'a t

    val callcc : ('a t -> 'a) -> 'a

    val throw : 'a t -> 'a -> 'b

    val do_after : 'a t -> ('a -> 'a) -> 'a t

    val get_id : 'a t -> int
  end 

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Cont :> CONT
  =
  struct
    type 'a t = int * ('a -> 'a) * 'a MLton.Cont.t

    val counter = ref 0
    fun new () =
      ( counter := !counter + 1
      ; !counter
      )

    fun callcc f =
      let
        val num = new ()
      in
        MLton.Cont.callcc (fn cont =>
          f (num, fn x => x, cont)
        )
      end

    fun do_after ((i, _, cont) : 'a t) f = (i, f, cont)

    fun throw ((_, f, cont) : 'a t) v =
      MLton.Cont.throw (cont, f v)

    fun get_id ((i, _, _) : 'a t) = i
  end
