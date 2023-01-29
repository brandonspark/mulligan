(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A wrapper around the `ref` type, with some additional helpful helper
 * functions.
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature REF =
  sig
    type 'a t

    val new : 'a -> 'a t
    val assign : 'a t -> 'a -> unit
    val force : 'a t -> 'a

    val compare : 'a t * 'a t -> order
    val eq : 'a t * 'a t -> bool

    val show : 'a t -> string
  end

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

structure Ref : REF =
  struct
    type 'a t = int * 'a ref

    val counter = ref 0
    fun new x =
      ( counter := !counter + 1
      ; (!counter, ref x)
      )
    fun assign (_, r) x = r := x
    fun force (_, r) = !r

    fun compare ((i1, _), (i2, _)) = Int.compare (i1, i2)
    fun eq (r1, r2) =
      case compare (r1, r2) of
        EQUAL => true
      | _ => false

    fun show (i, _) = "t" ^ Int.toString i
  end
