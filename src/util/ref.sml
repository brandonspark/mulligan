(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure Ref :
  sig
    type 'a t

    val new : 'a -> 'a t
    val assign : 'a t -> 'a -> unit
    val force : 'a t -> 'a

    val compare : 'a t * 'a t -> order
    val eq : 'a t * 'a t -> bool

    val print : 'a t -> string
  end =
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

    fun print (i, _) = "t" ^ Int.toString i
  end
