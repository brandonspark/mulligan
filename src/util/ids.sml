(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unique identifiers which can be freshly generated to not clash with any
 * previously existing values.
 *
 * Because of generative functors, these can be freshly minted to create brand
 * new types which are statically distinct.
 *)

(*****************************************************************************)
(* Functor *)
(*****************************************************************************)

functor MkTemp (val prefix : string) :>
  sig
    type t
    type key = t

    val new : Symbol.symbol option -> t
    val eq : t * t -> bool
    val compare : t * t -> order

    val show : t -> string
  end =
  struct
    type t = int * Symbol.symbol option
    type key = t

    val counter = ref 0

    fun new opt =
      ( counter := !counter + 1
      ; (!counter, opt)
      )


    fun eq ((n, _), (n', _)) = n = n'

    fun compare ((n, _), (n', _)) = Int.compare (n, n')

    fun show (i, opt) =
      case opt of
        NONE => prefix ^ Int.toString i
      | SOME s => Symbol.toValue s
  end

(*****************************************************************************)
(* Identifiers *)
(*****************************************************************************)

structure TyId = MkTemp (val prefix = "ty")
structure ExnId = MkTemp (val prefix = "exn")
structure AbsId = MkTemp (val prefix = "abs")
structure ContId = MkTemp (val prefix = "cont")

(*****************************************************************************)
(* Dictionaries *)
(*****************************************************************************)

structure TyIdDict = RedBlackDict (structure Key = TyId)
structure AbsIdDict = RedBlackDict (structure Key = AbsId)
structure ContIdDict = RedBlackDict (structure Key = ContId)
