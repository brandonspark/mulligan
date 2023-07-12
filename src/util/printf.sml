(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

signature PRINTF =
  sig
    type ('a, 'b, 'state) t

    val ` : string -> ('a, 'a, 'state) t
    val newFormat:
        ('state -> 'a -> string)
      -> ('a -> 'b, 'c, 'state) t * string
      -> ('b, 'c, 'state) t

    val spf: (string, 'a, unit) t -> 'a
    val cprintf: 'state -> (string, 'a, 'state) t -> 'a

    val fs : (string -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
    val fi : (Symbol.symbol -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
    val fli : (Symbol.symbol list -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
   end

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

structure Printf : PRINTF =
  struct
    datatype ('a, 'b, 'state) t =
      T of ('state * string -> 'a) -> 'state * string -> 'b

    fun cprintf state (T f) = f (fn (_, s) => s) (state, "")
    fun spf (T f) = f (fn (_, s) => s) ((), "")

    fun ` s = T (fn f => fn (state, s') => f (state, s' ^ s))

    fun newFormat toString (T f, s) =
      T (fn th =>
        f (fn (state, s') => fn a =>
            (th ( state, s' ^ toString state a ^ s )
            )
          )
        )

    fun promote f =
      (* unused context parameter *)
      fn _ => f

    structure TC = TerminalColors

    fun longid_to_str syms =
      String.concatWith "." (List.map Symbol.toValue syms)

    fun lightblue s = TC.foreground TC.lightblue ^ s ^ TC.reset

    val fs = fn acc => newFormat (promote (fn s => s)) acc
    val fi = fn acc => newFormat (promote (lightblue o Symbol.toValue)) acc
    val fli = fn acc => newFormat (promote (lightblue o longid_to_str)) acc
  end
