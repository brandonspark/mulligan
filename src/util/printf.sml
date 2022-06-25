
infix fs fi fli ftv fe fv fp

signature PRINTF =
   sig
      type ('a, 'b, 'state) t
      val ` : string -> ('a, 'a, 'state) t
      val newFormat:
          ('state -> 'a -> string)
       -> ('a -> 'b, 'c, 'state) t * string
       -> ('b, 'c, 'state) t
      val printf: (string, 'a, unit) t -> 'a
      val cprintf: 'state -> (string, 'a, 'state) t -> 'a

      val fs : (string -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
      val fi : (Symbol.symbol -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
      val fli : (Symbol.symbol list -> 'a, 'b, 'state) t * string -> ('a, 'b, 'state) t
   end

structure Printf: PRINTF =
   struct
     datatype ('a, 'b, 'state) t =
       T of ('state * string -> 'a) -> 'state * string -> 'b

     fun cprintf state (T f) = f (fn (_, s) => s) (state, "")
     fun printf (T f) = f (fn (_, s) => s) ((), "")

     fun ` s = T (fn f => fn (state, s') => f (state, s' ^ s))

     fun newFormat toString (T f, s) =
       T (fn th =>
          f (fn (state, s') => fn a =>
              (th ( state, s' ^ toString state a ^ s )
              )
            )
         )

     fun promote f =
       fn ctx => fn x => f x

     structure TC = TerminalColors

     fun longid_to_str syms =
       String.concatWith "." (List.map Symbol.toValue syms)

     fun lightblue s = TC.foreground TC.lightblue ^ s ^ TC.reset

     val op fs = fn acc => newFormat (promote (fn s => s)) acc
     val op fi = fn acc => newFormat (promote (lightblue o Symbol.toValue)) acc
     val op fli = fn acc => newFormat (promote (lightblue o longid_to_str)) acc
   end


