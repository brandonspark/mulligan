
functor MkTemp () :>
  sig
    type t
    type key = t

    val new : unit -> t
    val eq : t * t -> bool
    val compare : t * t -> order
  end =
  struct
    type t = int
    type key = t

    val counter = ref 0

    fun new () =
      ( counter := !counter + 1
      ; !counter
      )

    val eq = op=

    val compare = Int.compare
  end

structure TyId = MkTemp ()
structure ExnId = MkTemp ()
