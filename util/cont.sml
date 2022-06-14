
structure Cont :>
  sig
    type 'a t

    val callcc : ('a t -> 'a) -> 'a

    val throw : 'a t -> 'a -> 'b

    val get_id : 'a t -> int
  end =
  struct
    type 'a t = int * 'a MLton.Cont.t

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
          f (num, cont)
        )
      end

    fun throw (i, cont) v =
      ( print ("Throwing to cont " ^ Int.toString i ^ "\n")
      ; MLton.Cont.throw (cont, v)
      )

    fun get_id (i, _) = i
  end
