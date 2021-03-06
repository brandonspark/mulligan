
structure Cont :>
  sig
    type 'a t

    val callcc : ('a t -> 'a) -> 'a

    val throw : 'a t -> 'a -> 'b

    val do_after : 'a t -> ('a -> 'a) -> 'a t

    val get_id : 'a t -> int
  end =
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

    fun do_after (i, _, cont) f = (i, f, cont)

    fun throw (i, f, cont) v =
      MLton.Cont.throw (cont, f v)

    fun get_id (i, _, _) = i
  end
