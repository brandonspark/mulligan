
structure Common =
  struct
    fun println s = print (s ^ "\n")
    fun suspend x = fn () => x

    fun with_refval r v f =
      let
        val old_val = !r
        val () = r := v
        val res = f ()
        val () = r := old_val
      in
        res
      end
  end