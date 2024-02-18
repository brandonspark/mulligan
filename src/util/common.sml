
structure Common =
  struct
    fun println s = print (s ^ "\n")
    fun suspend x = fn () => x
  end