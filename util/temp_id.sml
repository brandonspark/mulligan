
structure TempId :
  sig
    val new : unit -> SMLSyntax.symbol
  end =
  struct
    val counter = ref 0

    fun new () =
      let
        val cur = !counter
      in
        counter := !counter + 1;
        Symbol.fromValue ("t" ^ Int.toString cur)
      end


  end
