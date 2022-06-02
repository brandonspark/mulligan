
structure Context :
  sig
    type t

    val enter_scope : string -> t -> t
    val add_signat : t -> SMLSyntax.signat -> t
  end =
  struct

  end
