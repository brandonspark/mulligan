(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure CM_Token =
  struct
    type symbol = Symbol.symbol

    datatype elem =
      PATH of symbol
    | STRING of string

    datatype token =
      ELEM of elem

    | LIBRARY
    | GROUP
    | STRUCTURE
    | SIGNATURE
    | FUNCTOR
    | IDENT of symbol
    | IS

    | EOF

    fun insensitive_compare token1 token2 =
      case (token1, token2) of
        (ELEM (PATH sym1), ELEM (PATH sym2)) => Symbol.eq (sym1, sym2)

      | (ELEM (STRING s1), ELEM (STRING s2)) => s1 = s2

      | (STRUCTURE, STRUCTURE) => true
      | (SIGNATURE, SIGNATURE) => true
      | (FUNCTOR, FUNCTOR) => true
      | (IDENT s1, IDENT s2) => Symbol.toValue s1 = Symbol.toValue s2

      | (EOF, EOF) => true
      | _ => false

    fun elem_to_string elem =
      case elem of
        PATH node => Symbol.toValue node
      | STRING s => s

    fun token_to_string tok =
      case tok of
        ELEM elem => elem_to_string elem

      | LIBRARY => "Library"
      | GROUP => "Group"
      | STRUCTURE => "structure"
      | SIGNATURE => "signature"
      | FUNCTOR => "functor"
      | IDENT s => Symbol.toValue s
      | IS => "Is"

      | EOF => "eof"

  end
