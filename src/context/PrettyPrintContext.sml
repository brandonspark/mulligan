(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure PrettyPrintContext =
  struct
    type symbol = SMLSyntax.symbol

    datatype marker =
        MOD of symbol
      | VAL of symbol

    structure MarkerOrdered =
      struct
        type t = marker

        fun enum marker =
          case marker of
            MOD _ => 0
          | VAL _ => 1

        fun eq (m1, m2) =
          case (m1, m2) of
            (MOD s1, MOD s2) => Symbol.eq (s1, s2)
          | (VAL s1, VAL s2) => Symbol.eq (s1, s2)
          | _ => false

        fun compare (m1, m2) =
          case Int.compare (enum m1, enum m2) of
            LESS => LESS
          | GREATER => GREATER
          | EQUAL =>
              (case (m1, m2) of
                (MOD s1, MOD s2) => Symbol.compare (s1, s2)
              | (VAL s1, VAL s2) => Symbol.compare (s1, s2)
              | _ => raise Fail "shouldn't happen"
              )
      end

    structure MarkerSet = RedBlackSet (structure Elem = MarkerOrdered)

    type t = MarkerSet.set

    fun union_sets l =
      List.foldl
        (fn (set, acc) =>
          MarkerSet.union set acc
        )
        MarkerSet.empty
        l
  end
