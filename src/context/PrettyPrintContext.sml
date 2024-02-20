(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The pretty-printer is contextual, meaning that it contains a lot of
 * identifiers which are populated by equipping our print functions
 * with a context of bindings.
 *
 * We call these "markers", which is a simple type which separates these
 * bindings by their namespaces (modules, values, etc).
 *
 * This is important because we need to know, when traveling out of the debugger's
 * specific one-hole context, which bindings start being and cease being valid.
 * We store that information in a data structure called a MarkerSet.
 *)

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

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
