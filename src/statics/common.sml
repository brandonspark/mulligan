(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

fun union_three s1 s2 s3 =
  SymSet.union
    (SymSet.union s1 s2)
    s3

fun set_from_list l =
  List.foldl
    (fn (elem, acc) =>
      SymSet.insert acc elem
    )
    SymSet.empty
    l

fun union_sets l =
  List.foldl
    (fn (elem, acc) =>
      SymSet.union acc elem
    )
    SymSet.empty
    l
