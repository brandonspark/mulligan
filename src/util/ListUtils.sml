(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Utility functions on lists.
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature LISTUTILS =
  sig
    val last : 'a list -> 'a
    val up_to_last : 'a list -> 'a list
    val map_last : ('a -> 'b) -> 'a list -> 'b
    val map_cons : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list
    val cons_rev : 'a list -> 'a -> 'a list

    val mapi : ('a * int -> 'b) -> 'a list -> 'b list

    val fold_with_tail : ('a * 'a list * 'b -> 'b) -> 'b -> 'a list -> 'b

    val flatten : 'a list list -> 'a list
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure ListUtils : LISTUTILS =
  struct
    fun last [] = raise Fail "finding last of empty list"
      | last l = List.nth (l, List.length l - 1)

    fun up_to_last l =
      List.take (l, List.length l - 1)

    fun map_last f l = f (last l)

    fun map_cons _ _ [] = []
      | map_cons f g (x::xs) = f x :: List.map g xs

    fun mapi f l =
      List.foldl
        (fn (x, (i, acc)) =>
          (i + 1, f (x, i) :: acc)
        )
        (0, [])
        l
      |> (fn l => List.rev (#2 l))

    fun fold_with_tail f z l =
      case l of
        [] => z
      | x::xs =>
          fold_with_tail f (f (x, xs, z)) xs

    fun cons_rev L x = x::L
    (* I could instead write `Fn.curry (Fn.flip op::)`, but then I get value
     * restricted. Sad! *)

    fun flatten [] = []
      | flatten ([]::xss) = flatten xss
      | flatten ((x::xs)::xss) = x::(flatten (xs::xss))
  end
