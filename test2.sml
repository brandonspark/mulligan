
(* val _ = (fn x => x) (fn y => y) (fn z => z)
val x = 2 + 3 + 5

val f = fn (x, y) => x

val _ =
  let
    val y = 1 * 3 * 2 * 4
    val z = x + x
    val x = x + y
  in
    f (x, y + 3)
  end
*)

val x = 1 + 2 + 3 + 4 + 5

local
  val x = 2
in
  val y = x + 3
end

val h = let val x = 2 in 2 end

val z = x + 2

fun h x = 1 * 2 + f 3
and f x = 1 + 4

val _ = h (1 + 2)

val x =
  ( 2 + 1
  ; 3 + 4 + 5
  ; true andalso false
  )

val _ = (raise Match) handle Match => (2 + 2)


(*
val _ = ( case SOME 5 of
            NONE => 3
          | SOME 5 => raise Div
 j      ) handle Bind => 150
 *)



(* val _ = f 5 *)

(*
val _ = if true andalso false then false orelse true else false

val _ = (1 + 2, true andalso true, (case 5 of 2 => 3 | _ => 1 + 1))

val _ =
  ( 1 + 2
  ; (3, (fn x => x) 2)
  ; (fn (x, y) => y) (true andalso false)
  )
 *)
