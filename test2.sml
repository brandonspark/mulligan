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

  factCPS (1 - 1) SOME

  ((fn n => fn k => ....) (1 - 1)) SOME
*)

val _ = Bar.y + 2

datatype tree = Empty | Node of tree * (string * int * tree) * bool
fun treeSum Empty = 0
  | treeSum (Node (t1, i, t2)) = treeSum t1 + i + treeSum t2

val _ = (treeSum Empty, 0 + 1)

val _ =
  treeSum
    (Node
       ( Node (Node (Empty, 3, Empty), 5, Empty)
       , 10
       , Node
           (Node (Empty, 2, Empty), 9, Node (Empty, 5, Node (Empty, 4, Empty)))
       ))

fun factCPS n k =
  case n of
    0 => k 1
  | _ => factCPS (n - 1) (fn res => k (res * n))

val _ = factCPS 2 SOME

val x = 1
structure Foo =
  struct
    val x = x + 1

    structure Bar =
      struct
        val x = x + 1

        val _ = x + x
      end

    val _ = x + Bar.x
  end

signature BAR =
  sig
    val y : int
  end

val _ = x + Foo.x + Foo.Bar.x

fun f x = 2 * x
and h x = 1 + f x

val _ = h 2 + h 4

val y = (2 + 1; 3 + 4 + 5; true andalso false)

val _ =
  (raise Match)
    handle
        Match =>
          (2 + 2)


          (*
          val _ = ( case SOME 5 of
                      NONE => 3
                    | SOME 5 => raise Div
           j      ) handle Bind => 150
           *)(* val _ = f 5 *)(*
                              val _ = if true andalso false then false orelse true else false

                              val _ = (1 + 2, true andalso true, (case 5 of 2 => 3 | _ => 1 + 1))

                              val _ =
                                ( 1 + 2
                                ; (3, (fn x => x) 2)
                                ; (fn (x, y) => y) (true andalso false)
                                )
                               *)
