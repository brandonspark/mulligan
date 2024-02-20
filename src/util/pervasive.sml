(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Top-level globally-available values.
 *
 * This file should contain only things which are frequently used in many
 * places, which we are OK with polluting the namespace with.
 *)

(*****************************************************************************)
(* Global helpers *)
(*****************************************************************************)

(* So we have slightly more precedence than `<|`.
 *)
infix 1 |>
fun x |> f = f x

fun fst (x, _) = x
fun snd (_, y) = y

(* A typical use case is something like:
 *
 * print <| spf (`"a number is "fs"\n.") (Int.toString i)
 *
 * With low precedence, we won't have to put parentheses, and the `print`
 * is understood to go last.
 *)
infixr 0 <|
fun f <| x = f x

fun ignore _ = ()

(* a unit test!!!! *)
val _ : string = (fn _ => "") <| 2 |> (fn _ => 4)

fun orange s = TerminalColors.text TerminalColors.orange s
fun red s = TerminalColors.text TerminalColors.red s
fun lightblue s = TerminalColors.text TerminalColors.lightblue s

fun opt_all l =
  List.foldr (fn (x, acc) =>
    case (x, acc) of
      (_, NONE) => NONE
    | (NONE, _) => NONE
    | (SOME x, SOME acc) => SOME (x :: acc)
  ) (SOME []) l

fun push x r = r := x :: (!r)

(* returns absolute paths *)
fun files_of_directory path =
  let
    val stream = OS.FileSys.openDir path
    val files : string list ref = ref []

    fun aux () =
      case OS.FileSys.readDir stream of
        NONE => ()
      | SOME file =>
          ( aux ()
          ; push (OS.Path.joinDirFile {dir = path, file = file}) files
          )
  in
    ( aux ();
      !files
    )
  end

fun file_exists path = Posix.FileSys.access (path, [])

datatype either = datatype Either.t

infix fd fs fi fli ftv fe fv fp fl
open Printf

(* TODO: a thought of "modifiers"?
 * lots of patterns atm are smth like
 *
 * spf ... (lightblue s)
 *
 * kinda annoying to write `(lightblue s)` so many times though,
 * esp since the `lightblue` is the first thing you see.
 *
 * maybe make this a postfix thing, could be...
 *
 * print <| spf (`"The rain in "fs" falls on the "fs"")
              (s1 st [lightblue])
 *
 * understood as "s1 such that it is lightblue"
 * can change to something that isn't `st` if it's tough to read though
 *)

fun with_time_str f =
  let
    val timer = Timer.startRealTimer ()
    val res = f ()
    val elapsed_time =
      Time.toReal (Timer.checkRealTimer timer)
      |> Real.fromLarge IEEEReal.TO_NEAREST

    val time_str = Real.fmt (StringCvt.FIX (SOME 2)) elapsed_time
  in
    (res, time_str)
  end
