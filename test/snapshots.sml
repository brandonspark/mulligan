(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open Printf
structure TF = TestFramework
structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Snapshot testing for `mulligan`.
   It's best to have a testing framework that can enforce that evaluation
   traces stay the same.
   This way, we'll know if a bug induces some unwanted behavior that isn't
   purely extensional.
 *)

val cmd_ext = "cmds"
val trace_ext = "trace"
val sml_ext = "sml" (* who would have guessed *)
val snapshot_dir = "test/snapshots"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This function lets us run a thunk with stdout and stderr
 * redirected to another file.
 * If that file doesn't exist, it's created.
 * In particular, we can use this to pipe the debugger's output to a trace file.
 *)
fun redirect_to_file file f =
  let
    (* these duplicate stdout and stderr's file descriptors, so that we
      * can set back to them once we change the real stdout and stderr
      *)
    val stdout_fd = Posix.IO.dup Posix.FileSys.stdout
    val stderr_fd = Posix.IO.dup Posix.FileSys.stderr


    val file_fd =
      Posix.FileSys.createf
        ( file
        , Posix.FileSys.O_WRONLY
        , Posix.FileSys.O.flags [Posix.FileSys.O.trunc]
        , Posix.FileSys.S.flags [Posix.FileSys.S.irwxu]
        )
  in
    ( Posix.IO.dup2 { old = file_fd, new = Posix.FileSys.stdout }
    ; Posix.IO.dup2 { old = file_fd, new = Posix.FileSys.stderr }
    ; f ()
    ; Posix.IO.dup2 { old = stdout_fd, new = Posix.FileSys.stdout }
    ; Posix.IO.dup2 { old = stderr_fd, new = Posix.FileSys.stderr }
    )
  end

fun parse_commands_file cmd_file =
  IO.cat cmd_file
  |> List.map DirectiveParser.parse_opt
  |> opt_all

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Snapshots =
  struct

    fun test_of_file cmd_file =
      let
        val trace_file = OS.Path.base cmd_file ^ "." ^ trace_ext
        val prog_file = OS.Path.base cmd_file ^ "." ^ sml_ext

        val commands = parse_commands_file cmd_file

        val trace_contents =
          if file_exists trace_file then
            String.concat (IO.cat trace_file)
          else
            "<nonexistent file>"

        fun test_fn _ =
          case commands of
            NONE =>
              TF.assert_failure
              <| spf (`"Failed to parse command file "fs"") (OS.Path.file cmd_file)
          | SOME commands =>
              let
                val config =
                  { test_mode = true
                  , skipping = false
                  , print_flag = true
                  (* Snapshot output should be uncolored, so that the trace
                  * files are actually human-readable.
                  *)
                  , colored_output = false
                  , commands = commands
                  }

                (* This will overwrite the existing trace file.
                 * It would be nice to not be side-effecting, and instead
                 * merely inform that the tests have changed, but in all honesty
                 * I usually just use version control to reset the files anyways.
                 * It's more convenient to me to just alter by default.
                 *)
                val () =
                  redirect_to_file trace_file (fn () =>
                    Run.run config
                      (Source.loadFromFile (FilePath.fromUnixPath prog_file))
                      (Basis.initial ())
                  )

                val new_trace_contents = String.concat (IO.cat trace_file)
              in
                if trace_contents = new_trace_contents then
                  ()
                else
                  TF.assert_failure
                  <| spf (`"Trace contents have changed -- was:\n"fs"\nnow:\n"fs"")
                    trace_contents
                    new_trace_contents
              end
      in
        TF.mk_test (OS.Path.file cmd_file, test_fn)
      end

    (* Runs from the `mulligan` directory.
     *)
    fun run () =
      let
        val cmd_files =
          files_of_directory snapshot_dir
          |> List.filter (fn path => OS.Path.ext path = SOME cmd_ext)
      in
        cmd_files
        |> List.map test_of_file
        |> (fn test => TF.mk_suite ("snapshots", test))
        |> TF.run
      end
  end
