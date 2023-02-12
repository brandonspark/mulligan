(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)


structure TC = TerminalColors

local
  open Error

  (* This is my step handler.
   *
   * I never knew my real handler.
   *)
  fun step_handler (ctx, location, focus, run, store) exn =
    case exn of
      Debugger.Perform
        ( Debugger.Step { context = ctx, location, focus, stop } ) =>
          ( if stop then run := Stepping
            else ()
          ; Step (ctx, location, focus)
          )
    | Debugger.Perform (Debugger.Break (_, cont)) =>
        ( run := Stepping
        ; case !store of
            [] => ()
          | (Frame x)::rest => store := (Starred x) :: rest
          | (Starred _) :: _ => ()
        ; Cont.throw cont ()
        )
    | Signal (SigError err) =>
        ( case err of
          EvalError reason =>
            ( red "Error" ^ ": Evaluation failure\n"
              ^ mk_reason reason
              ^ "Context:\n"
              ^ print_focus ctx location focus NONE
              |> surround TC.softred
              |> print
            ; OS.Process.exit OS.Process.failure
            )
        | UserError reason =>
            ( red "Error" ^ ": User-induced error\n"
              ^ mk_reason reason
              ^ "Context:\n"
              ^ print_focus ctx location focus NONE
              |> surround TC.softred
              |> print
            ; Step (ctx, location, focus)
            )
        | InvalidProgramError reason =>
            ( red "Error" ^ ": Invalid program\n"
              ^ mk_reason reason
              ^ "Context:\n"
              ^ print_focus ctx location focus NONE
              |> surround TC.softred
              |> print
            ; OS.Process.exit OS.Process.failure
            )
        | TypeError {reason} =>
            ( red "Error" ^ ": Type error\n"
              ^ mk_reason reason
              ^ "Context:\n"
              ^ print_focus ctx location focus NONE
              |> surround TC.softred
              |> print
            ; OS.Process.exit OS.Process.failure
            )
        | ( LexError _
          | ParseError _
          ) => raise Fail "shouldn't happen"
        )
    | _ =>
      ( if List.null (MLton.Exn.history exn) then () else
        print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") (MLton.Exn.history exn)))
      ; raise exn
      )
in
  structure RunDebugger =
    MkRun
      ( val step_handler = step_handler
        val running = false
        val print_flag = true
        (* These commands are strictly for testing purposes.
         *)
        val commands = []
      )
end
