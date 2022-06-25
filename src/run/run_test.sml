
local
  open Common
  open Error

  (* This is my step handler.
   *
   * I never knew my real handler.
   *)
  fun step_handler (ctx, location, focus, run, store) exn =
    case exn of
      Debugger.Perform
        ( Debugger.Step { context = ctx, location, focus, stop } ) =>
          Step (ctx, location, focus)
    | Debugger.Perform (Debugger.Break (_, cont)) =>
        ( case !store of
            [] => ()
          | (Frame x)::rest => store := (Starred x) :: rest
          | (Starred _) :: _ => ()
        ; Cont.throw cont ()
        )
    | Signal (SigError err) =>
        ( case err of
          EvalError reason =>
            raise exn
        | UserError reason =>
            raise Fail "probably should not happen"
        | InvalidProgramError reason =>
            raise exn
        | TypeError {reason} =>
            raise exn
        | LexError _ =>
            raise exn
        | ParseError _ =>
            raise exn
        )
    | _ =>
      ( if List.null (MLton.Exn.history exn) then () else
        print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") (MLton.Exn.history exn)))
      ; raise exn
      )
in
  structure RunTest =
    MkRun
      ( val step_handler = step_handler
        val running = true
        val print_flag = false
      )
end
