(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open Error
open PrettyPrintAst
open Printf
structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Configuration and logic for how to run the debugger.
 * 
 * We may, for instance, be interested in running the debugger strictly for
 * testing purposes. In which case, we need different logic than what the
 * debugger typically does.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

datatype 'a frame =
    Frame of 'a
  | Starred of 'a

datatype run_status =
    Running
  | Stepping

datatype prog_status =
    Step of Context.t * Location.location list * Debugger.focus
  | Finished of Context.t

type handler =
    Context.t
    * Location.location list
    * Debugger.focus
    * run_status ref
    * ( Context.t
      * Location.location list
      * Debugger.focus
      )  frame list ref
  -> exn
  -> prog_status

type runner_env = {
  step_handler : handler,
  running : bool,
  print_flag : bool,
  colored_output : bool,
  commands : Directive.t list
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

fun show_focus ctx location focus numopt =
  case focus of
    (Debugger.VAL (exp, _) | Debugger.EXP (exp, _)) =>
      PrettyPrintAst.report
        ctx
        exp
        (Option.getOpt (numopt, Context.get_print_depth ctx))
        location
      ^ "\n"
  | Debugger.PROG ast => PrettyPrintAst.pretty ctx ast true ^ "\n"

fun suspend x = fn () => x

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature RUN =
  sig
    val help_message : string 
    
    (* run the given trace with some pre-set commands 
     *)
    val run : runner_env -> Source.t -> Context.t -> Context.t

    val interactive_handler : handler 
    val test_handler : handler 
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

(* The run function allows the debugger to be run, with control over how each
 * step is handled, whether or not the `running` flag is set on start-up, and
 * whether things should be printed.
 *
 * In practice, this is used to so that we can silently, and uninteractively run
 * through the program, if we so wish.
 *)
structure Run : RUN =
  struct
    val help_message =
      "usage: mulligan [ARGS] FILE ... FILE\n" ^
      "Command-line arguments:\n" ^
      "  --help             displays this message\n\n\
      \  --no-color         disables colored output\n" ^

      "Debugger commands:\n" ^
      "  step               steps evaluation forward by one\n\
      \  step <i>           steps evaluation forward by <i>\n\
      \  evaluate           evaluates the currently focused subexpression to a value\n\
      \  prev               steps evaluation backward by one\n\
      \  prev <i>           steps evaluation backward by <i>\n\
      \  stop               exits program\n\
      \  reveal             reveals evaluation context by <print_depth> layers\n\
      \  reveal <i>         reveals evaluation context by <i> layers\n\
      \  breakfn <id>       sets a breakpoint for when the function value bound to <id>\
                            \is invoked\n\
      \  breakbind <id>     sets a breakpoint when the identifier <id> is bound to\n\
      \  clear              clears all breakpoints\n\
      \  clear <id>         clears breakpoint on function value bound to <id>\n\
      \  run                runs program until breakpoint or end of evaluation\n\
      \  last               rewinds evaluation until the last breakpoint\n\
      \  print <id>         prints value bound to identifier <id>\n\
      \  set <name> = <v>   sets the setting <name> to value <v>\n\
      \  help               displays this message\n\n" ^

      "Settings:\n" ^
      "  substitute = <b>        substitute values for identifiers in pretty printer\n\
      \  print_all = <b>         print entire context of program when stepping\n\
      \  print_dec = <b>         print context until nearest val binding when stepping\n\
      \  print_depth = <i>       print evaluation context <i> layers deep when stepping\n\
      \  pause_currying = <b>    pause evaluation when stepping curried arguments\n\
      \  pause_arithmetic = <b>  pause evaluation when stepping arithmetic operators \n\
      \  pause_app = <b>         pause evaluation when stepping function application\n"

    (* TODO: ugly, try to think of a better way to separate this logic *)
    fun run {step_handler, running, print_flag, colored_output, commands} source ctx : Context.t =
      let
        val print =
          if print_flag then
            if colored_output then 
              print
            else
              fn s => print (TC.decolorify s) 
          else
            (fn _ => ())

        fun println s = print (s ^ "\n")

        (* A store of all of the "frames" of the debugger we have
         * stepped through, since the beginning of execution.
         *)
        val store : ( Context.t
            * Location.location list
            * Debugger.focus
            ) frame list ref = ref []

        (* Whether we should autorun through execution.
         *)
        val run : run_status ref =
          ref (if running then Running else Stepping)

        (* All of the identifiers that we should break up, if we bind
         * to them.
         *)
        val breaks : SMLSyntax.symbol option ref list ref = ref []

        (* The last command executed.
         *)
        val last_command : Directive.t option ref = ref NONE

        (* Some commands to run at the beginning.
         * This is pretty much just for snapshot testing.
         *)
        val commands : Directive.t list ref = ref commands

        fun display (ctx, location, focus) =
          case !run of
            Running => ()
          | Stepping =>
            print ("==> \n"
              ^ show_focus ctx location focus NONE
              ^ "\n"
              )

        (* The eval_source function takes in an SML source and gets us into the main loop, via
        * Perform being raised on the first expression redex.
        *)
        fun parse_ast_from_source source ctx =
          let
            val _ =
              surround
                TC.lightblue
                ("Evaluating file " ^ orange (FilePath.toUnixPath (Source.fileName source)) ^ "...\n")
              |> print

            val (ast, _) = Elaborate.elab_ast (Parser.parse source, ctx)
                (* may err *)

            val _ =
              print
                ( "Loaded program:\n"
                ^ PrettyPrintAst.pretty ctx ast true
                ^ "\n\n"
                )
          in
            ast
          end

          (* The step function is responsible for doing the stepping.
           * There are only two actions `step` can take -- either it: 
           * - chooses to recurse and evaluate a sub-expression, or 
           * - it is finished evaluating an exp, and throws to a cont
           * 
           * If the evaluation of that expression ever signals that it 
           * is at a "significant redex", it raises Perform, and we enter
           * back into `start_loop` immediately.
           *)
          fun step (ctx, location, focus) evaluate =
            let
              val _ = push (Frame (ctx, location, focus)) store

              (* if we finish evaluating the currently focused expression with
               * `evaluate`, we need to set the run flag back to stepping  
               *)
              fun set () =
                if evaluate then run := Running else ()
              fun unset () =
                if evaluate then run := Stepping else ()

              val new_info =
                ( case focus of
                  Debugger.VAL (_, cont) =>
                    Cont.throw cont ()
                | Debugger.EXP (exp, cont) =>
                  ( case Value.exp_to_value ctx exp of
                      SOME value =>
                        Cont.throw cont (suspend value)
                    | _ =>
                      let
                        val _ = set ()

                        (* This ensures that once we throw back to this
                          * continuation, we continue stepping. 
                          *)
                        val new_cont =
                          Cont.do_after cont
                            (fn x =>
                              ( unset ()
                              ; if evaluate then
                                  Cont.callcc (fn cont =>
                                    (* THINK: why does it make sense to raise Perform
                                     * here? i don't understand
                                     *)
                                    raise Debugger.Perform (Debugger.Step
                                      { context = ctx
                                      , location = location
                                      , focus = Debugger.VAL (Value.value_to_exp (x ()), cont)
                                      , stop = false
                                      }
                                  ))
                                else ()
                              ; x
                              )
                            )
                      in
                        Debugger.eval location exp ctx new_cont
                        (* If we are evaluating some expression, and it raises an
                          * exception, we need to percolate that up to our caller.
                          *)
                        handle
                          Context.Raise exninfo =>
                            Cont.throw new_cont (fn () => raise Context.Raise exninfo)
                      end
                  )
                | Debugger.PROG ast =>
                    ( set ()
                    ; Finished (Debugger.eval_program ast ctx)
                    )
                )
                handle exn => step_handler (ctx, location, focus, run, store) exn

            in
              case new_info of
                Finished ctx =>
                  ( print (orange "Program evaluation finished.\n\n")
                  ; ctx
                  )
              | Step info =>
                  ( display info
                  ; start_loop info
                  )
            end

          and execute_prev info num_opt =
            let
              fun print_report (info as (ctx, location, focus)) =
                ( display (ctx, location, focus)
                ; info
                )

              fun rewind n l =
                case (n, l) of
                  ( (_, []) | (0, _) ) =>
                    (store := l; print_report info)
                | (1, (Frame info | Starred info) :: rest) =>
                    ( store := rest
                    ; print_report info
                    )
                | (_, _ :: rest) =>
                    rewind (n - 1) rest

              val num_steps = Option.getOpt (num_opt, 1)
            in
              start_loop (rewind num_steps (!store))
            end

          and start_loop info =
            (* If we're not running, then block for input.
             * Otherwise, allow stepping to happen.
             * In a sense, this means that if we are in `Running`,
             * we implicitly do `step` automatically.
             *)
            (case !run of
              Running => step info false
            | Stepping => main_loop info
            )

          (* The main loop is responsible for accepting user input and controlling
          * what is done by the program.
          *)
          and main_loop (info as (ctx, location, focus)) =
            let
              fun recur _ = start_loop info

              fun parse_command () =
                case !commands of
                  cmd::rest =>
                    ( commands := rest 
                    ; SOME cmd
                    )
                | [] => 
                    let
                      val input = TextIO.input TextIO.stdIn
                    in
                      case input of
                        (* An empty input repeats the previous command.
                        *)
                        "\n" => !last_command
                      | _ =>
                        case DirectiveParser.parse_opt input of
                          NONE => NONE
                        | SOME cmd =>
                            ( last_command := SOME cmd
                            ; SOME cmd
                            )  
                    end
            in
            ( TextIO.output (TextIO.stdOut, "- ")
            ; TextIO.flushOut TextIO.stdOut
            ; case parse_command () of
                SOME Directive.Step => step info false
              | SOME Directive.Evaluate => step info true
              | SOME Directive.Stop =>
                  ( print (lightblue "Bye bye!\n")
                  ; OS.Process.exit OS.Process.success
                  )
              | SOME (Directive.Reveal i') =>
                  let
                    val print_dec = #print_dec (Context.get_settings ctx)
                    val old_setting = !print_dec
                  in
                    ( print_dec := false
                    ; print ("Revealing:\n"
                            ^ show_focus ctx location focus i'
                            ^ "\n")
                    ; print_dec := old_setting
                    )
                    |> recur
                  end
              | SOME (Directive.Prev num_opt) => execute_prev info num_opt
              | SOME (Directive.Last num_opt) =>
                  let
                    fun last l i =
                      case (l, i) of
                        ([], _) => (info, [])
                      | ([(Frame x | Starred x)], _) => (x, [])
                      | (Starred x :: rest, 1) => (x, rest)
                      | (Frame _ :: rest, _) => last rest i
                      | (Starred _ :: rest, _) => last rest (i - 1)

                    val (info, rest) = last (!store) (Option.getOpt (num_opt, 1))
                  in
                    ( store := rest
                    ; display info
                    )
                    |> recur
                  end
              | SOME (Directive.BreakFn longid) =>
                  ( spf (`"Breakpoint set on function value bound to "fl"\n") longid |> print
                  ; let
                      val (_, broken) = Context.break_fn ctx longid true
                      val _ = push broken breaks 
                    in
                      start_loop info
                    end
                  )
              | SOME (Directive.BreakBind id) =>
                  let
                    val break_assigns = Context.get_break_assigns ctx
                  in
                    ( spf (`"Breakpoint set on bindings to identifier "fi"\n") id |> print
                    ; break_assigns := SymSet.insert (!break_assigns) id
                    )
                    |> recur
                  end
              | SOME Directive.Run =>
                  recur (run := Running)
              | SOME (Directive.Clear NONE) =>
                  ( List.map (fn broken => broken := NONE) (!breaks)
                  ; breaks := []
                  ; Context.get_break_assigns ctx := SymSet.empty
                  ; print "All breakpoints cleared.\n"
                  )
                  |> recur
              | SOME (Directive.Clear (SOME longid)) =>
                  ( print <| spf (`"Breaking function "fl"\n") longid
                  ; Context.break_fn ctx longid false
                  )
                  |> recur
              | SOME (Directive.Print longid) =>
                  ( print <| spf (`"Printing value of identifier "fl"\n") longid
                  ; Context.get_val ctx longid
                    |> PrettyPrintAst.show_value ctx
                    |> println
                  )
                  |> recur
              | SOME (Directive.Set (setting, value)) =>
                  let
                    datatype setting_value = 
                        BOOL of bool 
                      | STR of string 
                      | NUMBER of int

                    fun parse_value s =
                      case s of
                        Directive.VALUE s =>
                          (case Symbol.toValue s of
                            "true" => BOOL true
                          | "false" => BOOL false
                          | s => STR s
                          )
                      | Directive.NUM i => NUMBER i
                  in
                    recur
                      ( case (Symbol.toValue setting, parse_value value) of
                          ("substitute", BOOL b) =>
                            Context.set_substitute ctx b
                        | ("print_all", BOOL b) =>
                            #print_all (Context.get_settings ctx) := b
                        | ("print_dec", BOOL b) =>
                            #print_dec (Context.get_settings ctx) := b
                        | ("print_depth", NUMBER i) =>
                            #print_depth (Context.get_settings ctx) := i
                        | ("pause_currying", BOOL b) =>
                            #pause_currying (Context.get_settings ctx) := b
                        | ("pause_arithmetic", BOOL b) =>
                            #pause_arithmetic (Context.get_settings ctx) := b
                        | ("pause_app", BOOL b) =>
                            #pause_app (Context.get_settings ctx) := b

                        | ("substitute", _)
                        | ("print_all", _)
                        | ("print_dec", _)
                        | ("print_depth", _)
                        | ("pause_currying", _)
                        | ("pause_arithmetic", _) 
                        | ("pause_app", _) =>
                            print <| spf (`"Invalid input for setting "fi"\n") setting
                        | _ => print "Unrecognized setting.\n"
                      )
                  end
              | SOME (Directive.Report s) =>
                  recur
                    ( case Symbol.toValue s of
                      "ctx" => print (PrettyPrintAst.show_ctx ctx ^ "\n")
                    | "location" => print (PrettyPrintAst.show_location ctx location ^ "\n")
                    | _ => print "Unrecognized report.\n"
                    )
              | SOME Directive.Help =>
                  recur (print help_message)
              | SOME (Directive.TypeOf longid) =>
                  (case Context.get_ident_ty_opt ctx longid of
                    NONE => recur (print "Cannot find type of unbound identifier.\n")
                  | SOME (_, tyscheme) => recur (println (PrettyPrintAst.show_tyscheme tyscheme))
                  )
              | NONE =>
                  recur (print "Unrecognized command.\n")
            )
            end

        (* let's go! *)
        val ast = parse_ast_from_source source ctx
      in
        (* This call will result in entering the interactive loop.
        *
        * This handler gets entered at any point that we checkpoint _after_ a
        * throw.
        *)
        start_loop (ctx, [Location.PROG ast], Debugger.PROG ast)
      end


    (* This is my step handler.
    *
    * I never knew my real handler.
    *)
    fun interactive_handler (ctx, location, focus, run, store) exn =
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
                ^ show_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; OS.Process.exit OS.Process.failure
              )
          | UserError reason =>
              ( red "Error" ^ ": User-induced error\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ show_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; Step (ctx, location, focus)
              )
          | InvalidProgramError reason =>
              ( red "Error" ^ ": Invalid program\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ show_focus ctx location focus NONE
                |> surround TC.softred
                |> print
              ; OS.Process.exit OS.Process.failure
              )
          | TypeError {reason} =>
              ( red "Error" ^ ": Type error\n"
                ^ mk_reason reason
                ^ "Context:\n"
                ^ show_focus ctx location focus NONE
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

    fun test_handler (ctx, _, _, _, store) exn =
      case exn of
        Debugger.Perform
          ( Debugger.Step { context = ctx, location, focus, stop = _} ) =>
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
            EvalError _ =>
              raise exn
          | UserError _ =>
              raise Fail "probably should not happen"
          | InvalidProgramError _ =>
              raise exn
          | TypeError _ =>
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
  end
