
(* The MkRun functor allows the debugger to be run, with control over how each
 * step is handled, whether or not the `running` flag is set on start-up, and
 * whether things should be printed.
 *
 * In practice, this is used to so that we can silently, and uninteractively run
 * through the program, if we so wish.
 *)
functor MkRun
  ( val step_handler :
         Context.t
       * Location.location list
       * Debugger.focus
       * Common.run_status ref
       * ( Context.t
         * Location.location list
         * Debugger.focus
         )  Common.frame list ref
      -> exn
      -> Common.prog_status
    val running : bool
    val print_flag : bool
  ) :
  sig
    val help_message : string

    val eval_source : Source.t -> Context.t -> Context.t
  end =
  struct
    open Error
    open Common

    fun suspend x = fn () => x

    val help_message =
    "usage: mulligan [ARGS] FILE ... FILE\n" ^
    "Command-line arguments:\n" ^
    "  --help             displays this message\n\n" ^

    "Debugger commands:\n" ^
    "  step               steps evaluation forward by one\n\
    \  step <i>           steps evaluation forward by <i>\n\
    \  prev               steps evaluation backward by one\n\
    \  prev <i>           steps evaluation backward by <i>\n\
    \  stop               exits program\n\
    \  reveal             reveals evaluation context by <print_depth> layers\n\
    \  reveal <i>         reveals evaluation context by <i> layers\n\
    \  break <id>         sets a breakpoint for when the function value bound to <id>\
                         \is invoked\n\
    \  break bind <id>    sets a breakpoint when the identifier <id> is bound to\n\
    \  clear              clears all breakpoints\n\
    \  clear <id>         clears breakpoint on function value bound to <id>\n\
    \  run                runs program until breakpoint or end of evaluation\n\
    \  last               rewinds evaluation until the last breakpoint\n\
    \  print <id>         prints value bound to identifier <id>\n\
    \  set <name> = <v>   sets the setting <name> to value <v>\n\
    \  help               displays this message\n\n" ^

    "Settings:\n" ^
    "  substitute = <b>   substitute values for identifiers in pretty printer\n\
    \  print_dec = <b>    print context until nearest val binding when stepping\n\
    \  print_depth = <i>  print evaluation context <i> layers deep when stepping\n"

    val print =
      if print_flag then
        print
      else
        (fn _ => ())

    fun println s = print (s ^ "\n")

    (* The eval_source function takes in an SML source and gets us into the main loop, via
     * Perform being raised on the first expression redex.
     *)
    fun eval_source source ctx =
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

        (* The step function is responsible for doing the stepping. Upon handling
         * a Perform upon the first redex, it re-enters the interactive loop
         * with the new evaluation information, pausing the evaluation.
         *)

        val store : ( Context.t
            * Location.location list
            * Debugger.focus
            ) frame list ref = ref []

        val run : run_status ref =
          ref (if running then Running else Stepping)

        val breaks : SMLSyntax.symbol option ref list ref = ref []

        fun display (ctx, location, focus) =
          print ("==> \n"
            ^ print_focus ctx location focus NONE
            ^ "\n"
            )

        fun step (ctx, location, focus) =
          let
            val _ = store := Frame (ctx, location, focus) :: !store

            val new_info =
              ( case focus of
                Debugger.VAL (_, value, cont) =>
                  Cont.throw cont (suspend value)
              | Debugger.EXP (exp, cont) =>
                ( case Value.exp_to_value ctx exp of
                    SOME value =>
                      Cont.throw cont (suspend value)
                  | _ =>
                    (* If we are evaluating some expression, and it raises an
                     * exception, we need to percolate that up to our caller.
                     *)
                    (Debugger.eval location exp ctx cont)
                    handle
                      Context.Raise exninfo =>
                        Cont.throw cont (fn () => raise Context.Raise exninfo)
                )
              | Debugger.PROG ast =>
                  Finished (Debugger.eval_program ast ctx)
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

        and execute_prev (info as (ctx, location, focus)) num_opt =
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
          in
            start_loop (rewind (Option.getOpt (num_opt, 1)) (!store))
          end

        and start_loop info =
          (case !run of
            Running => step info
          | Stepping => main_loop info
          )

        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)
        and main_loop (info as (ctx, location, focus)) =
          let
            fun recur x = start_loop info
          in
          ( TextIO.output (TextIO.stdOut, "- ")
          ; TextIO.flushOut TextIO.stdOut
          ; case DirectiveParser.parse_opt (TextIO.input TextIO.stdIn) of
              SOME Directive.Step => step info
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
                          ^ print_focus ctx location focus i'
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
                    | (Frame x :: rest, _) => last rest i
                    | (Starred x :: rest, _) => last rest (i - 1)

                  val (info, rest) = last (!store) (Option.getOpt (num_opt, 1))
                in
                  ( store := rest
                  ; display info
                  )
                  |> recur
                end
            | SOME (Directive.Break s) =>
                ( print ( "Breakpoint set on function value bound to "
                        ^ orange (Symbol.toValue s)
                        ^ "\n"
                        )
                ; let
                    val (ctx, broken) = Context.break_fn ctx [s] true
                    val _ = breaks := broken :: !breaks
                  in
                    start_loop info
                  end
                )
            | SOME (Directive.BreakBind s) =>
                let
                  val break_assigns = Context.get_break_assigns ctx
                in
                  ( print ("Breakpoint set on bindings to identifier "
                          ^ orange (Symbol.toValue s) ^ "\n")
                  ; break_assigns := SymSet.insert (!break_assigns) s
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
            | SOME (Directive.Clear (SOME sym)) =>
                ( print ("Breaking function " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.break_fn ctx [sym] false
                )
                |> recur
            | SOME (Directive.Print sym) =>
                ( print ("Printing value of identifier " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.get_val ctx [sym]
                  |> PrettyPrintAst.print_value ctx
                  |> println
                )
                |> recur
            | SOME (Directive.Set (setting, value)) =>
                let
                  fun sym_to_bool s =
                    case Symbol.toValue s of
                      "true" => SOME true
                    | "false" => SOME false
                    | _ => NONE
                in
                  recur
                    ( case (Symbol.toValue setting, value) of
                      (_, Directive.VALUE s) =>
                        (case (Symbol.toValue setting, sym_to_bool s) of
                          (_, NONE) => print "Expected a boolean value.\n"
                        | ("substitute", SOME b) =>
                            Context.set_substitute ctx b
                        | ("print_dec", SOME b) =>
                            #print_dec (Context.get_settings ctx) := b
                        | _ => print "Unrecognized setting."
                        )
                    | ("print_depth", Directive.NUM i) =>
                        #print_depth (Context.get_settings ctx) := i
                    | _ => print "Unrecognized setting.\n"
                    )
                end
            | SOME (Directive.Report s) =>
                recur
                  ( case Symbol.toValue s of
                    "ctx" => print (PrettyPrintAst.ctx_toString ctx ^ "\n")
                  | "location" => print (PrettyPrintAst.location_toString ctx location ^ "\n")
                  | _ => print "Unrecognized report.\n"
                  )
            | SOME Directive.Help =>
                recur (print help_message)
            | SOME (Directive.TypeOf sym) =>
                (* TODO: support longids *)
                (case Context.get_ident_ty_opt ctx [sym] of
                  NONE => recur (print "Cannot find type of unbound identifier.\n")
                | SOME (_, tyscheme) => recur (println (PrettyPrintAst.print_tyscheme tyscheme))
                )
            | NONE =>
                recur (print "Unrecognized command.\n")
          )
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        start_loop (ctx, [Location.PROG ast], Debugger.PROG ast)
      end
  end
