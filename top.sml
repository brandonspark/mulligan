
structure Top =
  struct
    fun file_to_ast filename =
      let
        val filepath = FilePath.fromUnixPath filename
        val source = Source.loadFromFile filepath
        val _ = print "parsing...\n"
        val ast = Parser.parse source (* may err *)
        val _ = print "done parsing, elaborating...\n"
        val (derived_ast, _) = Elaborate.elab_ast (ast, Context.initial)
        val _ = print "done elaborating"
      in
        derived_ast
      end

    infix |>
    fun x |> f = f x

    fun println s = print (s ^ "\n")

    (* The initiate function takes in an AST and gets us into the main loop, via
     * Perform being raised on the first expression redex.
     *)
    fun initiate ast =
      let
        (* The step function is responsible for doing the stepping. Upon handling
         * a Perform upon the first redex, it passes a continuation
         * (second-class) to the interactive loop, pausing the evaluation.
         *)

        fun get_focus_exp focus =
          case focus of
            Debugger.EXP exp => exp
          | Debugger.VAL (exp, _) => exp

          (*
        fun get_focus_package (info as (i, return_cont, ctx, location, exp,
        cont)) focus =
          ( i
          , return_cont
          , ctx
          , location
          , exp
          , fn () => step info
          )
           *)

        val store : ( Context.t
                    * Location.location list
                    * Debugger.focus
                    * Context.value Cont.t) list ref = ref []

        datatype run_status =
            Running
          | Stepping

        val run : run_status ref = ref Stepping
        val breaks : SMLSyntax.symbol option ref list ref = ref []

        fun handle_break (b, cont) =
          ( run := Stepping
          ; Cont.throw cont ()
          )

        fun step (ctx, location, focus, cont) =
          let
            val _ =
              (store := (ctx, location, focus, cont) :: !store)

            val num = Cont.get_id cont
            val _ = print ("enter context " ^ Int.toString num ^ "\n")

            val new_info as (ctx, location, focus, cont) =
              ( case focus of
                Debugger.VAL (_, value) =>
                  ( print ("throwing val to cont " ^ Int.toString (Cont.get_id cont) ^ "\n")
                  ; Cont.throw cont value
                  )
              | _ =>
                ( print ("going into eval for " ^ PrettyPrintAst.report ctx
                  (get_focus_exp focus) 0 location ^ "\n")
                ; case Value.exp_to_value ctx (get_focus_exp focus) of
                    SOME value =>
                      ( print ("throwing " ^ PrettySimpleDoc.toString true
                        (PrettyPrintAst.show_value ctx value) ^ " value cont " ^ Int.toString
                        (Cont.get_id cont) ^ "\n")
                      ; Cont.throw cont value
                      )
                  | _ => Debugger.eval location (get_focus_exp focus) ctx cont
                )
              )
              handle
                Debugger.Perform
                  ( Debugger.Step { context = ctx, location, focus, cont, stop } ) =>
                  let
                    val _ =
                      if stop then
                        ( print "\n\n\n\n\nGOT A STOP\n"
                        ; run := Stepping
                        )
                      else
                        ()

                    val exp = get_focus_exp focus
                    val _ = print ("step returned with " ^ PrettyPrintAst.report ctx
                    exp 0 location ^ " and cont " ^ Int.toString (Cont.get_id cont)
                    ^ "\n")
                  in
                    (ctx, location, focus, cont)
                  end
              | Debugger.Perform (Debugger.Break cont) => handle_break cont

            val _ = print ("end context " ^ Int.toString num ^ "\n")
          in
            ( print ("3=> " ^ PrettyPrintAst.report ctx (get_focus_exp focus) 0 location ^ "\n")
            ; start_loop (ctx, location, focus, cont)
            )
          end

        and execute_prev (info as (ctx, location, focus, cont)) num_opt =
          let
            fun print_report (info as (ctx, location, focus, cont)) =
              ( print ("2=> " ^ PrettyPrintAst.report ctx (get_focus_exp focus)
                0 location ^ "\n")
              ; info
              )

            fun rewind n =
              case (n, !store) of
                (_, []) =>
                  print_report info
              | (0, _) =>
                  print_report info
              | (1, info :: rest) =>
                  ( store := rest
                  ; print_report info
                  )
              | (_, _ :: rest) =>
                  ( store := rest
                  ; rewind (n - 1)
                  )
          in
            start_loop (rewind (Option.getOpt (num_opt, 1)))
          end


        and start_loop (info as (ctx, location, focus, cont)) =
          (case !run of
            Running => step info
          | Stepping => main_loop info
          )


        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)

        and main_loop (info as (ctx, location, focus, cont)) =
          let
            val _ =
              print
                ("current exp is " ^ PrettyPrintAst.report ctx
                (get_focus_exp focus) 0 location ^ "\n")
            val output = TextIO.output (TextIO.stdOut, "- ")
            val _ = TextIO.flushOut TextIO.stdOut
            val input = TextIO.input TextIO.stdIn
          in
            case DirectiveParser.parse_opt input of
              SOME Directive.Step => step info
            | SOME Directive.Stop =>
                raise Fail "stop"
            | SOME (Directive.Reveal i') =>
                ( print ("revealing:\n"
                        ^ PrettyPrintAst.report ctx (get_focus_exp focus) (Option.getOpt (i', 0)) location
                        ^ "\n")
                ; start_loop info
                )
            | SOME (Directive.Prev num_opt) => execute_prev info num_opt
            | SOME (Directive.Break s) =>
                ( print ("breaking " ^ Symbol.toValue s ^ "\n")
                ; let
                    val (ctx, broken) = Context.break_fn ctx [s] true
                    val _ = breaks := broken :: !breaks
                  in
                    start_loop (ctx, location, focus, cont)
                  end
                )
            | SOME (Directive.BreakAssign s) =>
                let
                  val break_assigns = Context.get_break_assigns ctx
                in
                  ( print ("Breaking assignment to identifier " ^
                    Symbol.toValue s ^ "\n")
                  ; break_assigns := SymSet.insert (!break_assigns) s
                  ; start_loop info
                  )
                end
            | SOME Directive.Run => (run := Running; start_loop info)
            | SOME (Directive.Clear NONE) =>
                ( List.map (fn broken => broken := NONE) (!breaks)
                ; breaks := []
                ; Context.get_break_assigns ctx := SymSet.empty
                ; print "All breakpoints cleared.\n"
                ; start_loop info
                )
            | SOME (Directive.Clear (SOME sym)) =>
                ( print ("Breaking function " ^ Symbol.toValue sym ^ "\n")
                ; Context.break_fn ctx [sym] false
                ; start_loop info
                )
            | SOME (Directive.Print sym) =>
                ( print ("Printing value of " ^ Symbol.toValue sym ^ "\n")
                ; Context.get_val ctx [sym]
                  |> PrettyPrintAst.show_value ctx
                  |> PrettySimpleDoc.toString true
                  |> println
                ; start_loop info
                )
            | SOME (Directive.Set (setting, value)) =>
                ( case (Symbol.toValue setting, Symbol.toValue value) of
                    ("substitute", "true") =>
                      Context.set_substitute ctx true
                  | ("substitute", "false") =>
                      Context.set_substitute ctx false
                  | _ => raise Fail "unrecognized setting"
                ; start_loop info
                )
            | NONE =>
                ( print "Unrecognized command.\n"
                ; start_loop info
                )
          end

        fun entry_point {context = ctx, location, focus, cont, stop} =
          let
            val exp = get_focus_exp focus
          in
            print ("1=> " ^
              PrettyPrintAst.report
                ctx
                exp
                0
                location ^ "\n"
            );
            start_loop (ctx, location, focus, cont)
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        Debugger.eval_program ast Context.initial
          handle
            Debugger.Perform (Debugger.Step data) => entry_point data
          | Debugger.Perform (Debugger.Break cont) => handle_break cont

      end

    fun run filename =
      let
        (* val args = CommandLine.arguments () *)
        val ast = file_to_ast filename
      in
        initiate ast
      end

  end

val _ = Top.run "test2.sml"
