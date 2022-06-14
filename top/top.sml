
structure Top =
  struct
    fun file_to_ast filename =
      let
        val filepath = FilePath.fromUnixPath filename
        val source = Source.loadFromFile filepath
        val ast = Parser.parse source (* may err *)
        val (derived_ast, _) = Elaborate.elab_ast (ast, Basis.initial)
      in
        derived_ast
      end

    infix |>
    fun x |> f = f x

    fun println s = print (s ^ "\n")

    fun get_focus_exp focus =
      case focus of
        Debugger.EXP exp => exp
      | Debugger.VAL (exp, _) => exp

    datatype 'a frame =
        Frame of 'a
      | Starred of 'a

    datatype run_status =
        Running
      | Stepping

    structure TC = TerminalColors

    fun text color s = TC.foreground color ^ s ^ TC.reset
    val orange = text TC.orange
    val lightblue = text TC.lightblue

    val border = "=================================\n"
    fun surround color s = text color border ^ s ^ text color border

    (* The eval_file function takes in an SML file and gets us into the main loop, via
     * Perform being raised on the first expression redex.
     *)
    fun eval_file filename ctx =
      let
        val _ =
          print
            (surround TC.lightblue
              ("Evaluating file " ^ orange filename ^ "...\n") ^ "\n"
            )

        val ast = file_to_ast filename

        val _ =
          print
            ( "Loadded program:\n"
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
            * Context.value Cont.t) frame list ref = ref []

        val run : run_status ref = ref Stepping

        val breaks : SMLSyntax.symbol option ref list ref = ref []

        fun handle_break (b, cont) =
          ( run := Stepping
          ; case !store of
              [] => ()
            | (Frame x)::rest => store := (Starred x) :: rest
            | (Starred _) :: _ => ()
          ; Cont.throw cont ()
          )

        fun display (ctx, location, focus, cont) =
          print ("==> \n"
                ^ PrettyPrintAst.report
                    ctx
                    (get_focus_exp focus)
                    (Context.get_print_depth ctx)
                    location
                ^ "\n"
                )

        fun step (ctx, location, focus, cont) =
          let
            val _ = store := Frame (ctx, location, focus, cont) :: !store

            val num = Cont.get_id cont
            (*val _ = print ("enter context " ^ Int.toString num ^ "\n")
             * *)

            val new_info as (ctx, location, focus, cont) =
              ( case focus of
                Debugger.VAL (_, value) =>
                  ( (*print ("throwing val to cont " ^ Int.toString (Cont.get_id cont) ^ "\n")
                  ; *)Cont.throw cont value
                  )
              | _ =>
                ( (*print ("going into eval for " ^ PrettyPrintAst.report ctx
                  (get_focus_exp focus) 0 location ^ "\n")
                ; *)case Value.exp_to_value ctx (get_focus_exp focus) of
                    SOME value =>
                      ( (*print ("throwing " ^ PrettySimpleDoc.toString true
                        (PrettyPrintAst.show_value ctx value) ^ " value cont " ^ Int.toString
                        (Cont.get_id cont) ^ "\n")
                      ; *)Cont.throw cont value
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
                        ( (*print "\n\n\n\n\nGOT A STOP\n"
                        ; *)run := Stepping
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

            (*
            val _ = print ("end context " ^ Int.toString num ^ "\n")
             *)
          in
            ( display (ctx, location, focus, cont)
            ; start_loop (ctx, location, focus, cont)
            )
          end

        and execute_prev (info as (ctx, location, focus, cont)) num_opt =
          let
            fun print_report (info as (ctx, location, focus, cont)) =
              ( display (ctx, location, focus, cont)
              ; info
              )

            fun rewind n =
              case (n, !store) of
                ( (_, []) | (0, _) ) =>
                  print_report info
              | (1, (Frame info | Starred info) :: rest) =>
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
            (*val _ =
              print
                ("current exp is " ^ PrettyPrintAst.report ctx
                (get_focus_exp focus) 0 location ^ "\n")
            *)
          ( TextIO.output (TextIO.stdOut, "- ")
          ; TextIO.flushOut TextIO.stdOut
          ; case DirectiveParser.parse_opt (TextIO.input TextIO.stdIn) of
              SOME Directive.Step => step info
            | SOME Directive.Stop => raise Fail "stop"
            | SOME (Directive.Reveal i') =>
                let
                  val print_dec = #print_dec (Context.get_settings ctx)
                  val old_setting = !print_dec
                in
                  ( print_dec := false
                  ; print ("Revealing:\n"
                          ^ PrettyPrintAst.report ctx (get_focus_exp focus) (Option.getOpt (i', 0)) location
                          ^ "\n")
                  ; print_dec := old_setting
                  ; start_loop info
                  )
                end
            | SOME (Directive.Prev num_opt) => execute_prev info num_opt
            | SOME (Directive.Last num_opt) =>
                let
                  val first = ref info

                  fun last' () =
                    case !store of
                      [] => !first
                    | [Frame x] => (store := []; first := x; last' ())
                    | [Starred x] => (store := []; first := x; x)
                    | Frame _ :: rest => (store := rest; last' ())
                    | Starred x :: rest => (store := rest; x)

                  fun last i =
                    case i of
                      1 => last' ()
                    | _ => (last' (); last (i - 1))

                  val info = (last (Option.getOpt (num_opt, 1)))
                in
                  ( display info
                  ; start_loop info
                  )
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
                    start_loop (ctx, location, focus, cont)
                  end
                )
            | SOME (Directive.BreakAssign s) =>
                let
                  val break_assigns = Context.get_break_assigns ctx
                in
                  ( print ("Breaking assignment to identifier " ^ orange (Symbol.toValue s) ^ "\n")
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
                ( print ("Breaking function " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.break_fn ctx [sym] false
                ; start_loop info
                )
            | SOME (Directive.Print sym) =>
                ( print ("Printing value of identifier " ^ orange (Symbol.toValue sym) ^ "\n")
                ; Context.get_val ctx [sym]
                  |> PrettyPrintAst.show_value ctx
                  |> PrettySimpleDoc.toString true
                  |> println
                ; start_loop info
                )
            | SOME (Directive.Set (setting, value)) =>
                let
                  fun sym_to_bool s =
                    case Symbol.toValue s of
                      "true" => true
                    | "false" => false
                    | _ => raise Fail "expected a bool"
                in
                  ( case (Symbol.toValue setting, value) of
                      ("substitute", Directive.VALUE s) =>
                        Context.set_substitute ctx (sym_to_bool s)
                    | ("print_dec", Directive.VALUE s) =>
                        #print_dec (Context.get_settings ctx) := sym_to_bool s
                    | ("print_depth", Directive.NUM i) =>
                        #print_depth (Context.get_settings ctx) := i
                    | _ => raise Fail "unrecognized setting"
                  ; start_loop info
                  )
                end
            | SOME (Directive.Report s) =>
                ( case Symbol.toValue s of
                    "ctx" => print (PrettyPrintAst.ctx_toString ctx ^ "\n")
                  | "location" => print (PrettyPrintAst.location_toString ctx location ^ "\n")
                ; start_loop info
                )
            | NONE =>
                ( print "Unrecognized command.\n"
                ; start_loop info
                )
          )

        fun entry_point {context = ctx, location, focus, cont, stop} =
          let
            val exp = get_focus_exp focus
          in
            display (ctx, location, focus, cont);
            start_loop (ctx, location, focus, cont)
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        Debugger.eval_program ast ctx
          handle
            Debugger.Perform (Debugger.Step data) => entry_point data
          | Debugger.Perform (Debugger.Break cont) => handle_break cont
      end

    fun handle_file cur_path filename ctx =
      let
        val canonical_new =
          OS.Path.mkCanonical (OS.Path.concat (cur_path, filename))

        val (new_dir, new_path) =
          if OS.Path.isAbsolute filename then
            (OS.Path.dir filename, filename)
          else
            (OS.Path.dir canonical_new, canonical_new)
      in
        case (String.sub (filename, 0), OS.Path.ext filename) of
          (_, NONE) => raise Fail "error, expected ext"
        | (#"$", _) => raise Fail "TODO"
        | (_, SOME ("sml" | "sig" | "fun")) =>
            eval_file new_path ctx
        | (_, SOME "cm") =>
            (case CM_Parser.parse_file new_path of
              Either.INL strs => raise Fail "invalid parse"
            | Either.INR (((structs, sigs, functors), sources), []) =>
                let
                  val ctx' =
                    List.foldl
                      (fn (source, ctx) =>
                        handle_file new_dir
                          ( case source of
                            CM_Token.PATH sym => Symbol.toValue sym
                          | CM_Token.STRING s => s
                          )
                          ctx
                      )
                      ctx
                      sources

                  val modules =
                    List.map
                      (fn s =>
                        (s, Context.get_module ctx' [s])
                      )
                      structs

                  val sigs =
                    List.map
                      (fn s =>
                        case Context.get_sigval_opt ctx' s of
                          NONE => raise Fail "failed to find sig"
                        | SOME ans => (s, ans)
                      )
                      sigs

                  val functors =
                    List.map
                      (fn s =>
                        case Context.get_functorval_opt ctx' s of
                          NONE => raise Fail "failed to find sig"
                        | SOME ans => (s, ans)
                      )
                      functors

                  fun add_things f ctx l =
                    List.foldl
                      (fn ((id, elem), ctx) =>
                        f ctx id elem
                      )
                      ctx
                      l

                  val add_modules = fn ctx => add_things Context.add_module ctx modules
                  val add_sigs = fn ctx => add_things Context.add_sig ctx sigs
                  val add_functors = fn ctx => add_things Context.add_functor ctx functors
                in
                  ctx
                  |> add_modules
                  |> add_sigs
                  |> add_functors
                end
            | Either.INR _ => raise Fail "invalid parse"
            )
        | _ => raise Fail "invalid extension"
      end


    fun run () =
      let
        val args = CommandLine.arguments ()
      in
        List.foldl
          (fn (filename, ctx) =>
            handle_file "" filename ctx
          )
          Basis.initial
          args
      end

  end

val _ = Top.run ()
