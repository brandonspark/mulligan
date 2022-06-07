
structure Top =
  struct
    fun file_to_ast filename =
      let
        val filepath = FilePath.fromUnixPath filename
        val source = Source.loadFromFile filepath
        val ast = Parser.parse source (* may err *)
        val (derived_ast, _) = Elaborate.elab_ast (ast, Context.initial)
      in
        derived_ast
      end

    (*
    datatype kind =
        EXP of SMLSyntax.exp
      | VAL of Context.value

    val print_fn = fn () =>
      case kind of
        EXP exp =>
          PrettyPrintAst.pretty_exp ctx exp true
      | VAL value =>
          PrettyPrintAst.pretty_value ctx value value true
    *)

    datatype result =
      Result of
        Context.t
      * Location.location list
      * SMLSyntax.exp
      * (unit -> result)



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

        fun get_focus_package (ctx, location, exp, cont) focus =
          ( ctx
          , location
          , exp
          , case focus of
              Debugger.EXP exp => (fn () => step (ctx, location, exp, cont))
            | Debugger.VAL (exp, value) => (fn () => MLton.Cont.throw (cont,
              value))
          )

        and step (ctx, location, exp, cont) =
          (Debugger.eval location exp ctx cont)
          handle
            Debugger.Perform
              { context = ctx
              , location
              , focus
              , cont
              } =>
              let
                val exp = get_focus_exp focus
              in
                Result
                  (get_focus_package (ctx, location, exp, cont) focus)
              end

        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)
        fun main_loop (ctx, location, exp, f) =
          let
            val output = TextIO.output (TextIO.stdOut, "- ")
            val _ = TextIO.flushOut TextIO.stdOut
            val input = TextIO.input TextIO.stdIn
          in
            case DirectiveParser.parse_exn input of
              Directive.Step =>
                let
                  val Result (ctx, location, exp, f) =
                    f ()

                  val _ = print ("3=> " ^ PrettyPrintAst.report ctx exp 0 location ^ "\n")
                in
                  main_loop (ctx, location, exp, f)
                end
            | Directive.Stop =>
                raise Fail "stop"
            | Directive.Reveal i =>
                ( print ("revealing:\n "
                        ^ PrettyPrintAst.report ctx exp (Option.getOpt (i, 0)) location
                        ^ "\n")
                ; main_loop (ctx, location, exp, f)
                )
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        Debugger.eval_program ast Context.initial
          handle
            Debugger.Perform
              { context = ctx
              , location
              , focus
              , cont
              } =>
              let
                val exp =  get_focus_exp focus
              in
                print ("1=> " ^
                  PrettyPrintAst.report
                    ctx
                    exp
                    0
                    location ^ "\n"
                );
                main_loop
                    (get_focus_package (ctx, location, exp, cont) focus)
              end
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
