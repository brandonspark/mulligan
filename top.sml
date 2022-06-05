
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

    datatype result =
      Result of
        Context.t
      * Debugger.location list
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
        fun step location exp ctx cont =
          ( Debugger.eval location exp ctx cont
          ; raise Fail "shouldn't get here"
          )
          handle
            Debugger.Perform
              { context
              , location
              , exp
              , cont
              , continue
              } =>
                Result (context, location, exp, fn () => step location exp context cont)
              (* if continue then
              else
                (context, location, exp, fn () => MLton.Cont.throw (cont, exp))
              *)

        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)
        fun main_loop location exp ctx f =
          let
            val input = TextIO.input TextIO.stdIn
          in
            case input of
              "go\n" =>
                let
                  val Result (context, location, exp, f) =
                    f ()
                in
                  print ("stepped\n");
                  main_loop location exp context f
                end
            | "stop\n" =>
                raise Fail "stop"
            | _ => raise Fail "unrecognized command"
          end
        in
        (* This call will result in entering the interactive loop.
         *)
        Debugger.eval_program ast Context.initial
          handle
            Debugger.Perform
              { context
              , location
              , exp
              , cont
              , continue
              } =>
              main_loop
                location
                exp
                context
                (fn () => step location exp context cont)
            (*
            if continue then
            else
              (context, location, exp, fn () => MLton.Cont.throw (cont, exp))
            *)
      end

    fun run filename =
      let
        (* val args = CommandLine.arguments () *)
        val ast = file_to_ast filename
      in
        initiate ast
      end

  end
