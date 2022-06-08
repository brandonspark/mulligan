
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
        int
      * int Cont.t
      * Context.t
      * Location.location list
      * SMLSyntax.exp
      * (result -> result)



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

        and step (ctx, location, focus, cont) =
          ( (* print ("going into eval for " ^ PrettyPrintAst.report ctx
            (get_focus_exp focus) 0
            location ^ "\n");
             *)
            case Context.exp_to_value ctx (get_focus_exp focus) of
              SOME value => ((* print ("throwing " ^ PrettySimpleDoc.toString true
              (PrettyPrintAst.show_value ctx value) ^ " value cont " ^ Int.toString
              (Cont.get_id cont)
              ^ "\n"); *) Cont.throw cont value)
            | _ => Debugger.eval location (get_focus_exp focus) ctx cont
          ; raise Fail "asdsas"
          )
          handle
            Debugger.Perform
              { context = ctx
              , location
              , focus
              , cont
              } =>
              let
                val exp = get_focus_exp focus
                (* val _ = print ("step returned with " ^ PrettyPrintAst.report ctx
                exp 0 location ^ " and cont " ^ Int.toString (Cont.get_id cont)
                ^ "\n")
                 *)
              in
                (ctx, location, focus, cont)
              end

          (*
        fun iter (info as (i, prev_cont, ctx, location, exp, _)) =
          (i, prev_cont, ctx, location, exp, fn () => step info)
           *)

        (* The main loop is responsible for accepting user input and controlling
         * what is done by the program.
         *)

        val store : ( Context.t
                    * Debugger.focus
                    * Location.location list
                    * Context.value Cont.t) list ref = ref []

        fun main_loop (info as (ctx, location, focus, cont)) =
          let
            (*val _ = print ("current exp is " ^ PrettyPrintAst.report ctx
            (get_focus_exp focus) 0 location ^ "\n")
             *)
            val output = TextIO.output (TextIO.stdOut, "- ")
            val _ = TextIO.flushOut TextIO.stdOut
            val input = TextIO.input TextIO.stdIn
          in
            case DirectiveParser.parse_exn input of
              Directive.Step =>
                let
                  val _ =
                    (store := (ctx, focus, location, cont) :: !store)
                  (*val num = Cont.get_id cont
                  val _ = print ("enter context " ^ Int.toString num ^ "\n")
                   *)
                  (* After this call, it seems the value of `i` changes. *)
                  (* It appears that after the call to `f`, we throw back into
                   * another context, namely the first context where we were
                   * evaluating the original expression.
                   * However, because the expression has updated, nothing looks
                   * different - except the prev continuation.
                   *)
                  val new_info as (ctx, location, focus, cont) =
                    case focus of
                      Debugger.VAL (_, value) =>
                        ((* print ("throwing val to cont " ^ Int.toString
                        (Cont.get_id cont) ^ "\n"); *)Cont.throw cont value
                        )
                    | _ => step info

                  (*val _ = print ("end context " ^ Int.toString num ^ "\n")
                   **)
                in
                  ( print ("3=> " ^ PrettyPrintAst.report ctx
                  (get_focus_exp focus) 0 location ^ "\n")
                  ; main_loop (ctx, location, focus, cont)
                  )
                end
            | Directive.Stop =>
                raise Fail "stop"
            | Directive.Reveal i' =>
                ( print ("revealing:\n "
                        ^ PrettyPrintAst.report ctx (get_focus_exp focus) (Option.getOpt (i', 0)) location
                        ^ "\n")
                ; main_loop info
                )
            | Directive.Prev =>
                ( case !store of
                    [] =>
                      ( print ("2=> " ^ PrettyPrintAst.report ctx (get_focus_exp
                      focus) 0 location ^ "\n")
                      ; main_loop info
                      )
                  | (ctx, focus, location, cont) :: rest =>
                      ( print ("2=> " ^ PrettyPrintAst.report ctx (get_focus_exp
                      focus) 0 location ^ "\n")
                      ; store := rest
                      ; main_loop (ctx, location, focus, cont)
                      )
                )
          end

        fun entry_point {context = ctx, location, focus, cont} =
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
            main_loop (ctx, location, focus, cont)
          end
      in
        (* This call will result in entering the interactive loop.
         *
         * This handler gets entered at any point that we checkpoint _after_ a
         * throw.
         *)
        Debugger.eval_program ast Context.initial
          handle
            Debugger.Perform data => entry_point data

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
