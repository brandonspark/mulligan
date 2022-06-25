
structure Common =
  struct
    datatype 'a frame =
        Frame of 'a
      | Starred of 'a

    datatype run_status =
        Running
      | Stepping

    datatype prog_status =
        Step of Context.t * Location.location list * Debugger.focus
      | Finished of Context.t

    structure TC = TerminalColors

    fun text color s = TC.foreground color ^ s ^ TC.reset
    val orange = text TC.orange
    val red = text TC.red
    val lightblue = text TC.lightblue

    fun print_focus ctx location focus numopt =
      case focus of
        (Debugger.VAL (exp, _, _) | Debugger.EXP (exp, _)) =>
          PrettyPrintAst.report
            ctx
            exp
            (Option.getOpt (numopt, Context.get_print_depth ctx))
            location
          ^ "\n"
      | Debugger.PROG ast => PrettyPrintAst.pretty ctx ast true ^ "\n"
  end
