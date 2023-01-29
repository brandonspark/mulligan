(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure Token =
  struct
    datatype t =
        NUM of int
      | IDENT of Symbol.symbol list
      | STEP
      | EVALUATE
      | REVEAL
      | STOP
      | PREV
      | BREAKBIND
      | BREAKFN
      | RUN
      | CLEAR
      | PRINT
      | REPORT
      | LAST
      | HELP
      | TYPEOF

      | EQUAL
      | SET

      | EOF

    fun to_string t =
      case t of
        NUM i => "NUM " ^ Int.toString i
      | IDENT syms =>
          String.concatWith "." (List.map Symbol.toValue syms)
      | STEP => "STEP"
      | EVALUATE => "EVALUATE"
      | REVEAL => "REVEAL"
      | STOP => "STOP"
      | EQUAL => "EQUAL"
      | SET => "SET"
      | PREV => "PREV"
      | BREAKFN => "BREAKFN"
      | BREAKBIND => "BREAKBIND"
      | RUN => "RUN"
      | CLEAR => "CLEAR"
      | PRINT => "PRINT"
      | REPORT => "REPORT"
      | LAST => "LAST"
      | HELP => "HELP"
      | TYPEOF => "TYPEOF"
      | EOF => "EOF"

  end
