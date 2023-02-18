(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The type of debugger commands.
 *)

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Directive =
  struct
    type longid = Symbol.symbol list

    datatype value =
        NUM of int
      | VALUE of Symbol.symbol

    datatype t =
        Step
      | Evaluate
      | Reveal of int option
      | Stop
      | Prev of int option
      | BreakFn of longid
      | BreakBind of Symbol.symbol
      | Run
      | Clear of longid option
      | Print of longid
      | Set of Symbol.symbol * value
      | Report of Symbol.symbol
      | Last of int option
      | Help
      | TypeOf of longid
  end
