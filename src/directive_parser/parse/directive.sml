
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

    datatype ('a, 'b) either = INL of 'a | INR of 'b
  end
