
structure Directive =
  struct
    datatype value =
        NUM of int
      | VALUE of Symbol.symbol

    datatype t =
        Step
      | Reveal of int option
      | Stop
      | Prev of int option
      | Break of Symbol.symbol
      | BreakBind of Symbol.symbol
      | Run
      | Clear of Symbol.symbol option
      | Print of Symbol.symbol
      | Set of Symbol.symbol * value
      | Report of Symbol.symbol
      | Last of int option
      | Help
      | TypeOf of Symbol.symbol

    datatype ('a, 'b) either = INL of 'a | INR of 'b
  end
