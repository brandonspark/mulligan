
structure Directive =
  struct
    datatype t =
        Step
      | Reveal of int option
      | Stop
      | Prev of int option
      | Break of Symbol.symbol
      | BreakAssign of Symbol.symbol
      | Run
      | Clear of Symbol.symbol option
      | Print of Symbol.symbol
      | Set of Symbol.symbol * Symbol.symbol

    datatype ('a, 'b) either = INL of 'a | INR of 'b
  end
