
structure Directive =
  struct
    datatype t =
        Step
      | Reveal of int option
      | Stop

    datatype ('a, 'b) either = INL of 'a | INR of 'b
  end
