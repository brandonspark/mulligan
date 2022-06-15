
structure Either =
  struct
    datatype ('a, 'b) either = INL of 'a | INR of 'b
    datatype t = datatype either
  end
