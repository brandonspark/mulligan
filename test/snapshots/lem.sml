datatype ('a, 'b) either = INL of 'a | INR of 'b
type 'a lem = ('a, 'a Cont.cont) either

val lem_proof : unit -> 'a lem =
  fn () =>
    Cont.callcc
      (fn ret =>
        INL (Cont.callcc (fn na =>
          Cont.throw ret (INR na)
        ))
      )

val res =
  case lem_proof () of
    INL n => n * n
  | INR nn => Cont.throw nn 2
