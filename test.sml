
(* Proof of concept for a tight interactive loop which acts as a "debugger" for
 * expressions in a simple language of booleans.
 *)
structure C = SMLofNJ.Cont

datatype exp =
    Const of bool
  | Andalso of exp * exp
  | Orelse of exp * exp
  | Star [.deriving show]


datatype 'a status =
    Stop of exp list * exp * (unit -> 'a status)
  | Go of exp [.deriving show]

exception Surface of exp list * exp * exp C.cont * bool

fun replace_star exp exp' =
  case exp of
    Star => exp'
  | Const _ => exp
  | Andalso (e1, e2) => Andalso (replace_star e1 exp', replace_star e2 exp')
  | Orelse (e1, e2) => Orelse (replace_star e1 exp', replace_star e2 exp')

fun checkpoint outer_exp inner_exp ctx =
  let
    val exp =
      C.callcc (fn cont =>
        raise Surface (outer_exp::ctx, inner_exp, cont, true)
      )

    (* This one restores the context once we're done with the expression being
     * evaluated.
     *
     * It need not be done when we're concerning a value, though.
     *)
    val _ =
      C.callcc (fn cont =>
        raise Surface (ctx, replace_star outer_exp exp, cont, false)
      )
  in
    exp
  end

(* eval only ever throws *)
fun eval exp cont ctx =
  case exp of
    Const b => C.throw cont exp
  | Andalso (exp1, exp2) =>
      let
        val exp1 = checkpoint (Andalso (Star, exp2)) exp1 ctx
        val exp2 = checkpoint (Andalso (exp1, Star)) exp2 ctx
      in
        case (exp1, exp2) of
          (Const true, Const true) => C.throw cont (Const true)
        | _ => C.throw cont (Const false)
      end
  | Orelse (exp1, exp2) =>
      let
        val exp1 = checkpoint (Orelse (Star, exp2)) exp1 ctx
        val exp2 = checkpoint (Orelse (exp1, Star)) exp2 ctx
      in
        case (exp1, exp2) of
          (Const false, Const false) => C.throw cont (Const false)
        | _ => C.throw cont (Const true)
      end

fun step exp cont ctx =
  let
    val exp = eval exp cont ctx
  in
    Go exp
  end
  handle Surface (ctx, exp, cont, flag) =>
    if flag then
      Stop (ctx, exp, fn () => step exp cont ctx)
    else
      Stop (ctx, exp, fn () => C.throw cont exp)

val test =
 Andalso
    ( Orelse ( Const true
             , Const true
             )
    , Const false
    )

exception Done of exp

fun interact () =
  let
    val exp = ref test
    val ctx : exp list ref = ref []
    val final_exp =
      C.callcc (fn cont =>
        let
          val f : (unit -> exp status) ref = ref (fn () => step (!exp) cont (!ctx))
        in
          ( while true do
            (case (print "\n"; TextIO.input TextIO.stdIn) of
              "go\n" =>
                    (case (!f) () of
                      Stop (ctx', exp', f') =>
                        ( print ("stepped to " ^ (show_exp exp') ^ "\n")
                        ; exp := exp'
                        ; ctx := ctx'
                        ; f := f'
                        )
                    | Go exp' =>
                        (print (if true then "t\n" else "f\n")
                        ; raise Fail "done"
                        )
                    )
            | "stop\n" => raise Fail "stop"
            | "out\n" =>
                (case !ctx of
                  [] => raise Fail "no outer context!"
                | fst::_ => print ("CONTEXT:\n" ^ show_exp fst ^ "\n")
                )
            | _ => raise Fail "unrecognized"
            )
          ; raise Fail "impossible"
          )
        end
      )
  in
    final_exp
  end
