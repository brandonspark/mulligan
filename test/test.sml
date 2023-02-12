(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open SMLSyntax
open Error
structure TF = TestFramework

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Integration testing for `mulligan`.
 * These tests allow us to verify that the extensional behavior of the 
 * debugger is behaving as expected.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val sym = Symbol.fromValue

val true_val =
  Vconstr {id = [sym "true"], arg = NONE}
val false_val =
  Vconstr {id = [sym "false"], arg = NONE}

fun vstring s = Vstring (sym s)

fun mk_record fields =
  Vrecord (List.map (fn (lab, value) => {lab = sym lab, value = value}) fields)

(* Set up the runner with our precise testing configuration.
  *)
val run_test = 
  Run.run 
    { step_handler = Run.test_handler
    , running = true 
    , print_flag = false
    , colored_output = false
    , commands = []
    }

(*****************************************************************************)
(* Test outcomes *)
(*****************************************************************************)

datatype outcome =
    RES of Context.value
  | RAISE of Context.value
  | ERR of Error.error

fun outcome_eq (outcome1, outcome2) =
  case (outcome1, outcome2) of
    (RES v1, RES v2) => Value.value_eq (v1, v2)
  | (RAISE v1, RAISE v2) => Value.value_eq (v1, v2)
  | (ERR e1, ERR e2) => Error.error_eq (e1, e2)
  | _ => false

fun show_outcome outcome =
  case outcome of
    RES v => "RES " ^ PrettyPrintAst.print_value Basis.initial v
  | RAISE v => "RAISE " ^ PrettyPrintAst.print_value Basis.initial v
  | ERR error => "ERR " ^ Error.show_error error

(*****************************************************************************)
(* Run-handling logic *)
(*****************************************************************************)

(* This handles the result of a run into our `outcome` type.
  *)
fun run_handler test_name exn =
  case exn of
    Signal (SigError error) => ERR error
  | Context.Raise (name, exnid, arg) => RAISE (Vexn {name = name, exnid = exnid, arg = arg})
  | ParseSMLError.Error e =>
      ( print ("Parse error during test " ^ lightblue test_name ^ "\n")
      ; TCS.print
          (ParseSMLError.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e)
      ; if List.null (MLton.Exn.history exn) then () else
          print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") (MLton.Exn.history exn)))
      ; OS.Process.exit OS.Process.failure
      )
  | _ =>
      ( print ("Unknown exception caught during test " ^ lightblue test_name ^ "\n")
      ; raise exn
      )

fun evaluate test_name text =
  let
    val source =
      Source.loadFromString
        { filename = test_name
        , text = text
        }
  in
    ( case Context.get_val_opt (run_test source Basis.initial) [Symbol.fromValue "res"] of
        (* If we didn't bind `res`, say that it's some random unlikely value. 
         *)
        NONE => RES (Vselect (sym "terminated but res unbound")) 
      | SOME value => RES value
    )
    handle exn =>
      run_handler test_name exn
  end

(*****************************************************************************)
(* Test framework assertions *)
(*****************************************************************************)


fun assert_is test_name (text, outcome) =
  TF.assert_equal
    (SOME show_outcome)
    outcome_eq
    (evaluate test_name text, outcome)

fun assert_is_rolling test_name pairs =
  List.foldl
    (fn ((text, outcome), acc) =>
      ( assert_is test_name (acc ^ " " ^ text, outcome)
      ; acc ^ " " ^ text
      )
    )
    ""
    pairs

fun assert_ill_typed test_name text =
  case (evaluate test_name text) of
    ERR (TypeError _) => ()
  | outcome => TF.assert_failure (text ^ "\nGot: " ^ show_outcome outcome)

fun assert_evaluates test_name text =
  case (evaluate test_name text) of
    RES _ => ()
  | outcome => TF.assert_failure (text ^ "\nGot: " ^ show_outcome outcome)

fun assert_errs test_name text =
  case (evaluate test_name text) of
    ERR _ => ()
  | outcome => TF.assert_failure (text ^ "\nGot: " ^ show_outcome outcome)

(*****************************************************************************)
(* Test outcomes *)
(*****************************************************************************)

structure Test :
  sig
    val run_tests : unit -> unit
  end =
  struct
    open TestFramework
    open Error

    fun test_arithmetic ctx =
      let
        val test_name = TestFramework.get_test_name ctx
        val assert_is = assert_is test_name

        val typed_tests =
          [ ("val res = 1 + 2", RES (Vnumber (Int 3)))
          , ("val res = 1 * 2", RES (Vnumber (Int 2)))
          , ("val res = 1 - 2", RES (Vnumber (Int ~1)))
          , ("val res = 1 div 2", RES (Vnumber (Int 0)))
          , ("val res = 1 mod 2", RES (Vnumber (Int 1)))
          , ("val x = 1.0 / 1.2 val res = ()", RES Vunit)
          ]
        val _ = List.app assert_is typed_tests

        val illtyped_tests =
          [ "val res = 1 + true"
          , "val res = \"hi\" * 2"
          , "val res = 1 - 2.0"
          , "val res = () div 2"
          , "val res = [] mod 2"
          , "val x = 1.0 / false val res = ()"
          ]
        val _ = List.app (assert_ill_typed test_name) illtyped_tests
      in
        ()
      end

    fun test_builtin ctx =
      let
        val test_name = TestFramework.get_test_name ctx
        val assert_is = assert_is test_name

        val static_test =
          "val x : 'a list = []                     \
          \val y : int list = []                    \

          \val res1 = 1 :: [5, 0]                   \
          \val res2 : 'a list list = [] :: [[], []] \
          \val res3 : int list list = [1] :: []     \

          \val f : 'a -> 'a list = fn x => [x]      \
          \val g : 'a list -> 'a option =           \
          \ fn l =>                                 \
          \   case l of                             \
          \     [z] => SOME z                       \
          \   | _ => NONE                           \
          \val _ : int option = g (f 5)             \

          \val order1 : order = LESS                \
          \val order2 : order = EQUAL               \
          \val order3 : order = GREATER             \

          \val b1 : bool = true                     \
          \val b2 : bool = false                    \

          \val u : unit = ()                        \

          \val exn1 : exn = Div                     \
          \val exn2 : exn = Match                   \
          \val exn3 : exn = Bind                    \
          \val exn4 : string -> exn = Fail          "

        val _ =
          assert_evaluates static_test
      in
        ()
      end

    fun test_fn ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val static_tests =
          [ "val f = fn x => x + 1      \
            \val g = fn x => x ^ \"hi\" \

            \val x = f 2                \
            \val y = g \"oh \"          "
          , "val x = fn h => x h        \
            \and rec y = fn z => y z    "
          ]

        val _ =
          List.app (assert_evaluates test_name) static_tests

        val illtyped_tests =
          [ "val res = (fn x : int => x) \"hi\""
          , "val res = (fn name as x : int => x) \"hi\""
          , "val res = (fn x : 'a => x) 2"
          ]

        val _ =
          List.app (assert_ill_typed test_name) illtyped_tests

      in
        ()
      end

    fun test_tuple ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val tests =
          [ ("val (res, y) = (1, \"hi\")", RES (Vnumber (Int 1)))
          , ("val (_, res) = (1, \"hi\")", RES (vstring "hi"))
          , ("val (a, b) = (3, true) \
             \val res = if b then a else a + 2", RES (Vnumber (Int 3))
            )

          , ("val f = (fn (x, y) => (x orelse true, y + 2)) \
             \val (res, _) = f (false, 1)" , RES true_val
            )
          , ("val f = (fn (x, y) => (x orelse true, y + 2)) \
             \val (_, res) = f (false, 1)", RES (Vnumber (Int 3))
            )
          ]

        val _ =
          List.app (assert_is test_name) tests
      in
        ()
      end

    fun test_record ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val tests =
          [ ( "val res = {b = \"hi\", a = false, c = 150} val r = res"
            , RES ( mk_record [ ("a", false_val)
                              , ("b", vstring "hi")
                              , ("c", Vnumber (Int 150))
                              ]
                  )
            )

          , ( "val res = (#a r) orelse true"
            , RES true_val
            )
          , ( "val res = (#b r) ^ \" there\""
            , RES (vstring "hi there")
            )
          , ( "val res = (#c r) div 50"
            , RES (Vnumber (Int 3))
            )
          , ( "val nested = {x = {a = true, b = \"that\"}, y = 150} \
              \val res = \"do \" ^ #b (#x nested)                   "
            , RES (vstring "do that")
            )
          ]
        val _ = assert_is_rolling test_name tests
      in
        ()
      end

    fun test_recursion ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val dynamic_tests =
          [ ( "fun fact 0 = 1                 \
              \  | fact n = n * fact (n - 1)  \

              \val res = fact 3               "
            , RES (Vnumber (Int 6))
            )
          , ( "fun fact NONE = 0              \
              \  | fact (SOME n) = n + n      \

              \val res = fact NONE            "
            , RES (Vnumber (Int 0))
            )
          , ( "val res = fact (SOME 150)"
            , RES (Vnumber (Int 300))
            )
          , ( "fun len l =               \
              \  case l of               \
              \    [] => 0               \
              \  | x::xs => 1 + len xs   \

              \val res = len [1,2,3,4,5] "
            , RES (Vnumber (Int 5))
            )
          , ( "fun stagedExp k =                  \
              \  if k = 0 then fn n => 1          \
              \  else                             \
              \    let                            \
              \      val prev = stagedExp (k - 1) \
              \    in                             \
              \      fn n => n * prev n           \
              \    end                            \
              \val f = stagedExp 3                \
              \val res = f 2                      "
            , RES (Vnumber (Int 8))
            )
          ]
        val _ = assert_is_rolling test_name dynamic_tests

        val static_test =
          "fun f (x : 'a) : 'b = f x  \
          \fun g (x : int) : 'b = g x "

        val _ = assert_evaluates test_name static_test
      in
        ()
      end

    fun test_typed ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val static_test =
          "val x : int = 1 + 2                          \
          \val y : string = \"hi\"                      \

          \val f = (fn (x : string, y : int) => (y, x)) \

          \val (a : int, b : string) = f (y, x)         "

        val _ = assert_evaluates test_name static_test
      in
        ()
      end

    fun test_datatypes ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val static_test =
          "datatype foo = A | B                                 \

          \val x : foo = A                                      \
          \val y : foo = B                                      \

          \datatype 'a opt = NONE | SOME of 'a                  \

          \val a : 'a opt = NONE                                \
          \val b : 'b opt = NONE                                \
          \val c : int opt = SOME 5                             \

          \datatype ('a, 'b) either = Left of 'a | Right of 'b  \

          \val res1 : (int, 'a) either = Left 150               \
          \val res2 : ('a, string) either = Right \"hi\"        \

          \datatype 'a nest = Base | Recur of (int, 'a) either  \

          \val res3 : bool nest = Base                          \
          \val res4 : bool nest = Recur (Left 150)              \
          \val res5 : bool nest = Recur (Right true)            \

          \val res6 : bool =                                    \
          \  case res5 of                                       \
          \    Base => false                                    \
          \  | Recur (Left _) => false                          \
          \  | Recur (Right b) => b                             \
          \val res7 : int =                                     \
          \  case Left 150 of                                   \
          \    Left x => x                                      \
          \  | _ => 122                                         \
          \val res8 : string =                                  \
          \  case Right \"150\" of                              \
          \    Right x => x                                     \
          \  | _ => \"no\"                                      "

        val _ = assert_evaluates test_name static_test
      in
        ()
      end

    fun test_poly ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val static_tests =
          [ "val f = fn x => x                                \

            \val x = f 1                                      \
            \val y = f \"hi\"                                 \

            \val g =                                          \
            \ fn f =>                                         \
            \   fn x =>                                       \
            \     fn y =>                                     \
            \       (f x, f y)                                \

            \val a = g (fn x => x + 1) 1 2                    \
            \val b = g (fn s => s ^ \" to\") \"hi\" \"there\" "
          , "val x = fn () =>                  \
            \  (let                            \
            \     val id: 'a -> 'a = fn z => z \
            \   in                             \
            \     id id                        \
            \   end                            \
            \  ; fn z => z                     \
            \  )                               "
          , "val x : bool -> bool = (fn x => x) (fn x => x)"
          ]
        val _ = List.app (assert_evaluates test_name) static_tests

        val advanced_static_test =
          "val 'a f =                                     \
          \  fn x : 'a => x : 'a                          \

          \val a = f 1                                    \
          \val b = f \"hi\"                               \

          \val ('a, 'b, 'c) g                             \
          \      : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = \
          \  fn f : 'b -> 'c =>                           \
          \   fn g : 'a -> 'b =>                          \
          \      fn x : 'a =>                             \
          \        f (g x)                                \

          \val 'a x : ('a -> 'a) -> 'a -> 'a =            \
          \  fn f : 'a -> 'a =>                           \
          \    let                                        \
          \      val y : 'b = raise Div                   \
          \    in                                         \
          \      f : 'a -> 'a                             \
          \    end                                        "
        val _ = assert_evaluates test_name advanced_static_test

        val _ =
          assert_is test_name
          ( "fun 'a f x =                      \
            \  let                             \
            \    val id : 'a -> 'a = fn z => z \
            \  in                              \
            \    (x : 'a)                      \
            \  end                             "
          , ERR (InvalidProgramError "shadowed implicit type variables")
          )

        val _ =
          assert_is test_name
          ( "fun 'a f x =       \
            \  let              \
            \    fun 'a g x = x \
            \  in               \
            \    x              \
            \  end              "
          , ERR (InvalidProgramError "shadowed implicit type variables")
          )

        val _ =
          assert_is test_name
          ( "fun f x =                                         \
            \  ( let                                           \
            \      val 'a x =                                  \
            \        (fn () => raise Fail \"hi\") : unit -> 'a \
            \    in                                            \
            \      1                                           \
            \    end                                           \
            \  ; ()                                            \
            \  )                                               \
            \and g x =                                         \
            \  (fn x : 'a => x)                                "
          , ERR (InvalidProgramError "shadowed implicit type variables")
          )

        (* Both of these tests are not the same as how NJ does it, but this is
         * how MLton does it.
         * Basically, just keep the unification variables around, and let them
         * become the first necessary thing.
         *)
        val _ =
          assert_is test_name
            ( "val f = (fn x => x) (fn x => x) \
              \val res = f 5                   "
            , RES (Vnumber (Int 5))
            )

        val _ =
          assert_ill_typed test_name
            "val f = (fn x => x) (fn x => x) \
            \val res = f 5                   \
            \val res = f \"hi\"              "
      in
        ()
      end

    fun test_scoping ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val _ =
          assert_evaluates test_name
            "local                                           \
            \  datatype t = Foo                              \
            \in                                              \
            \  (* t's tyid is in scope *)                    \
            \  datatype t2 = datatype t                      \
            \end                                             \

            \(* t's tyid is out of scope *)                  \
            \signature FOO =                                 \
            \  sig                                           \
            \    (* what happens is we look for t2's tyid,   \
            \       but it's out of scope by now             \
            \     *)                                         \
            \    datatype t3 = datatype t2                   \
            \  end                                           \

            \structure Foo :> FOO =                          \
            \  struct                                        \
            \    datatype t3 = datatype t2                   \
            \  end                                           "

          val _ =
            [ "local datatype t = FOO in end val _ = FOO"
            , "local val x = 2 in end val _ = x + 2"
            , "val _ = let val x = 2 in 5 end val res = x + 2"
            , "structure Foo = struct val x = 2 end val _ = x + 2"
            ]
            |> List.app (assert_errs test_name)
      in
        ()
      end

    fun test_abstract ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val header =
          "structure Foo :>                                \
          \sig                                             \
          \  type t                                        \
          \  val to : int -> t                             \
          \  val a : (t * t) -> t                          \
          \  val app : t -> (t -> t) -> t                  \
          \end =                                           \
          \struct                                          \
          \  type t = int                                  \
          \  val to = fn x => x                              \
          \  fun a (x, y) = let val res = x + y in res end \
          \  fun app x f = f x                             \
          \end                                             "

        val illtyped_tests =
            [ "val _ = Foo.to 2 + Foo.to 3"
            , "val _ = Foo.app (Foo.to 2) (fn x => x + x)"
            ]

        val _ =
          List.app
            (fn test => assert_ill_typed test_name (header ^ test))
            illtyped_tests

        val header_is_tests =
            [ ( "val res = Foo.a (Foo.to 2, Foo.to 3)"
              , RES (Vnumber (Int 5))
              )
            , ( "val res = Foo.app (Foo.to 2) (fn x => Foo.a (x, x))"
              , RES (Vnumber (Int 4))
              )
            , ( "val res = (fn x : Foo.t => x) (Foo.to 2)"
              , RES (Vnumber (Int 2))
              )
            ]

        val _ =
          List.app
            (fn (text, outcome) => assert_is test_name (header ^ text, outcome))
            header_is_tests
      in
        ()
      end

    fun test_cont ctx =
      let
        val test_name = TestFramework.get_test_name ctx

        val is_tests =
          [ ( "val res = Cont.callcc (fn cont => Cont.throw cont 5)"
            , RES (Vnumber (Int 5))
            )
          , ( "val res = Cont.callcc (fn _ => 5)"
            , RES (Vnumber (Int 5))
            )
          , ( "exception Raise of int Cont.cont               \

              \fun f () =                                     \
              \  Cont.callcc (fn x =>                         \
              \    raise Raise x                              \
              \  )                                            \

              \val x =                                        \
              \  f () handle Raise cont => Cont.throw cont 40 \

              \val res = x + x                                "
            , RES (Vnumber (Int 80))
            )
          , ( "datatype ('a, 'b) either = INL of 'a | INR of 'b \
              \type 'a lem = ('a, 'a Cont.cont) either          \

              \val lem_proof : unit -> 'a lem =                 \
              \  fn () =>                                       \
              \    Cont.callcc                                  \
              \      (fn ret =>                                 \
              \        INL (Cont.callcc (fn na =>               \
              \          Cont.throw ret (INR na)                \
              \        ))                                       \
              \      )                                          \

              \val res =                                        \
              \  case lem_proof () of                           \
              \    INL n => n * n                               \
              \  | INR nn => Cont.throw nn 2                    "
            , RES (Vnumber (Int 4))
            )
          ]

        val _ = List.app (assert_is test_name) is_tests
      in
        ()
      end

    infix >::
    infix >:::

    fun run_tests () =
      TestFramework.run
        ( "integration" >:::
            [ "arithmetic" >:: test_arithmetic
            , "builtin"    >:: test_builtin
            , "fn"         >:: test_fn
            , "tuple"      >:: test_tuple
            , "record"     >:: test_record
            , "recursion"  >:: test_recursion
            , "datatypes"  >:: test_datatypes
            , "poly"       >:: test_poly
            , "scoping"    >:: test_scoping
            , "abstract"   >:: test_abstract
            , "cont"       >:: test_cont
            ]
        )
  end

(* let's go! *)
val _ = 
  ( Test.run_tests ()
  ; Snapshots.run ()
  )
