(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a test framework for generalized tests.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type context =
  { path : string
  , test_name : string
  }

datatype test =
    Test of string * (context -> unit)
  | Suite of string * test list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val border =  "============================================"
val border2 = "--------------------------------------------"

val base_context =
  { path = ""
  , test_name = ""
  }

fun set_test_name (ctx : context) new =
  { path = #path ctx
  , test_name = new
  }

infix ++
fun (x, y) ++ (a, b) = (x + a, y + b)

fun red s = TC.foreground TC.softred ^ s ^ TC.reset
fun lightblue s = TC.foreground TC.lightblue ^ s ^ TC.reset
fun green s = TC.foreground TC.softgreen ^ s ^ TC.reset

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature TESTFRAMEWORK =
  sig
    type context
    type test

    val >:: : string * (context -> unit) -> test
    val >::: : string * test list -> test

    val get_test_name : context -> string

    val assert_equal : ('a -> string) option -> ('a * 'a -> bool) -> 'a * 'a -> unit
    val assert_failure : string -> 'a
    val assert_bool : string -> bool -> unit

    val run : test -> unit
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure TestFramework : TESTFRAMEWORK =
  struct
    exception TestFail of string

    type context = context

    datatype test = datatype test

    val >:: = Test
    val >::: = Suite

    fun get_test_name (ctx : context) = #test_name ctx

    fun assert_equal print_fn eq_fn (x, y) =
      if eq_fn (x, y) then
        ()
      else
        raise TestFail
          ( case print_fn of
              NONE => "Expected equality"
            | SOME print_fn =>
              spf (`"Expected equality: got "fs" and "fs"") (print_fn x) (print_fn y)
          )

    fun assert_failure s =
      raise TestFail s

    fun assert_bool s b =
      if b then ()
      else
        raise TestFail s

    fun run test =
      let
        fun handle_test ctx (test_name, test_fn) =
          let
            val ctx = set_test_name ctx test_name
          in
            ( test_fn ctx
            ; (1, 0)
            )
          end
          handle TestFail s =>
            ( print (border ^ "\n")
            ; print (red "Failure: " ^ lightblue test_name ^ "\n\n")
            ; print (s ^ "\n")
            ; (0, 1)
            )

        fun run' ctx test =
          case test of
            Test test => handle_test ctx test
          | Suite (name, tests) =>
              #1
                ( List.foldl
                    (fn (test, (acc, num)) =>
                      let
                        val ctx =
                            { path = #path ctx ^ Int.toString num ^ ":" ^ name ^ "/"
                            , test_name = "ERR"
                            }
                      in
                        ( run' ctx test ++ acc
                        , num + 1
                        )
                      end
                    )
                    ((0, 0), 0)
                    tests
                )

        (* !! here we run the tests !! 
         *)
        val ((passed, failed), time) =
          with_time_str (fn () => run' base_context test)

      in
        ( print (border2 ^ "\n")
        ; print <| spf (`"Ran: "fs" "fs" in: "fs" seconds.\n")
                (lightblue (Int.toString (passed + failed)))
                (if (passed + failed) = 1 then "test" else "tests")
                (lightblue time)
        ; if failed = 0 then
            print (green "All tests passed.\n")
          else
            print <| spf (`""fs": "fs" test cases.\n") (red "FAILED") (Int.toString failed)
        )
      end
  end
