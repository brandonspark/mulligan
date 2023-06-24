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

(* This context type is meant to include anything that should be visible from
 * within the relevant tests.
 *)
type context =
  { path : string
  , test_name : string
  }

type test_modifier =
  (* A function to be called after the test, depending on failure or success.
   *)
  { after_fn : bool -> unit
  }

datatype test =
    Test of string * (context -> unit) * test_modifier option
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

    val mk_test : string * (context -> unit) -> test
    val mk_suite : string * test list -> test

    val >:: : string * (context -> unit) -> test
    val >::: : string * test list -> test

    val modify_test : test_modifier -> test -> test

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

    fun mk_test (s, test) = Test (s, test, NONE)
    val mk_suite = Suite

    val >:: = mk_test
    val >::: = mk_suite

    (* Modify a test or test suite with a modifier.
     * A use case for this, for instance, is snapshot testing, when you may need
     * to side-effectively update the snapshots on failure.
     *)
    fun modify_test modifier test =
      case test of
        Test (s, test, _) => Test (s, test, SOME modifier)
      | Suite (s, tests) => Suite (s, List.map (fn test => modify_test modifier test) tests)

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
        fun handle_test ctx (test_name, test_fn, modifier) =
          let
            val ctx = set_test_name ctx test_name
            val passed =
              ( test_fn ctx
              ; true
              )
              handle TestFail s =>
                ( print (border ^ "\n")
                ; print (red "Failure: " ^ lightblue test_name ^ "\n\n")
                ; print (s ^ "\n")
                ; false
                )
          in
            ( case modifier of
                NONE => ()
              | SOME { after_fn } => after_fn passed
            ; if passed then
                (1, 0)
              else
                (0, 1)
            )
          end

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

        val test_name =
          case test of
            Test (s, _, _) => s
          | Suite (s, _) => s
      in
        ( print (border2 ^ "\n")
        ; print <| spf (`"Test: "fs"\n") (orange test_name)
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
