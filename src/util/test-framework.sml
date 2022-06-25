
structure TestFramework :
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
    =
  struct
    structure TC = TerminalColors

    fun red s = TC.foreground TC.softred ^ s ^ TC.reset
    fun lightblue s = TC.foreground TC.lightblue ^ s ^ TC.reset
    fun green s = TC.foreground TC.softgreen ^ s ^ TC.reset

    exception TestFail of string

    type context =
      { path : string
      , test_name : string
      }

    val base_context =
      { path = ""
      , test_name = ""
      }

    datatype test =
        Test of string * (context -> unit)
      | Suite of string * test list

    val >:: = Test
    val >::: = Suite

    val border =  "============================================"
    val border2 = "--------------------------------------------"

    infix ++
    fun (x, y) ++ (a, b) = (x + a, y + b)

    fun assert_equal print_fn eq_fn (x, y) =
      if eq_fn (x, y) then
        ()
      else
        raise TestFail
          ( case print_fn of
              NONE => "Expected equality"
            | SOME print_fn =>
              "Expected equality: got " ^ print_fn x ^ " and " ^ print_fn y
          )

    fun assert_failure s =
      raise TestFail s

    fun assert_bool s b =
      if b then ()
      else
        raise TestFail s

    fun set_test_name ctx new =
      { path = #path ctx
      , test_name = new
      }

    fun get_test_name (ctx : context) = #test_name ctx

    fun run test =
      let
        fun handle_test ctx (test_name, test_fn) =
          let
            val name = lightblue (#path ctx ^ test_name)
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

        fun run' ctx i test =
          case test of
            Test test => handle_test ctx test
          | Suite (name, tests) =>
              #1
                ( List.foldl
                    (fn (test, (acc, num)) =>
                      let
                        val ctx =
                            { path = #path ctx ^ Int.toString num ^ ":"
                            , test_name = "ERR"
                            }
                      in
                        ( run' ctx num test ++ acc
                        , num + 1
                        )
                      end
                    )
                    ((0, 0), 0)
                    tests
                )

        val timer = Timer.startRealTimer ()
        val (passed, failed) =
          run' base_context 0 test
        val time = Real.fmt (StringCvt.FIX (SOME 2)) (Time.toReal (Timer.checkRealTimer timer))
      in
        ( print (border2 ^ "\n")
        ; print ( "Ran: "
                ^ lightblue (Int.toString (passed + failed))
                ^ (if (passed + failed) = 1 then " test in: " else " tests in: ")
                ^ lightblue time
                ^ " seconds.\n")
        ; if failed = 0 then
            print (green "All tests passed.\n")
          else
            print ((red "FAILED") ^ ": " ^ Int.toString failed ^ " test cases.\n")
        )
      end
  end
