
structure Basis :
  sig
    val initial : SMLSyntax.context
  end =
  struct
    open SMLSyntax
    open Value

    val sym = Symbol.fromValue

    infix |>
    fun x |> f = f x

    val sym_true = sym "true"
    val sym_false = sym "false"

    fun dict_from_list l =
      List.foldl
        (fn ((key, elem), dict) =>
          SymDict.insert dict key elem
        )
        SymDict.empty
        l


    (* TODO: word stuff *)
    val initial_values =
      [ ( "+"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 + i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 + r2))
          | _ => raise Fail "invalid args to +"
          )
        )
      , ( "-"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 - i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 - r2))
          | _ => raise Fail "invalid args to -"
          )
        )
      , ( "*"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 * i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 * r2))
          | _ => raise Fail "invalid args to *"
          )
        )
      , ( "div"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 div i2))
          | _ => raise Fail "invalid args to div"
          )
        )
      , ( "mod"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 mod i2))
          | _ => raise Fail "invalid args to div"
          )
        )
      , ( "/"
        , (fn Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 / r2))
          | _ => raise Fail "invalid args to /"
          )
        )
      , ( "not"
        , (fn Vconstr {id = [x], arg = NONE} =>
            if Symbol.eq (x, sym_true) then
              Vconstr {id = [sym_false], arg = NONE}
            else if Symbol.eq (x, sym_false) then
              Vconstr {id = [sym_true], arg = NONE}
            else
              raise Fail "invalid arg to `not`"
          | _ => raise Fail "invalid arg to `not`"
          )
        )
      , ( "^"
        , (fn Vtuple [Vstring s1, Vstring s2] =>
              Vstring (Symbol.fromValue (Symbol.toValue s1 ^ Symbol.toValue s2))
          | _ => raise Fail "invalid args to ^"
          )
        )
      , ( "chr"
        , (fn Vnumber (Int i) => Vchar (Char.chr i)
          | _ => raise Fail "invalid args to `chr`"
          )
        )
      , ( "explode"
        , (fn Vstring s => Vlist (List.map Vchar (String.explode (Symbol.toValue s)))
          | _ => raise Fail "invalid args to `explode`"
          )
        )
      , ( "floor"
        , (fn Vnumber (Real r) => Vnumber (Int (Real.floor r))
          | _ => raise Fail "invalid args to `floor`"
          )
        )
      , ( "ord"
        , (fn Vchar c => Vnumber (Int (Char.ord c))
          | _ => raise Fail "invalid arg to `ord`"
          )
        )
      , ( "real"
        , (fn Vnumber (Int i) => Vnumber (Real (real i))
          | _ => raise Fail "invalid arg to `real`"
          )
        )
      , ( "size"
        , (fn Vstring s => Vnumber (Int (String.size (Symbol.toValue s)))
          | _ => raise Fail "invalid arg to `size`"
          )
        )
      , ( "str"
        , (fn Vchar c => Vstring (Symbol.fromValue (str c))
          | _ => raise Fail "invalid arg to `str`"
          )
        )
      , ( "round"
        , (fn Vnumber (Real r) => Vnumber (Int (round r))
          | _ => raise Fail "invalid arg to `round`"
          )
        )
      , ( "substring"
        , (fn Vtuple [Vstring s, Vnumber (Int i1), Vnumber (Int i2)] =>
            ( Vstring
              (Symbol.fromValue (String.substring (Symbol.toValue s, i1, i2))) handle Subscript =>
                raise Context.Raise (Vconstr {id = [Symbol.fromValue "Subscript"], arg = NONE})
            )
          | _ => raise Fail "invalid args to `substring`"
          )
        )
      , ( "~"
        , (fn Vnumber (Int i) => Vnumber (Int (~i))
          | Vnumber (Real i) => Vnumber (Real (~i))
          | _ => raise Fail "invalid arg to `~`"
          )
        )
      ]
      |> List.map (fn (x, y) => (sym x, Vbasis { name = sym x, function = y }))

    val initial_cons =
      [ "SOME", "NONE", "true", "false", "::", "LESS", "EQUAL", "GREATER",
         "nil", "Match", "Bind", "Div", "Fail"]
      |> List.map sym

    val initial_exns =
      [ "Fail", "Bind", "Match", "Div", "Subscript" ]
      |> List.map sym

    val initial_mods = []

    val initial_infix =
      [ ( "div", (LEFT, 7) )
      , ( "mod", (LEFT, 7) )
      , ( "*", (LEFT, 7) )
      , ( "/", (LEFT, 7) )

      , ( "+", (LEFT, 6) )
      , ( "-", (LEFT, 6) )
      , ( "^", (LEFT, 6) )

      , ( "::", (RIGHT, 5) )
      , ( "@", (RIGHT, 5) )

      , ( "<>", (LEFT, 4) )
      , ( "=", (LEFT, 4) )
      , ( "<", (LEFT, 4) )
      , ( ">", (LEFT, 4) )
      , ( "<=", (LEFT, 4) )
      , ( ">=", (LEFT, 4) )

      , ( ":=", (LEFT, 3) )
      , ( "o", (LEFT, 3) )

      , ( "before", (LEFT, 0) )
      ]
      |> List.map (fn (x, y) => (sym x, y))

    val initial_tys =
      [ ( "order"
        , ( 0
          , [ ("LESS", NONE)
            , ("GREATER", NONE)
            , ("EQUAL", NONE)
            ]
          )
        )
      , ( "option"
        , ( 1
          , [ ("SOME", SOME (Ttyvar (sym "'a")))
            , ("NONE", NONE)
            ]
          )
        )
      , ( "list"
        , ( 1
          , [ ("::", SOME ( Tprod [ Ttyvar (sym "'a")
                                  , Tapp ([Ttyvar (sym "'a")], [sym "list"])
                                  ]
                          )
              )
            , ("nil", NONE)
            ]
          )
        )
      , ( "unit"
        , ( 0
          , [ ("()", NONE) ]
          )
        )
        (* TODO: int, real, types with weird constructors? *)
      ]
      |> List.map
           (fn (tycon, (arity, cons)) =>
             (sym tycon
             , { arity = arity
               , cons = List.map (fn (x, y) => { id = sym x, ty = y }) cons
               }
             )
           )

    fun sym_from_list l =
      List.foldl
        (fn (x, acc) =>
          SymSet.insert acc x
        )
        SymSet.empty
        l

    val initial_scope =
      Scope
        { valdict = dict_from_list initial_values
        , condict = sym_from_list initial_cons
        , exndict = sym_from_list initial_exns
        , moddict = dict_from_list initial_mods
        , infixdict = dict_from_list initial_infix
        , tydict = dict_from_list initial_tys
        }


    val initial =
      { scope = initial_scope
      , outer_scopes = []
      , sigdict = SymDict.empty
      , functordict = SymDict.empty
      , hole_print_fn = fn () => PrettySimpleDoc.text TerminalColors.white "<hole>"
      , settings =
          { break_assigns = ref SymSet.empty
          , substitute = ref true
          , step_app = ref true
          , step_arithmetic = ref false
          , print_dec = ref false
          , print_depth = ref 1
          }
      }
  end
