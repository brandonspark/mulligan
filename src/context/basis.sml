(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure SH = SMLSyntaxHelpers

structure Basis :
  sig
    val initial : unit -> SMLSyntax.context

    val bool_ty : SMLSyntax.tyval
    val int_ty : SMLSyntax.tyval
    val string_ty : SMLSyntax.tyval
    val real_ty : SMLSyntax.tyval
    val char_ty : SMLSyntax.tyval
    val unit_ty : SMLSyntax.tyval
    val exn_ty : SMLSyntax.tyval
    val exn_tyid : TyId.t
    val list_tyid : TyId.t
    val option_tyid : TyId.t

    val match_exnid : ExnId.t

    val cont_ty : SMLSyntax.tyval -> SMLSyntax.tyval

    val cons : SMLSyntax.value

    exception Cont of SMLSyntax.value
  end =
  struct
    open SMLSyntax
    open Error

    val sym = Symbol.fromValue

    infix |>
    fun x |> f = f x

    val sym_true = sym "true"
    val sym_false = sym "false"

    val true_val = Vconstr { id = [sym_true], arg = NONE }
    val false_val = Vconstr { id = [sym_false], arg = NONE }
    fun lift_v f x = if f x then true_val else false_val

    val alpha = TVtyvar (sym "'a")
    val beta = TVtyvar (sym "'b")

    fun nullary tyid = TVapp ([], tyid)

    fun forall_none_tyval tyval =
      ( 0
      , fn [] => tyval
       | _ =>
          prog_err "incorrect arity for tycon"
      )
    fun forall_none tyid =
      ( 0
      , fn [] => nullary tyid
       | _ =>
          prog_err "incorrect arity for tycon"
      )


    fun forall_single f =
      ( 1
      , fn tyvals =>
        case tyvals of
          [x] => f x
        | _ => prog_err "incorrect arity to instantiate tycon"
      )

    fun dict_from_list l =
      List.foldl
        (fn ((key, elem), dict) =>
          SymDict.insert dict key elem
        )
        SymDict.empty
        l

    fun tyid_dict_from_list l =
      List.foldl
        (fn ((key, elem), dict) =>
          TyIdDict.insert dict key elem
        )
        TyIdDict.empty
        l

    fun convert b =
      if b then
        Vconstr {id = [sym_true], arg = NONE}
      else
        Vconstr {id = [sym_false], arg = NONE}

    fun poly_eq v1 v2 =
      case (v1, v2) of
        (Vnumber (Int i1), Vnumber (Int i2)) => i1 = i2
      | (Vnumber (Word w1), Vnumber (Word w2)) => w1 = w2
      | (Vstring s1, Vstring s2) => Symbol.eq (s1, s2)
      | (Vchar c1, Vchar c2) => c1 = c2
      | (Vrecord fields1, Vrecord fields2) =>
          let
            fun subset fields1 fields2 =
              List.foldl
                (fn ({lab, value}, acc) =>
                  acc andalso
                  (case List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields2 of
                    NONE => false
                  | SOME {value = value', lab = _} => poly_eq value value'
                  )
                )
                true
                fields1
          in
            subset fields1 fields2 andalso subset fields2 fields1
          end
      | (Vunit, Vunit) => true
      | (Vconstr {id, arg}, Vconstr {id = id', arg = arg'}) =>
          SH.longid_eq (id, id') andalso
          (case (arg, arg') of
            (NONE, NONE) => true
          | (SOME v, SOME v') => poly_eq v v'
          | _ => false
          )
      | (Vselect sym, Vselect sym') => Symbol.eq (sym, sym')
      | (Vtuple vs1, Vtuple vs2) =>
          ListPair.allEq (fn (v, v') => poly_eq v v') (vs1, vs2)
      | (Vlist vs1, Vlist vs2) =>
          ListPair.allEq (fn (v, v') => poly_eq v v') (vs1, vs2)
      | (Vinfix {left, id, right}, Vinfix {left=left', id=id', right=right'}) =>
          poly_eq left left' andalso poly_eq right right' andalso Symbol.eq (id, id')
      | (Vfn _, _) => prog_err "= called on function value"
      | (_, Vfn _) => prog_err "= called on function value"
      | (Vbasis _, _) => prog_err "= called on basis function value"
      | (_, Vbasis _) => prog_err "= called on basis function value"
      | _ => false

    val not_poly_eq = fn v1 => fn v2 => convert (not (poly_eq v1 v2))
    val poly_eq = fn v1 => fn v2 => convert (poly_eq v1 v2)

    (* Types *)

    fun some x = SOME (Symbol.fromValue x)

    val int_tyid = TyId.new (some "int")
    val int_ty = nullary int_tyid
    val string_tyid = TyId.new (some "string")
    val string_ty = nullary string_tyid
    val char_tyid = TyId.new (some "char")
    val char_ty = nullary char_tyid
    val real_tyid = TyId.new (some "real")
    val real_ty = nullary real_tyid
    val unit_tyid =TyId.new (some "unit")
    val unit_ty = nullary unit_tyid

    val exn_tyid = TyId.new (some "exn")

    val option_info as (option_tyid, _, option_cons) =
      let
        val self_tyid = TyId.new (some "option")
      in
        ( self_tyid
        , 1
        , [ ("SOME", forall_single (fn var => TVarrow (var, TVapp ([var],self_tyid))))
          , ("NONE", forall_single (fn var => TVapp ([var], self_tyid)))
          ]
        )
      end
    val order_info as (order_tyid, _, order_cons) =
      let
        val self_tyid = TyId.new (some "order")
      in
        ( self_tyid
        , 0
        , [ ("LESS", forall_none self_tyid)
          , ("EQUAL", forall_none self_tyid)
          , ("GREATER", forall_none self_tyid)
          ]
        )
      end
    val list_info as (list_tyid, _, list_cons) =
      let
        val self_tyid = TyId.new (some "list")
        fun listof var = TVapp ([var], self_tyid)
      in
        ( self_tyid
        , 1
        , [ ("::", forall_single (fn var => TVarrow (TVprod [var, listof var], listof var)))
          , ("nil", forall_single (fn var => listof var))
          ]
        )
      end
    val bool_info as (bool_tyid, _, bool_cons) =
      let
        val self_tyid = TyId.new (some "bool")
      in
        ( self_tyid
        , 0
        , [ ("true", forall_none self_tyid)
          , ("false", forall_none self_tyid)
          ]
        )
      end

    val bool_ty = nullary bool_tyid
    val exn_ty = nullary exn_tyid

    val initial_tynames =
      [ ("int", Scheme (forall_none int_tyid))
      , ("string", Scheme (forall_none string_tyid))
      , ("char", Scheme (forall_none char_tyid))
      , ("real", Scheme (forall_none real_tyid))
      , ("bool", Datatype bool_tyid)
      , ("order", Datatype order_tyid)
      , ("list", Datatype list_tyid)
      , ("option", Datatype option_tyid)
      , ("exn", Scheme (forall_none exn_tyid))
      , ("unit", Scheme (forall_none unit_tyid))
      ]
      |> List.map (fn (id, tyscheme) => (sym id, tyscheme))

    val (initial_dtys, initial_cons_pair) =
      [ option_info
      , order_info
      , list_info
      , bool_info
      ]
      |> List.map
           (fn (tyid, arity, cons) =>
             ( ( tyid
               , { arity = arity
                 , cons =
                     List.map
                       (fn (id, type_scheme) =>
                         {id = sym id, tyscheme = type_scheme}
                       )
                       cons
                 }
               )
             , List.map (fn (x, y) => ((sym x, C tyid), (sym x, (Csign, y)))) cons
             )
           )
      |> ListPair.unzip
      |> (fn (tys, cons_tys_lists) => (tys, List.concat cons_tys_lists))

    val (initial_cons, initial_cons_tys) = ListPair.unzip initial_cons_pair

    (* Exceptions *)

    fun mk_exn name opt =
      case opt of
        NONE => (name, ExnId.new (SOME (Symbol.fromValue name)), exn_ty)
      | SOME tyval => (name, ExnId.new (SOME (Symbol.fromValue name)), TVarrow (tyval, exn_ty))

    val fail_info as (fail_name, fail_exnid, fail_ty) =
      mk_exn "Fail" (SOME string_ty)
    val bind_info as (bind_name, bind_exnid, bind_ty) =
      mk_exn "Bind" NONE
    val match_info as (match_name, match_exnid, match_ty) =
      mk_exn "Match" NONE
    val div_info as (div_name, div_exnid, div_ty) =
      mk_exn "Div" NONE
    val subscript_info as (subscript_name, subscript_exnid, subscript_ty) =
      mk_exn "Subscript" NONE

    val (initial_exns, initial_exns_tys) =
      [ fail_info
      , bind_info
      , match_info
      , div_info
      , subscript_info
      ]
      |> List.map
           (fn (name, value, tyval) =>
             ( (sym name, E value)
             , (sym name, (Esign, forall_none_tyval tyval))
             )
           )
      |> ListPair.unzip

    (* Values *)

    val cons =
      { function =
        (fn Vtuple [x, Vlist xs] => Vlist (x::xs)
        | _ => eval_err "invalid args to ::"
        )
      , name = sym "::"
      , is_infix = true
      } |> Vbasis

    (* TODO: word stuff *)
    val (initial_values, initial_values_tys) =
      [ ( "+"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 + i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 + r2))
          | _ => eval_err "invalid args to +"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], int_ty)
        )
      , ( "-"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 - i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 - r2))
          | _ => eval_err "invalid args to -"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], int_ty)
        )
      , ( "*"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 * i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 * r2))
          | _ => eval_err "invalid args to *"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], int_ty)
        )
      , ( "div"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 div i2))
          | _ => eval_err "invalid args to div"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], int_ty)
        )
      , ( ">="
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => lift_v (op>=) (i1, i2)
          | _ => eval_err "invalid args to >="
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], bool_ty)
        )
      , ( "<="
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => lift_v (op<=) (i1, i2)
          | _ => eval_err "invalid args to <="
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], bool_ty)
        )
      , ( "<"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => lift_v (op<) (i1, i2)
          | _ => eval_err "invalid args to <"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], bool_ty)
        )
      , ( ">"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => lift_v (op>) (i1, i2)
          | _ => eval_err "invalid args to >"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], bool_ty)
        )
      , ( "mod"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 mod i2))
          | _ => eval_err "invalid args to div"
          )
        , true
        , TVarrow (TVprod [int_ty, int_ty], int_ty)
        )
      , ( "/"
        , (fn Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 / r2))
          | _ => eval_err "invalid args to /"
          )
        , true
        , TVarrow (TVprod [real_ty, real_ty], real_ty)
        )
      , ( "not"
        , (fn Vconstr {id = [x], arg = NONE} =>
            if Symbol.eq (x, sym_true) then
              Vconstr {id = [sym_false], arg = NONE}
            else if Symbol.eq (x, sym_false) then
              Vconstr {id = [sym_true], arg = NONE}
            else
              eval_err "invalid arg to `not`"
          | _ => eval_err "invalid arg to `not`"
          )
        , false
        , TVarrow (bool_ty, bool_ty)
        )
      , ( "^"
        , (fn Vtuple [Vstring s1, Vstring s2] =>
              Vstring (Symbol.fromValue (Symbol.toValue s1 ^ Symbol.toValue s2))
          | _ => eval_err "invalid args to ^"
          )
        , true
        , TVarrow (TVprod [string_ty, string_ty], string_ty)
        )
      , ( "chr"
        , (fn Vnumber (Int i) => Vchar (Char.chr i)
          | _ => eval_err "invalid args to `chr`"
          )
        , false
        , TVarrow (int_ty, char_ty)
        )
      , ( "explode"
        , (fn Vstring s => Vlist (List.map Vchar (String.explode (Symbol.toValue s)))
          | _ => eval_err "invalid args to `explode`"
          )
        , false
        , TVarrow (string_ty, TVapp ([string_ty], list_tyid))
        )
      , ( "floor"
        , (fn Vnumber (Real r) => Vnumber (Int (Real.floor r))
          | _ => eval_err "invalid args to `floor`"
          )
        , false
        , TVarrow (real_ty, int_ty)
        )
      , ( "ord"
        , (fn Vchar c => Vnumber (Int (Char.ord c))
          | _ => eval_err "invalid arg to `ord`"
          )
        , false
        , TVarrow (char_ty, int_ty)
        )
      , ( "real"
        , (fn Vnumber (Int i) => Vnumber (Real (real i))
          | _ => eval_err "invalid arg to `real`"
          )
        , false
        , TVarrow (int_ty, real_ty)
        )
      , ( "size"
        , (fn Vstring s => Vnumber (Int (String.size (Symbol.toValue s)))
          | _ => eval_err "invalid arg to `size`"
          )
        , false
        , TVarrow (string_ty, int_ty)
        )
      , ( "str"
        , (fn Vchar c => Vstring (Symbol.fromValue (str c))
          | _ => eval_err "invalid arg to `str`"
          )
        , false
        , TVarrow (char_ty, string_ty)
        )
      , ( "round"
        , (fn Vnumber (Real r) => Vnumber (Int (round r))
          | _ => eval_err "invalid arg to `round`"
          )
        , false
        , TVarrow (real_ty, int_ty)
        )
      , ( "substring"
        , (fn Vtuple [Vstring s, Vnumber (Int i1), Vnumber (Int i2)] =>
            ( Vstring
              (Symbol.fromValue (String.substring (Symbol.toValue s, i1, i2))) handle Subscript =>
                raise Context.Raise ([Symbol.fromValue "Subscript"], subscript_exnid, NONE)
            )
          | _ => eval_err "invalid args to `substring`"
          )
        , false
        , TVarrow (TVprod [string_ty, int_ty, int_ty], string_ty)
        )
      , ( "~"
        , (fn Vnumber (Int i) => Vnumber (Int (~i))
          | Vnumber (Real i) => Vnumber (Real (~i))
          | _ => eval_err "invalid arg to `~`"
          )
        , false
        , TVarrow (int_ty, int_ty)
        )
      ]
      |> List.map
           (fn (name, value, is_infix, tyval) =>
             ( (sym name, V (Vbasis {name = sym name, function = value, is_infix = is_infix}))
             , (sym name, (Vsign, forall_none_tyval tyval))
             )
           )
      |> (fn l =>
          ( [ ( "="
              , (fn Vtuple [left, right] => poly_eq left right
                | _ => eval_err "invalid arg to `=`"
                )
              , true
              , ( Vsign
                , SH.guard_tyscheme
                  (1, fn [tyval] => TVarrow (TVprod [tyval, tyval], bool_ty)
                     | _ => raise Fail "impossible")
                )
              ),
              ( "<>"
              , (fn Vtuple [left, right] => not_poly_eq left right
                | _ => eval_err "invalid arg to `=`"
                )
              , true
              , ( Vsign
                , SH.guard_tyscheme
                  (1, fn [tyval] => TVarrow (TVprod [tyval, tyval], bool_ty)
                     | _ => raise Fail "impossible")
                )
              )
               (* TODO: equality types *)
            ] |> List.map
                   (fn (name, value, is_infix, tyinfo) =>
                     ( (sym name, V (Vbasis {name = sym name, function = value, is_infix = is_infix}))
                     , (sym name, tyinfo)
                     )
                   )
          ) @ l
        )
      |> ListPair.unzip

    (* Modules *)

    exception Cont of SMLSyntax.value

    val (cont_ty, cont_mod) =
      let
        val cont_absid = AbsId.new (some "cont")
        val cont_ty = fn x => TVabs ([x], cont_absid)
        val (identdict, valtydict) =
          [ ( "callcc"
            , (fn value => raise Cont value)
            , false
            , forall_single (fn x => TVarrow (TVarrow (cont_ty x, x), x))
            )
          , ( "throw"
            , (fn Vbasis {function, name, is_infix} =>
                Vbasis { function = fn x => function x
                       , name = sym ("throw[" ^ Symbol.toValue name ^ "]")
                       , is_infix = is_infix
                       }
              | _ => raise Fail "should not happen, error in throw"
              )
            , false
            , SH.guard_tyscheme
                ( 2
                , fn [a, b] => TVarrow (cont_ty a, TVarrow (a, b))
                 | _ => raise Fail "should not happen, arity mismatch in throw"
                )
            )
          ]
          |> List.map (fn (x, y, is_infix, z) =>
              ( (sym x, V (Vbasis {name = sym x, function = y, is_infix = is_infix}))
              , (sym x, (Vsign, z))
              )
             )
          |> ListPair.unzip
      in
        ( cont_ty
        , Scope
            { identdict = dict_from_list identdict
            , valtydict = dict_from_list valtydict
            , moddict = SymDict.empty
            , infixdict = SymDict.empty
            , tynamedict =
                SymDict.singleton (sym "cont")
                  (Scheme (forall_single cont_ty))
            }
        )
      end

    val initial_mods =
      [ ("Cont", cont_mod)
      ]
      |> List.map (fn (x, y) => (sym x, y))

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

    fun sym_from_list l =
      List.foldl
        (fn (x, acc) =>
          SymSet.insert acc x
        )
        SymSet.empty
        l

    val initial_scope =
      Scope
        { identdict = dict_from_list (initial_values @ initial_cons @ initial_exns)
        , valtydict =
            dict_from_list
              (initial_values_tys @ initial_cons_tys @ initial_exns_tys)
        , moddict = dict_from_list initial_mods
        , infixdict = dict_from_list initial_infix
        , tynamedict = dict_from_list initial_tynames
        }

    val initial_dtydict = tyid_dict_from_list initial_dtys

    fun initial () : SMLSyntax.context =
      { scope = initial_scope
      , outer_scopes = []
      , dtydict = ref initial_dtydict
      , sigdict = SymDict.empty
      , functordict = SymDict.empty
      , tyvars = SymSet.empty
      , hole_print_fn = fn () => PrettySimpleDoc.text TerminalColors.white "<hole>"
      , settings =
          { break_assigns = ref SymSet.empty
          , substitute = ref true
          , print_all = ref false 
          , print_dec = ref true
          , print_depth = ref 1
          , pause_currying = ref false
          , pause_app = ref true
          , pause_arithmetic = ref true
          }
      , abstys = AbsIdDict.empty
      }
  end
