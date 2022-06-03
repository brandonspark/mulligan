
structure SymbolOrdered =
  struct
    type t = SMLSyntax.symbol

    val compare = Symbol.compare
    val eq = Symbol.eq
  end
structure SymDict = RedBlackDict(structure Key = SymbolOrdered)

structure Context :
  sig
    type symbol = SMLSyntax.symbol

    type 'a dict = 'a SymDict.dict

    datatype data =
        SIG of SMLSyntax.signat
      | VAL of SMLSyntax.exp
      | TY of { tyvars : symbol list
              , tycon : symbol
              }
      | CON
      | EXN of symbol
      | MOD of data dict

    datatype infixity = LEFT | RIGHT

    (* The outer scopes are a list, in order of ascending outwardness.
     * Each scope maps to the stuff in it, and all things in scope are within
     * the current value of outer_scopes.
     *)
    type t

    (* Use when entering the scope of a module. *)
    val enter_scope : t -> t

    (* Use when exiting the scope of a module. *)
    val exit_scope : string -> t -> t

    (* Add a signature to the current scope. *)
    val add_topdec : t -> SMLSyntax.topdec -> t
    val add_dec : t -> SMLSyntax.dec -> t

    val add_bindings : t -> (SMLSyntax.symbol * SMLSyntax.exp) list -> t

    val add_rec_valbinds : t -> SMLSyntax.valbinds -> t

    val add_fvalbinds : t -> SMLSyntax.fvalbinds -> t

    val match_against :
      t -> {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list ->
        (t * scope) option -> SMLSyntax.exp -> SMLSyntax.exp * t
  end =
  struct
    open SMLSyntax

    infix |>
    fun x |> f = f x

    fun concatMap f = List.concat o List.map f

    type symbol = symbol

    type 'a dict = 'a SymDict.dict

    datatype data =
        SIG of signat
      | VAL of exp
      | TY of { tyvars : symbol list
              , tycon : symbol
              }
      | CON
      | EXN of symbol
      | MOD of scope
    withtype scope = data dict

    (* TODO: separate namespaces *)
    type t = scope * scope list

    datatype infixity = LEFT | RIGHT

    fun get_last l =
      List.nth (l, List.length l - 1)


    fun longid_eq (l1, l2) =
      ListPair.allEq Symbol.eq (l1, l2)

    exception Mismatch of string

    fun match_pat pat v =
      case (pat, v) of
        (Pnumber i1, Enumber (Int i2)) =>
          if i1 = i2 then
            []
          else
            raise Mismatch "num pats did not match"
      | (Pword s, Enumber (Word s')) =>
          if Symbol.toValue s = s' then
            []
          else
            raise Mismatch "word pats did not match"
      | (Pstring s, Estring s') =>
          if Symbol.eq (s, s') then
            []
          else
            raise Mismatch "string pats did not match"
      | (Pchar c, Echar c') =>
          if c = c' then
            []
          else
            raise Mismatch "char pats did not match"
      | (Pwild, _) => []
      | (Pident {opp, id}, _) => [(id, v)]
      | (Pconstr {opp, id}, Econstr {opp = _, id = id'}) =>
          if longid_eq (id, id') then
            []
          else
            raise Mismatch "constr pats did not match"
      | (Precord patrows, Erecord fields) =>
          List.foldl
            (fn (patrow, acc) =>
              case patrow of
                PRellipsis => acc
              | PRlab {lab, pat} =>
                  (case
                    List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields
                  of
                    NONE => raise Mismatch "exp did not match patrow"
                  | SOME {lab, exp} => match_pat pat exp @ acc
                  )
              | PRas {id, ty, aspat} =>
                  (case aspat of
                    NONE => (id, v) :: acc
                  | SOME pat => (id, v) :: match_pat pat v @ acc
                  )
            )
            []
            patrows
      | (Pparens pat, v) => match_pat pat v
      | (Punit, Eunit) => []
      | (Ptuple pats, Etuple exps) =>
          ListPair.zipEq (pats, exps)
          |> concatMap (fn (x, y) => match_pat x y)
      | (Plist pats, Elist exps) =>
          ListPair.zipEq (pats, exps)
          |> concatMap (fn (x, y) => match_pat x y)
      | (Por pats, _) =>
          ( case
              List.foldl
                (fn (pat, NONE) =>
                  SOME (match_pat pat v)
                  handle Mismatch _ => NONE
                )
                NONE
                pats
            of
              NONE => raise Mismatch "failed to match any or cases"
            | SOME res => res
          )
      | (Papp {opp, id, atpat}, Eapp {left = Econstr {opp = _, id = id'}, right}) =>
          if longid_eq (id, id') then
            match_pat atpat right
          else
            raise Mismatch "failed to match constructors with args"
      | (Pinfix {left, id, right}, Einfix {left = left', id = id', right = right'}) =>
          (* TODO: constructors need more than just name equality *)
          if Symbol.eq (id, id') then
            match_pat left left' @ match_pat right right'
          else
            raise Mismatch "idents not equal"
      | (Ptyped {pat, ty}, _) => match_pat pat v
      | (Playered {opp, id, ty, aspat}, _) =>
          (id, v) :: match_pat aspat v

    fun find_match matches v =
      List.foldl
        (fn ({pat, exp}, NONE) =>
          ( SOME (match_pat pat v, exp)
            handle Mismatch _ => NONE
          )
        | ({pat, exp}, SOME ans) => SOME ans
        )
        NONE
        matches

    exception Raise of exp

    fun ctx_rec scope =
      SymDict.map
        (fn VAL (Efn (matches, E, VE)) => VAL (Efn (matches, E, scope))
        | other => other
        )

    (* Evaluate each expression to a "normal form" for the value.
    fun evaluate ctx exp =
      case exp of
        ( Enumber _
        | Estring _
        | Echar _
        | Eunit _
        | Econstr _
        | Eselect _
        ) => exp
      | Erecord fields =>
          Erecord
            (List.map (fn {lab, exp} => {lab = lab, exp = evaluate ctx exp}) fields)
      | Eident {opp, id} => (* TODO: lookup *)

      (* Due to evaluation order, these should raise if any of the constituent
       * expressions raise an exception. Same with looping forever.
       *)
      | Etuple exps => Etuple (List.map (evaluate ctx) exps)
      | Elist exps => Elist (List.map (evaluate ctx) exps)
      | Eseq exps => Eseq (List.map (evaluate ctx) exps)
      | Elet {dec, exps} =>
          let
            val new_ctx = add_dec ctx dec
            val new_exps = List.map (subst new_ctx) exps
          in
            List.map (evaluate new_ctx) new_exps
            |> get_last
          end
      | Eparens exp => evaluate ctx exp
      | Eapp {left, right} =>
          (case (evaluate ctx left, evaluate ctx right) of
            (constr as Econstr {id, ...}, v) =>
              Eapp { left = constr
                   , right = v
                   }
          | (Eselect sym, Erecord fields) =>
              List.find
                (fn {lab, ...} => Symbol.eq (lab, sym))
                field
              |> #exp
              |> evaluate ctx
          | (Efn (matches, E, VE), v) =>
              (* TODO *)
              let
                val (substs, exp) = find_match matches v
                val new_ctx = merge E (ctx_rec VE)
              in
                (* CHECK: This should not induce capture, because we're using
                 * the function's own closure.
                 *)
                evaluate new_ctx exp
              end
          | _ => raise Fail "invalid form for application"
          )
      | Einfix {left, id, right} =>
         Einfix { left = evaluate ctx left
                , id = id
                , right = evaluate ctx right
                }
      | Etyped {exp, ty} => evaluate ctx exp
      | Eandalso {left, right} =>
          (case (evaluate ctx left, evaluate ctx right) of
            (Econstr {id = [s], ...}, Econstr {id = [s'], ...}) =>
              if not (is_bool s andalso is_bool s') then
                raise Fail "andalso on non-bools"
              else if ( Symbol.eq (s, Symbol.fromValue "true")
                andalso Symbol.eq (s', Symbol.fromValue "true") ) then
                Econstr { opp = false, id = [Symbol.fromValue "true"] }
              else
                Econstr { opp = false, id = [Symbol.fromValue "false"] }
          | _ => raise Fail "andalso on non-bools"
          )
      | Eorelse {left, right} =>
          (case (evaluate ctx left, evaluate ctx right) of
            (Econstr {id = [s], ...}, Econstr {id = [s'], ...}) =>
              if not (is_bool s andalso is_bool s') then
                raise Fail "orelse on non-bools"
              else if ( Symbol.eq (s, Symbol.fromValue "false")
                andalso Symbol.eq (s', Symbol.fromValue "false") ) then
                Econstr { opp = false, id = [Symbol.fromValue "false"] }
              else
                Econstr { opp = false, id = [Symbol.fromValue "true"] }
          | _ => raise Fail "andalso on non-bools"
          )
      | Ehandle {exp, matches} =>
          evaluate ctx exp
          handle Raise exp =>
            let
              val (substs, exp) = find_match matches exp
              val new_ctx = add_substs ctx substs
            in
              (* CHECK: This should not induce capture.
               *)
              evaluate new_ctx exp
            end
      | Eraise exp =>
          (case evaluate ctx exp of
            (constr as Econstr _) => raise Raise const
          | app as Eapp { left = Econstr _, right = _ } =>
              raise Raise app
          | _ => raise Fail "invalid raise"
          )
      | Eif {exp1, exp2, exp3} =>
          (case evaluate ctx exp1 of
            Econstr {opp, id} =>
              if longid_eq [Symbol.fromValue "true"] then
                evaluate ctx exp2
              else if longid_eq [Symbol.fromValue "false"] then
                evaluate ctx exp3
              else
                raise Fail "if given non-bool"
          | _ => raise Fail "if given non-bool"
          )
      | Ewhile { exp1, exp2 } =>
          while
            (case evaluate ctx exp1 of
              Econstr {opp, id} =>
                if longid_eq [Symbol.fromValue "true"] then
                  true
                else if longid_eq [Symbol.fromValue "false"] then
                  false
                else
                  raise Fail "if given non-bool"
            | _ => raise Fail "if given non-bool"
            )
          do
            evaluate ctx exp2
      | Ecase {exp, matches} =>
          let
            val (substs, exp) = find_match matches exp
            val new_ctx = add_substs ctx substs
          in
            (* CHECK: This should not induce capture
             *)
            evaluate new_ctx exp
          end
      (* Leave it as is. *)
      | Efn _ => exp
    *)

    fun enter_scope (scope, ctx) = (SymDict.empty, scope :: ctx)

    fun exit_scope name (scope, ctx) =
      case ctx of
        [] => raise Fail "exiting scope without outer scope"
      | outer'::scopes =>
          (SymDict.insert outer' name (MOD scope), scopes)

    (* fun match_ctx ctx pat exp =
      let
        val substs = match_pat pat (evaluate ctx exp)
      in
        List.foldl
          (fn ((id, v), ctx) =>
            add_bind ctx id v
          )
          ctx
          substs
      end
    *)

    fun dict_from_list l =
      List.foldl
        (fn ((key, elem), dict) =>
          SymDict.insert dict key elem
        )
        SymDict.empty
        l

    fun merge_scope (cur_scope, ctx) scope =
      ( SymDict.union cur_scope scope
          (fn (key, val1, val2) => val2)
      , ctx
      )


    (*
    fun add_dec ctx dec =
      case dec of
        Dval {valbinds, ...} =>
          List.foldl
            (fn ({recc, pat, exp}, (acc, flag)) =>
              ( match_pat pat (evaluate ctx exp) @ acc
              , flag orelse recc
              )
            )
            ([], false)
            valbinds
          |> (fn (pairs, flag) => (dict_from_list pairs, flag))
          |> (fn (scope, flag) =>
              if flag then ctx_rev scope
              else scope
             )
          |> merge_scope ctx
      | Dfun {fvalbinds, ...} =>
          raise Fail "test"

    fun add_strdec (scope, ctx) strdec =
      case strdec of
        DMdec dec => add_dec (scope, ctx) dec
      | DMstruct => raise Fail "TODO"

    fun add_topdec (scope, ctx) topdec =
      case topdec of
        Strdec strdec => raise Fail "TODO"
    *)
  end
