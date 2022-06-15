
structure Value :
  sig

    (* Value stuff. *)

    type t = SMLSyntax.context
    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type sigval = SMLSyntax.sigval

    val value_to_exp : value -> SMLSyntax.exp
    val exp_to_value : t -> SMLSyntax.exp -> value option

    val apply_fn :
           {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list * t * scope option
        -> value
        -> (SMLSyntax.symbol * value) list * t * SMLSyntax.exp

    exception Mismatch of string

    val match_against :
         t
      -> {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list
      -> value
      -> (SMLSyntax.symbol * value) list * t * SMLSyntax.exp

    val match_pat : t -> SMLSyntax.pat -> value -> (SMLSyntax.symbol * value) list

    val evaluate_signat : t -> SMLSyntax.signat -> sigval

  end =
  struct
    open PrettyPrintContext
    open SMLSyntax
    open Context
    open Error

    infix |>
    fun x |> f = f x

    (* VALUE STUFF *)

    (* This is used purely so we acn have an exp for the surrounding context of
     * the focused expression. These are for compatibility with the pretty
     * printer, not for any computational purpose.
     *)
    fun value_to_exp value =
      case value of
        Vnumber num => Enumber num
      | Vstring s => Estring s
      | Vchar c => Echar c
      | Vrecord fields =>
          Erecord
            (List.map
              (fn {lab, value} =>
                { lab = lab, exp = value_to_exp value }
              )
              fields
            )
      | Vunit => Eunit
      | Vconstr {id, arg} =>
          (case arg of
            NONE => Eident {opp = false, id = id}
          | SOME arg => Eapp { left = Eident {opp = false, id = id}
                             , right = value_to_exp arg
                             }
          )
      | Vselect sym => Eselect sym
      | Vtuple values => Etuple (List.map value_to_exp values)
      | Vlist values => Elist (List.map value_to_exp values)
      | Vinfix {left, id, right} =>
          Einfix { left = value_to_exp left
                 , id = id
                 , right = value_to_exp right
                 }
      | Vfn {matches, env, rec_env, ...} => Efn (matches, SOME env)
      (* For basis values *)
      | Vbasis {name, function} => Eident {opp = false, id = [name]}

    fun opt_all_list l =
      List.foldr
        (fn (elem, NONE) => NONE
        | (NONE, _) => NONE
        | (SOME elem, SOME acc) => SOME (elem::acc)
        )
        (SOME [])
        l

    fun exp_to_value ctx exp =
      case exp of
        Enumber num => SOME (Vnumber num)
      | Estring s => SOME (Vstring s)
      | Echar c => SOME (Vchar c)
      | Eselect sym => SOME (Vselect sym)
      | Eunit => SOME Vunit
      | Eident {id, ...} =>
          if Context.is_con ctx id then
            SOME (Vconstr {id = id, arg = NONE})
          else
            SOME (Context.get_val ctx id)
      | Efn (matches, SOME env) => raise Fail "should probably not happen"
      | Efn (matches, NONE) =>
          (* The only reason to do an exp to value on a verbatim Efn is if it
           * was a bona fide literal lambda expression, ergo non-recursive.
           * So it should be OK to set rec_env and break to NONE.
           *)
          SOME (Vfn {matches = matches, env = ctx, rec_env = NONE, break = ref NONE})
      | Erecord fields =>
          List.map
            (fn {lab, exp} =>
              Option.map
                (fn value => {lab = lab, value = value})
                (exp_to_value ctx exp)
            )
            fields
          |> opt_all_list
          |> Option.map (fn fields => Vrecord fields)
      | Elist exps =>
          List.map
            (exp_to_value ctx)
            exps
          |> opt_all_list
          |> Option.map (fn fields => Vlist fields)
      | Etuple exps =>
          List.map
            (exp_to_value ctx)
            exps
          |> opt_all_list
          |> Option.map (fn fields => Vtuple fields)

      | Eparens exp => exp_to_value ctx exp
      | Eapp {left = Eident {opp, id}, right} =>
          if Context.is_con ctx id then
            Option.map
              (fn exp => Vconstr {id = id, arg = SOME exp})
              (exp_to_value ctx right)
          else
            NONE
      | Einfix {left, id, right} =>
          if Context.is_con ctx [id] then
            exp_to_value ctx left
            |> Option.map (fn left =>
                 exp_to_value ctx right
                 |> Option.map (fn right =>
                    Vinfix {left = left, id = id, right = right}
                  )
               )
            |> Option.join
          else
            NONE
      | Etyped {exp, ...} => exp_to_value ctx exp
      | ( Eseq _
        | Eapp _
        | Elet _
        | Eandalso _
        | Eorelse _
        | Ehandle _
        | Eraise _
        | Eif _
        | Ewhile _
        | Ecase _ ) => NONE
      | Ehole => raise Fail "shouldn't happen?"


    exception Mismatch of string

    fun match_pat ctx pat v =
      case (pat, v) of
        (Pnumber i1, Vnumber (Int i2)) =>
          if i1 = i2 then
            []
          else
            raise Mismatch "num pats did not match"
      | (Pword s, Vnumber (Word s')) =>
          if Symbol.toValue s = s' then
            []
          else
            raise Mismatch "word pats did not match"
      | (Pstring s, Vstring s') =>
          if Symbol.eq (s, s') then
            []
          else
            raise Mismatch "string pats did not match"
      | (Pchar c, Vchar c') =>
          if c = c' then
            []
          else
            raise Mismatch "char pats did not match"
      | (Pwild, _) => []
      | (Pident {opp, id}, Vconstr {id = id', arg = NONE}) =>
          if Context.is_con ctx id then
            if longid_eq (id, id') then
              []
            else
              raise Mismatch "constructors did not match"
          else
            (case id of
              [id] => [(id, v)]
            | _ => raise Mismatch "pattern matching against nonexistent constr"
            )
      | (Pident {opp, id}, _) =>
          if Context.is_con ctx id then
            raise Mismatch "cannot match nullary constructor"
          else
            (case id of
              [id] => [(id, v)]
            | _ => raise Mismatch "pattern matching against nonexistent constr"
            )
      | (Precord patrows, Vrecord fields) =>
          List.foldl
            (fn (patrow, acc) =>
              case patrow of
                PRellipsis => acc
              | PRlab {lab, pat} =>
                  (case
                    List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields
                  of
                    NONE => raise Mismatch "val did not match patrow"
                  | SOME {lab, value} => match_pat ctx pat value @ acc
                  )
              | PRas {id, ty, aspat} =>
                  (case aspat of
                    NONE => (id, v) :: acc
                  | SOME pat => (id, v) :: match_pat ctx pat v @ acc
                  )
            )
            []
            patrows
      | (Pparens pat, v) => match_pat ctx pat v
      | (Punit, Vunit) => []
      | (Ptuple pats, Vtuple vals) =>
          ListPair.zipEq (pats, vals)
          |> (List.concat o List.map (fn (x, y) => match_pat ctx x y))
      | (Plist pats, Vlist vals) =>
          ListPair.zipEq (pats, vals)
          |> (List.concat o List.map (fn (x, y) => match_pat ctx x y))
      | (Por pats, _) =>
          ( case
              List.foldl
                (fn (pat, NONE) =>
                  ( SOME (match_pat ctx pat v)
                    handle Mismatch _ => NONE
                  )
                | (_, SOME ans) => SOME ans
                )
                NONE
                pats
            of
              NONE => raise Mismatch "failed to match any or cases"
            | SOME res => res
          )
      | (Papp {opp, id, atpat}, Vconstr {id = id', arg = SOME v}) =>
          if longid_eq (id, id') then
            match_pat ctx atpat v
          else
            raise Mismatch "failed to match constructors with args"
      | (Pinfix {left, id, right}, Vinfix {left = left', id = id', right = right'}) =>
          (* TODO: constructors need more than just name equality *)
          if Symbol.eq (id, id') then
            match_pat ctx left left' @ match_pat ctx right right'
          else
            raise Mismatch "idents not equal"
      | (Ptyped {pat, ty}, _) => match_pat ctx pat v
      | (Playered {opp, id, ty, aspat}, _) =>
          (id, v) :: match_pat ctx aspat v
      | _ => raise Mismatch "pats don't match"

    fun match_against ctx matches value =
      let
        val (bindings, exp) =
          case
            List.foldl
              (fn ({exp, pat}, NONE) =>
                ( SOME (match_pat ctx pat value, exp)
                  handle Mismatch _ => NONE
                )
              | ({exp, pat}, SOME ans) => SOME ans
              )
              NONE
              matches
          of
            NONE => raise Mismatch "failed to match"
          | SOME ans => ans
      in
        (bindings, add_bindings ctx bindings, exp)
      end

    val match_pat = fn ctx => fn pat => fn value =>
      match_pat ctx pat value

    fun apply_fn (matches, E, VE) value =
      match_against
        (Context.merge_scope E (Context.ctx_rec (Option.getOpt (VE, scope_empty))))
        matches
        value

    fun merge_sigvals (Sigval {valspecs, dtyspecs, exnspecs, modspecs})
                      (Sigval { valspecs = valspecs'
                              , dtyspecs = dtyspecs'
                              , exnspecs = exnspecs'
                              , modspecs = modspecs'
                              }) =
      Sigval
        { valspecs = SymSet.union valspecs valspecs'
        , dtyspecs = SymDict.union dtyspecs dtyspecs' (fn (_, _, snd) => snd)
        , exnspecs = SymSet.union exnspecs exnspecs'
        , modspecs = SymDict.union modspecs modspecs' (fn (_, _, snd) => snd)
        }

    val empty_sigval =
      Sigval
        { valspecs = SymSet.empty
        , dtyspecs = SymDict.empty
        , exnspecs = SymSet.empty
        , modspecs = SymDict.empty
        }

    fun evaluate_spec ctx spec (sigval as Sigval {valspecs, dtyspecs, exnspecs, modspecs}) =
      case spec of
        SPval valbinds =>
          { valspecs =
              List.foldl
                (fn ({id, ...}, valspecs) =>
                  SymSet.insert valspecs id
                )
                valspecs
                valbinds
          , dtyspecs = dtyspecs
          , exnspecs = exnspecs
          , modspecs = modspecs
          }
          |> Sigval
      (* TODO: type stuff *)
      | SPtype typdescs => sigval
      | SPeqtype typdescs => sigval
      | SPdatdec datbinds =>
          { valspecs = valspecs
          , dtyspecs =
              List.foldl
                (fn ({tyvars, tycon, condescs}, dtyspecs) =>
                  SymDict.insert
                    dtyspecs
                    tycon
                    { arity = List.length tyvars
                    , cons = condescs
                    }
                )
                dtyspecs
                datbinds
          , exnspecs = exnspecs
          , modspecs = modspecs
          }
          |> Sigval
      | SPdatrepl {left_tycon, right_tycon} =>
          { valspecs = valspecs
          , dtyspecs =
              SymDict.insert
                dtyspecs
                left_tycon
                (Context.get_datatype ctx right_tycon)
          , exnspecs = exnspecs
          , modspecs = modspecs
          }
          |> Sigval
      | SPexception exbinds =>
          { valspecs = valspecs
          , dtyspecs = dtyspecs
          , exnspecs =
              List.foldl
                (fn ({id, ...}, exnspecs) =>
                  SymSet.insert exnspecs id
                )
                exnspecs
                exbinds
          , modspecs = modspecs
          }
          |> Sigval
      | SPmodule modules =>
          { valspecs = valspecs
          , dtyspecs = dtyspecs
          , exnspecs = exnspecs
          , modspecs =
              List.foldl
                (fn ({signat, id}, modspecs) =>
                  SymDict.insert
                    modspecs
                    id
                    (evaluate_signat ctx signat)
                )
                modspecs
                modules
          }
          |> Sigval
      | SPinclude signat =>
          merge_sigvals sigval (evaluate_signat ctx signat)
      | SPinclude_ids ids =>
          List.map (fn id => get_sig ctx id) ids
          |> List.foldl
               (fn (sigval, acc_sigval) =>
                 merge_sigvals acc_sigval sigval
               )
               sigval
      (* TODO: type stuff *)
      | SPsharing_type _ => sigval
      | SPsharing _ => sigval
      | SPseq specs =>
          List.foldl
            (fn (spec, acc) =>
              evaluate_spec ctx spec acc
            )
            sigval
            specs

    and evaluate_signat ctx signat =
      case signat of
        Sident sym =>
          get_sig ctx sym
      | Sspec spec => evaluate_spec ctx spec empty_sigval
      | Swhere {signat, wheretypee} =>
          (* TODO: type stuff *)
          evaluate_signat ctx signat

  end
