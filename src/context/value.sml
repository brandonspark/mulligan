
structure Value :
  sig

    (* Value stuff. *)

    type t = SMLSyntax.context
    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type sigval = SMLSyntax.sigval

    val value_to_exp : value -> SMLSyntax.exp
    val exp_to_value : t -> SMLSyntax.exp -> value option

    val value_eq : value * value -> bool

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

    fun snoc l =
      ( List.take (l, List.length l - 1)
      , List.nth (l, List.length l - 1)
      )
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

    fun value_eq values =
      case values of
        (Vnumber num, Vnumber num') => number_eq (num, num')
      | (Vstring s, Vstring s') => Symbol.eq (s, s')
      | (Vchar c, Vchar c') => c = c'
      | (Vrecord fields, Vrecord fields') =>
          let
            fun field_subset fields1 fields2 =
              List.foldl
                (fn ({lab, value}, acc) =>
                  case List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields2 of
                    NONE => false
                  | SOME {value = value', ...} =>
                      value_eq (value, value') andalso acc
                )
                true
                fields1
          in
            field_subset fields fields'
            andalso field_subset fields' fields
          end
      | (Vunit, Vunit) => true
      | (Vconstr {id, arg = NONE}, Vconstr {id = id', arg = NONE}) =>
          longid_eq (id, id')
      | (Vconstr {id, arg = SOME value}, Vconstr {id = id', arg = SOME value'}) =>
          longid_eq (id, id') andalso value_eq (value, value')
      | (Vselect sym, Vselect sym') => Symbol.eq (sym, sym')
      | (Vtuple values, Vtuple values') =>
          ListPair.allEq value_eq (values, values')
      | (Vlist values, Vlist values') =>
          ListPair.allEq value_eq (values, values')
      | (Vinfix {left, id, right}, Vinfix {left = left', id = id', right = right'}) =>
          value_eq (left, left') andalso Symbol.eq (id, id') andalso value_eq (right, right')
      | (Vfn _, Vfn _) => false
      (* For basis values *)
      | (Vbasis {name, function}, Vbasis {name = name', ...}) =>
          Symbol.eq (name, name')
      | _ => false


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

    fun apply_fn (matches, env, VE) value =
      match_against
        (Context.merge_scope env (Context.ctx_rec (Option.getOpt (VE, scope_empty))))
        matches
        value

    fun merge_sigvals (Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs})
                      (Sigval { valspecs = valspecs'
                              , tyspecs = tyspecs'
                              , dtyspecs = dtyspecs'
                              , exnspecs = exnspecs'
                              , modspecs = modspecs'
                              }) =
      Sigval
        { valspecs = SymDict.union valspecs valspecs' (fn (_, _, snd) => snd)
        , tyspecs = SymDict.union tyspecs tyspecs' (fn (_, _, snd) => snd)
        , dtyspecs = SymDict.union dtyspecs dtyspecs' (fn (_, _, snd) => snd)
        , exnspecs = SymDict.union exnspecs exnspecs' (fn (_, _, snd) => snd)
        , modspecs = SymDict.union modspecs modspecs' (fn (_, _, snd) => snd)
        }

    val empty_sigval =
      Sigval
        { valspecs = SymDict.empty
        , tyspecs = SymDict.empty
        , dtyspecs = SymDict.empty
        , exnspecs = SymDict.empty
        , modspecs = SymDict.empty
        }

    fun evaluate_spec
          ctx
          spec
          (sigval as Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs}) =
      let
        (* We search in the original tyspec, because all of these are
         * "simultaneous" and don't see each other.
         *)
        fun spec_datatype_fn (sym, tyvals) =
          case (SymDict.find tyspecs sym, SymDict.find dtyspecs sym) of
            (SOME _, SOME _) => prog_err "should be impossible"
          | (SOME { status = Abstract (_, id), ... }, _) => SOME (TVabs (tyvals, id))
          | (SOME { status = Concrete (_, ty_fn), ... }, _) => SOME (ty_fn tyvals)
          | (_, SOME {arity, tyid, cons}) => SOME (TVapp (tyvals, tyid))
          | (NONE, NONE) => NONE

        fun get_type_scheme tyvars ty =
          Context.mk_type_scheme
            spec_datatype_fn
            tyvars
            ty
            ctx

        fun append_type_scheme (n, ty_fn) tyval =
          (n, fn tyvals => TVarrow (ty_fn tyvals, tyval))

        fun set_modspecs (Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs}) new =
          Sigval
            { valspecs = valspecs
            , tyspecs = tyspecs
            , dtyspecs = dtyspecs
            , exnspecs = exnspecs
            , modspecs = new
            }

        fun map_module (sigval as Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs}) longid f =
          case longid of
            [] => f sigval
          | outer::rest =>
              let
                val (acc, new_sigval) =
                  map_module (SymDict.lookup modspecs outer) rest f
              in
                (acc, set_modspecs sigval (SymDict.insert modspecs outer new_sigval))
              end

        fun get_module sigval longid =
          let
            exception Return of sigval
          in
            #2 (map_module sigval longid (fn sigval => raise Return sigval))
            handle Return sigval => sigval
          end

        fun replace_tyspecs (Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs}) new =
          Sigval
            { valspecs = valspecs
            , tyspecs = new
            , dtyspecs = dtyspecs
            , exnspecs = exnspecs
            , modspecs = modspecs
            }

        (* This code is kinda complicated because structure sharing allows
         * enclosed types to be concrete, but only if they do not overlap in
         * name with any types from the other structures.
         *
         * Also, we precompute whether this type is equality before changing it.
         *)
        fun share_type allow_concrete equality_flag tyspecs tycon absid =
          case SymDict.find tyspecs tycon of
            SOME {equality, status = Abstract (n, absid')} =>
              (case absid of
                NONE => (SOME (Abstract (n, absid')), tyspecs)
              | SOME (Abstract (n', new_absid)) =>
                  if n <> n' then
                    prog_err "sharing type on inconsistent arities"
                  else
                    ( absid
                    , SymDict.insert
                        tyspecs
                        tycon
                        { equality = equality_flag
                        , status = Abstract (n, new_absid)
                        }
                    )
              | SOME (Concrete _) => prog_err "sharing type on non-abstract type"
              )
          | SOME {equality, status = Concrete res} =>
              if allow_concrete then
                (SOME (Concrete res), tyspecs)
              else
                prog_err "sharing type on concrete type"
          | NONE => prog_err "sharing type on nonexistent type"

        fun share_tycons allow_concrete tycons sigval =
          let
            val equality_flag =
              List.foldl
                (fn (tycon, acc) =>
                  let
                    val (xs, x) = snoc tycon
                  in
                    map_module sigval xs (fn Sigval sigval =>
                      (#equality (SymDict.lookup (#tyspecs sigval) x), Sigval sigval)
                    )
                    |> (fn (b, _) => b orelse acc)
                  end
                )
                false
                tycons
          in
            List.foldl
              (fn (tycon, (absid, Sigval sigval)) =>
                let
                  val (xs, x) = snoc tycon
                in
                  map_module (Sigval sigval) xs (fn Sigval sigval =>
                    let
                      val (absid, tyspecs) =
                        share_type allow_concrete equality_flag (#tyspecs sigval) x absid
                    in
                      (absid, replace_tyspecs (Sigval sigval) tyspecs)
                    end
                  )
                end
              )
              (NONE, sigval)
              tycons
            |> #2
          end
      in
        case spec of
          SPval valbinds =>
            { valspecs =
                List.foldl
                  (fn ({id, ty}, valspecs) =>
                    SymDict.insert valspecs id (get_type_scheme [] ty)
                  )
                  valspecs
                  valbinds
            , tyspecs = tyspecs
            , dtyspecs = dtyspecs
            , exnspecs = exnspecs
            , modspecs = modspecs
            }
            |> Sigval
        (* TODO: type stuff *)
        | SPtype typdescs =>
            { valspecs = valspecs
            , tyspecs =
                List.foldl
                  (fn ({tyvars, tycon, ty}, tyspecs) =>
                    case ty of
                      NONE =>
                        (* No type means that this is an abstract type, so we
                         * generate it a unique identifier and add it to the
                         * tyspec.
                         *)
                        SymDict.insert tyspecs tycon
                          { equality = false
                          , status = Abstract (List.length tyvars, AbsId.new (SOME tycon))
                          }
                    | SOME ty =>
                        (* This must be a concrete type.
                         * We should insert the type scheme where we replace all
                         * instances of previously-declared types with their
                         * instantiated counterparts.
                         *)
                        SymDict.insert tyspecs tycon
                          { equality = false
                          , status = Concrete (mk_type_scheme spec_datatype_fn tyvars ty ctx)
                          }
                  )
                  tyspecs
                  typdescs
            , dtyspecs = dtyspecs
            , exnspecs = exnspecs
            , modspecs = modspecs
            }
            |> Sigval
        | SPeqtype typdescs =>
            { valspecs = valspecs
            , tyspecs =
                List.foldl
                  (fn ({tyvars, tycon}, tyspecs) =>
                    (* No type means that this is an abstract type, so we
                     * generate it a unique identifier and add it to the
                     * tyspec.
                     *)
                    SymDict.insert tyspecs tycon
                      { equality = true
                      , status = Abstract (List.length tyvars, AbsId.new (SOME tycon))
                      }
                  )
                  tyspecs
                  typdescs
            , dtyspecs = dtyspecs
            , exnspecs = exnspecs
            , modspecs = modspecs
            }
            |> Sigval
        | SPdatdec datbinds =>
            let
              val enum_datbinds =
                List.map (fn datbind as {tycon, ...} => (datbind, TyId.new (SOME tycon))) datbinds

              (* This function both can look through the previous typdescs, as
               * well as any of the mutually recursive datatypes.
               *)
              fun datatype_fn (sym, tyvals) =
                case
                  ( spec_datatype_fn (sym, tyvals)
                  , List.find
                      (fn ({tycon, ...}, _) => Symbol.eq (sym, tycon))
                      enum_datbinds
                  )
                of
                  (NONE, NONE) => NONE
                | (SOME _, SOME _) => raise Fail "shouldn't be possible"
                | (SOME ty, _) => SOME (ty)
                | (_, SOME (_, tyid)) => SOME (TVapp (tyvals, tyid))
            in
              { valspecs = valspecs
              , tyspecs = tyspecs
              , dtyspecs =
                  List.foldl
                    (fn (({tyvars, tycon, condescs}, tyid), dtyspecs) =>
                      (* TODO: This means when doing signature matching, we have
                       * to be able to check equality-up-to-self-TYids.
                       *)
                      SymDict.insert
                        dtyspecs
                        tycon
                        { arity = List.length tyvars
                        , tyid = tyid
                        , cons =
                          List.map
                            (fn {id, ty} =>
                              { id = id
                              , tyscheme =
                                case ty of
                                  NONE =>
                                    Context.mk_type_scheme
                                      datatype_fn
                                      tyvars
                                      (Tapp (List.map Ttyvar tyvars, [tycon]))
                                      ctx
                                | SOME ty =>
                                    Context.mk_type_scheme datatype_fn tyvars
                                    (Tarrow (ty, Tident [tycon])) ctx
                              }
                            )
                            condescs
                        }
                    )
                    dtyspecs
                    enum_datbinds
              , exnspecs = exnspecs
              , modspecs = modspecs
              }
            end
            |> Sigval
        | SPdatrepl {left_tycon, right_tycon} =>
            { valspecs = valspecs
            , tyspecs = tyspecs
            , dtyspecs =
              let
                val {arity, cons} =
                  Context.get_datatype_with_longid ctx right_tycon
              in
                SymDict.insert
                  dtyspecs
                  left_tycon
                  {arity = arity, cons = cons, tyid = TyId.new (SOME left_tycon)}
              end
            , exnspecs = exnspecs
            , modspecs = modspecs
            }
            |> Sigval
        | SPexception exbinds =>
            { valspecs = valspecs
            , tyspecs = tyspecs
            , dtyspecs = dtyspecs
            , exnspecs =
                List.foldl
                  (fn ({id, ty, ...}, exnspecs) =>
                    SymDict.insert exnspecs id
                      (case ty of
                        NONE => (0, fn tyvals => Basis.exn_ty)
                      | SOME ty =>
                          append_type_scheme
                            (get_type_scheme [] ty)
                            Basis.exn_ty
                      )
                  )
                  exnspecs
                  exbinds
            , modspecs = modspecs
            }
            |> Sigval
        | SPmodule modules =>
            { valspecs = valspecs
            , tyspecs = tyspecs
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
        | SPsharing_type {spec, tycons} =>
            let
              val sigval as Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs} =
                evaluate_spec ctx spec sigval

            in
              share_tycons false tycons sigval
            end
        | SPsharing {spec, tycons} =>
            let
              val sigval as Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs} =
                evaluate_spec ctx spec sigval

              val mod_tys =
                (* Accumulating a dictionary mapping type names to the
                 * structures they can be found in.
                 *)
                List.foldl
                  (fn (longstrid, mod_tys) =>
                    let
                      val Sigval sigval = get_module sigval longstrid
                    in
                      SymDict.foldl (fn (tyname, tyscheme, mod_tys) =>
                        SymDict.insertMerge
                          mod_tys
                          tyname
                          [longstrid]
                          (fn strids => longstrid :: strids)
                      ) mod_tys (#tyspecs sigval)
                    end
                  )
                  SymDict.empty
                  tycons
            in
              SymDict.foldl
                (fn (tyname, longstrids, sigval) =>
                  share_tycons
                    true
                    (List.map (fn longstrid => longstrid @ [tyname]) longstrids)
                    sigval
                )
                sigval
                mod_tys
            end
        | SPseq specs =>
            List.foldl
              (fn (spec, acc) =>
                evaluate_spec ctx spec acc
              )
              sigval
              specs
      end

    and evaluate_signat ctx signat =
      case signat of
        Sident sym =>
          get_sig ctx sym
      | Sspec spec => evaluate_spec ctx spec empty_sigval
      | Swhere {signat, wheretypee} =>
          (* TODO: type stuff *)
          evaluate_signat ctx signat

  end
