(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open PrettyPrintContext
open SMLSyntax
open Context
open Error

structure S = SMLSyntax

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Functionality related to values.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

fun snoc l =
  ( List.take (l, List.length l - 1)
  , List.nth (l, List.length l - 1)
  )

fun append_type_scheme (n, ty_fn) tyval =
  (n, fn tyvals => TVarrow (ty_fn tyvals, tyval))

val empty_sigval =
  Sigval
    { valspecs = SymDict.empty
    , tyspecs = SymDict.empty
    , dtyspecs = SymDict.empty
    , exnspecs = SymDict.empty
    , modspecs = SymDict.empty
    }

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

fun replace_tyspecs (Sigval {valspecs, tyspecs = _, dtyspecs, exnspecs, modspecs}) new =
  Sigval
    { valspecs = valspecs
    , tyspecs = new
    , dtyspecs = dtyspecs
    , exnspecs = exnspecs
    , modspecs = modspecs
    }

fun set_modspecs (Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs = _}) new =
  Sigval
    { valspecs = valspecs
    , tyspecs = tyspecs
    , dtyspecs = dtyspecs
    , exnspecs = exnspecs
    , modspecs = new
    }

(* Search through a sigval to map a module by a particular longid a.b.c .
 *)
fun map_sigval_module (sigval as Sigval {valspecs, tyspecs, dtyspecs, exnspecs, modspecs}) longid f =
  case longid of
    [] => f sigval
  | outer::rest =>
      let
        val (acc, new_sigval) =
          map_sigval_module (SymDict.lookup modspecs outer) rest f
      in
        (acc, set_modspecs sigval (SymDict.insert modspecs outer new_sigval))
      end

(* Gross, but you use what you got.
 *)
fun get_sigval_module sigval longid =
  let
    exception Return of sigval
  in
    #2 (map_sigval_module sigval longid (fn sigval => raise Return sigval))
    handle Return sigval => sigval
  end

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature VALUE =
  sig
    (* Value stuff. *)
    type t = value

    type scope = scope
    type sigval = sigval

    val value_to_exp : value -> exp
    val exp_to_value : context -> exp -> value option
    val exp_is_value : context -> exp -> bool

    val value_eq : value * value -> bool

    (* TODO: these don't _really_ fit here, but don't have a better
       place to put it currently
     *)

    val evaluate_signat : context -> signat -> sigval

    val add_sigbindings :
         Context.t
      -> (SMLSyntax.symbol * SMLSyntax.sigval) list
      -> Context.t

    val add_funbind : Context.t -> SMLSyntax.funbind -> Context.t
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Value : VALUE =
  struct
    open SMLSyntax

    type t = value

    (* VALUE STUFF *)

    (* This is used purely so we can have an exp for the surrounding context of
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
      | Vexn {name, exnid = _, arg} =>
          (case arg of
            NONE => Eident {opp = false, id = name}
          | SOME arg => Eapp { left = Eident {opp = false, id = name}
                             , right = value_to_exp arg
                             }
          )
      | Vfn {matches, env, ...} => Efn (matches, SOME env)
      (* For basis values *)
      | Vbasis {name, function, is_infix = _} => Eident {opp = false, id = [name]}

    fun exp_is_value ctx exp =
      case exp of
        Enumber _
      | Estring _
      | Echar _
      | Eselect _
      | Eunit
      | Eident _
      | Efn _ => true
      | Erecord fields =>
          List.foldr
            (fn ({exp, lab = _}, acc) =>
              exp_is_value ctx exp andalso acc
            )
            true
            fields
      | Elist exps =>
          List.foldr
            (fn (exp, acc) =>
              exp_is_value ctx exp andalso acc
            )
            true
            exps
      | Etuple exps =>
          List.foldr
            (fn (exp, acc) =>
              exp_is_value ctx exp andalso acc
            )
            true
            exps
      | Eparens exp => exp_is_value ctx exp
      | Eapp {left = Eident {id, ...}, right} =>
          Context.is_con ctx id (* THINK: or is exn? *)
          andalso exp_is_value ctx right
      | Einfix {left, id, right} =>
           Context.is_con ctx [id] (* THINK: or is exn? *)
           andalso exp_is_value ctx left
           andalso exp_is_value ctx right
      | Etyped {exp, ...} => exp_is_value ctx exp
      | ( Eseq _ (* THINK: eseq singleton? *)
        | Eapp _
        | Elet _
        | Eandalso _
        | Eorelse _
        | Ehandle _
        | Eraise _
        | Eif _
        | Ewhile _
        | Ecase _ ) => false
      | Ehole => raise Fail "shouldn't happen?"



    fun exp_to_value ctx exp =
      case exp of
        Enumber num => SOME (Vnumber num)
      | Estring s => SOME (Vstring s)
      | Echar c => SOME (Vchar c)
      | Eselect sym => SOME (Vselect sym)
      | Eunit => SOME Vunit
      | Eident {id, ...} =>
          (case Context.get_ident_opt ctx id of
            (SOME (E _) | SOME (C _)) =>
              SOME (Vconstr {id = id, arg = NONE})
          | SOME (V v) => SOME v
          | NONE => raise Fail "exp to value on unbound ident"
          )
      | Efn (_, SOME _) => raise Fail "should probably not happen"
      | Efn (matches, NONE) =>
          (* The only reason to do an exp to value on a verbatim Efn is if it
           * was a bona fide literal lambda expression, ergo non-recursive.
           * So it should be OK to set rec_env and break to NONE.
           *)
          SOME (
            Vfn { matches = matches
                , env = ctx
                , rec_env = NONE
                , break = ref NONE
                , abstys = AbsIdDict.empty
                }
          )
      | Erecord fields =>
          List.map
            (fn {lab, exp} =>
              Option.map
                (fn value => {lab = lab, value = value})
                (exp_to_value ctx exp)
            )
            fields
          |> opt_all
          |> Option.map (fn fields => Vrecord fields)
      | Elist exps =>
          List.map
            (exp_to_value ctx)
            exps
          |> opt_all
          |> Option.map (fn fields => Vlist fields)
      | Etuple exps =>
          List.map
            (exp_to_value ctx)
            exps
          |> opt_all
          |> Option.map (fn fields => Vtuple fields)

      | Eparens exp => exp_to_value ctx exp
      | Eapp {left = Eident {id, ...}, right} =>
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
        (Vnumber num, Vnumber num') => SH.number_eq (num, num')
      | (Vstring s, Vstring s') => Symbol.eq (s, s')
      | (Vchar c, Vchar c') => c = c'
      | (Vrecord fields, Vrecord fields') =>
          let
            fun field_eq ({lab, value}, {lab = lab', value = value'}) =
              Symbol.eq (lab, lab') andalso value_eq (value, value')
          in
            Common.subset fields fields' field_eq
            andalso Common.subset fields' fields field_eq
          end
      | (Vunit, Vunit) => true
      | (Vconstr {id, arg = NONE}, Vconstr {id = id', arg = NONE}) =>
          SH.longid_eq (id, id')
      | (Vconstr {id, arg = SOME value}, Vconstr {id = id', arg = SOME value'}) =>
          SH.longid_eq (id, id') andalso value_eq (value, value')
      | (Vselect sym, Vselect sym') => Symbol.eq (sym, sym')
      | (Vtuple values, Vtuple values') =>
          ListPair.allEq value_eq (values, values')
      | (Vlist values, Vlist values') =>
          ListPair.allEq value_eq (values, values')
      | (Vinfix {left, id, right}, Vinfix {left = left', id = id', right = right'}) =>
          value_eq (left, left') andalso Symbol.eq (id, id') andalso value_eq (right, right')
      | (Vfn _, Vfn _) => false
      (* For basis values *)
      | (Vbasis {name, function = _, is_infix = _}, Vbasis {name = name', ...}) =>
          Symbol.eq (name, name')
      | _ => false

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
          | (_, SOME {tyid, ...}) => SOME (TVapp (tyvals, tyid))
          | (NONE, NONE) => NONE

        (* This function is supposed to allow us to get the type scheme which
         * takes into account the abstract types and datatypes defined
         * previously in the signature.
         *)
        fun get_type_scheme tyvars ty =
          Context.mk_type_scheme
            spec_datatype_fn
            tyvars
            ty
            ctx

        (* This code is kinda complicated because structure sharing allows
         * enclosed types to be concrete, but only if they do not overlap in
         * name with any types from the other structures.
         *
         * Also, we precompute whether this type is equality before changing it.
         *)
        fun share_type allow_concrete equality_flag tyspecs tycon absid =
          case SymDict.find tyspecs tycon of
            SOME {equality = _, status = Abstract (n, absid')} =>
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
          | SOME {equality = _, status = Concrete res} =>
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
                    map_sigval_module sigval xs (fn Sigval sigval =>
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
                  map_sigval_module (Sigval sigval) xs (fn Sigval sigval =>
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

        (* These are all the known absids at this time.
         * Thus, any function values bound at this stage of the signature know
         * about these abstract types, and have license to manipulate them as
         * such.
         * At ascription time, these function values will be marked with this
         * information.
         *)
        val absids =
          SymDict.foldl
            (fn (_, {equality = _, status}, acc) =>
              case status of
                Abstract (_, id) => id :: acc
              | _ => acc
            )
            []
            tyspecs
      in
        case spec of
          SPval valbinds =>
            { valspecs =
                List.foldl
                  (fn ({id, ty}, valspecs) =>
                    (* TODO: this is broken for polymorphic types
                     *)
                    SymDict.insert valspecs id (get_type_scheme [] ty, absids)
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
                | (SOME ty, _) => SOME ty
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
                        NONE => (0, fn _ => Basis.exn_ty)
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
              val sigval =
                evaluate_spec ctx spec sigval

            in
              share_tycons false tycons sigval
            end
        | SPsharing {spec, tycons} =>
            let
              val sigval =
                evaluate_spec ctx spec sigval

              val mod_tys =
                (* Accumulating a dictionary mapping type names to the
                 * structures they can be found in.
                 *)
                List.foldl
                  (fn (longstrid, mod_tys) =>
                    let
                      val Sigval sigval = get_sigval_module sigval longstrid
                    in
                      SymDict.foldl (fn (tyname, _, mod_tys) =>
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
      | Swhere {signat, wheretypee = _} =>
          (* TODO: type stuff *)
          evaluate_signat ctx signat

    fun add_sigbindings {scope, outer_scopes, dtydict, sigdict, functordict,
    tyvars, hole_print_fn, settings, abstys} sigbindings =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict =
            ( List.foldl
                (fn ((id, sigval), sigdict) =>
                  SymDict.insert sigdict id sigval
                )
                sigdict
                sigbindings
            )
      , functordict = functordict
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }

    fun add_funbind (ctx as {scope, outer_scopes, dtydict, sigdict, functordict, tyvars
    , hole_print_fn, settings, abstys})
                    {id, funarg, seal, body} =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = sigdict
      , functordict =
          SymDict.insert
            functordict
            id
            ( Functorval
              { arg_seal =
                  case funarg of
                    Normal {id, signat} =>
                      { id = SOME id, sigval = evaluate_signat ctx signat }
                  | Sugar spec =>
                      { id = NONE, sigval = evaluate_signat ctx (Sspec spec) }
              , seal =
                  Option.map
                    (fn {signat, opacity} =>
                      { opacity = opacity, sigval = evaluate_signat ctx signat }
                    )
                    seal
              , body = body
              }
            )
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }

  end
