
(* The point of this is to elaborate the austere AST to the simpler AST.
 *
 * All type information is not important. Assume a type-checking AST.
 *)
structure Elaborate =
  struct
    open AstType
    open SMLSyntax

    infix <<| |>
    fun (x, y) <<| f = (f x, y)
    fun x |> f = f x
    fun (x, y) >>| f = (x, f y)

    fun tok_to_sym tok =
      Symbol.fromValue (Token.toString tok)

    fun ml_tok_to_longid tok =
      case Token.getClass tok of
        Token.LongIdentifier =>
          Token.toString tok
          |> String.tokens (fn c => c = #".")
          |> List.map Symbol.fromValue
      | _ => raise Fail "not a long identifier"

    fun elab_seq f s =
        Seq.iterate
          (fn (acc, elem) => f elem :: acc)
          elems
        <<| List.rev

    fun elab_seq_ctx f s ctx =
      Seq.iterate
          (fn ((acc, ctx), elem) =>
            f (elem, ctx)
            <<| (fn x => x::acc)
          )
          (elems, ctx)
        <<| List.rev


    fun lift f = (fn (x, ctx) => (f x, ctx))

    fun expand ctx constr x = (constr x, ctx)

    fun opt_to_bool opt =
      case opt of
        NONE => false
      | SOME _ => true

    fun elab_sseq f s =
      case s of
        SyntaxSeq.Empty => []
      | SyntaxSeq.One x => [f x]
      | SyntaxSeq.Many {elems, ...} =>
          elab_seq f elems

    fun elab_constant tok =
        case Token.getClass tok of
          Token.IntegerConstant =>
            Token.toString tok
            |> Int.fromString
            |> Option.valOf
            |> Int
            |> Enumber
        | Token.WordConstant =>
            Enumber (Word (Token.toString tok))
        | Token.RealConstant =>
            Token.toString tok
            |> Real.fromString
            |> Option.valOf
            |> Real
            |> Enumber
        | Token.CharConstant =>
            Token.toString tok
            |> Char.fromString
            |> Option.valOf
            |> Echar
        | Token.StringConstant =>
            Estring (Token.toString tok)
        | ( Token.Whitespace
          | Token.Comment
          | Token.MLtonReserved
          | Token.Reserved
          | Token.Identifier =>
          | Token.LongIdentifier =>
          ) => raise Fail "not a constant"

    fun elab_constant_pat tok =
        case Token.getClass tok of
          Token.IntegerConstant =>
            Token.toString tok
            |> Int.fromString
            |> Option.valOf
            |> Pnumber
        | Token.WordConstant =>
            Pword (tok_to_sym tok)
        | Token.RealConstant =>
            raise Fail "can't have real pats"
        | Token.CharConstant =>
            Token.toString tok
            |> Char.fromString
            |> Option.valOf
            |> Pchar
        | Token.StringConstant =>
            Pstring (Token.toString tok)
        | ( Token.Whitespace
          | Token.Comment
          | Token.MLtonReserved
          | Token.Reserved
          | Token.Identifier =>
          | Token.LongIdentifier =>
          ) => raise Fail "not a constant"

    fun elab_ast (ast, ctx) =
      case ast of
        Ast decs =>
          elab_seq_ctx
            (fn (topdec, ctx) =>
              elab_topdec (topdec, ctx)
              <<| (fn topdec => topdec :: decs_acc)
            )
            decs
            ctx

    and elab_topdec (topdec, ctx) =
      case topdec of
        SigDec sigdec => elab_sigdec (sigdec, ctx)
      | StrDec strdec => elab_strdec (strdec, ctx)
      | FunDec fundec => elab_fundec (fundec, ctx)

    and elab_sigdec (sigdec, ctx) =
      case sigdec of
        Signature {elems, ...} =>
          elab_seq_ctx
            (fn ({ident, sigexp, ...}, ctx) =>
              elab_sigexp (sigexp, ctx)
              <<| (fn signat => {id=tok_to_sym ident, signat=signat})
              <<| (fn sigbind => sigbind::acc)
            )
            elems
            ctx
          |> (fn (sigbinds, ctx) => (sigbinds, Context.add_signat ctx sigbinds)

    and elab_sigexp (sigexp, ctx) =
      case sigexp of
        Ident tok => (Sident (tok_to_sym tok), ctx)
      | Spec {spec, ...} =>
          elab_spec (spec, ctx)
          <<| Sspec
      | WhereType {sigexp, elems} =>
          let
            val (signat, ctx) = elab_sigexp (sigexp, ctx)
            val wheretypee =
              elab_seq
                (fn {tyvars, tycon, ty, ...} =>
                  { tyvars = elab_tyvars tyvars
                  , id = ml_tok_to_longid tycon
                  , ty = elab_ty ty
                  }
                )
                elems
          in
            (Swhere {signat=signat, wheretypee=wheretypee}, ctx)
          end

      and elab_spec (spec, ctx) =
        case spec of
          Sig.EmptySpec => SPseq []
        | Sig.Val {elems, ...} =>
            elab_seq
              (fn (acc, {vid, ty, ...}) =>
                {id = tok_to_sym vid, ty = elab_ty ty}
              )
              elems
            |> expand SPval ctx
        | Sig.Type {elems, ...} =>
            elab_seq
              (fn {tyvars, tycon} =>
                { tyvars = elab_tyvars tyvars
                , tycon = tok_to_sym tycon
                , ty = NONE
                }
              )
              elems
            |> expand SPtype ctx
        | Sig.TypeAbbreviation {elems, ...} =>
            elab_seq
              (fn {tyvars, tycon, ty} =>
                { tyvars = elab_tyvars tyvars
                , tycon = tok_to_sym tycon
                , ty = SOME (elab_ty ty)
                }
              )
              elems
            |> expand SPtype ctx
        | Sig.Eqtype {elems, ...} =>
            elab_seq
              (fn {tyvars, tycon} =>
                { tyvars = elab_tyvars tyvars
                , tycon = tok_to_sym tycon
                , ty = NONE (* TODO? *)
                }
              )
              elems
            |> expand SPeqtype ctx
        | Sig.Datatype {elems, ...} =>
            elab_seq
              (fn {tyvars, tycon, elems, ...} =>
                { tyvars = elab_tyvars tyvars
                , tycon = tok_to_sym tycon
                , condescs =
                    elab_seq
                      (fn {vid, arg} =>
                        { id = tok_to_sym vid, ty =
                          case arg of
                            NONE => NONE
                          | SOME {ty, ...} => SOME ty
                        }
                      )
                      elems
                }
              )
              ctx
            |> expand SPdatdec ctx
        | Sig.ReplicateDatatype {left_id, right_id, ...} =>
            ( SPdatrepl { left_tycon = tok_to_sym left_id,
                        , right_tycon = ml_tok_to_longid right_id
                        }
            , ctx
            )
        | Sig.Exception {elems, ...} =>
            elab_seq
              (fn {vid, arg} =>
                { id = tok_to_sym vid
                , ty = Option.map (fn {off, ty} => ty) arg
                }
              )
              elems
            |> expand SPexception ctx
        | Sig.Structure {elems, ...} =>
            elab_seq_ctx
              (fn ({id, sigexp, ...}, ctx) =>
                elab_sigexp sigexp ctx
                <<| (fn signat => {id = tok_to_sym id, signat = signat})
              )
              elems
              ctx
            <<| SPmodule
        | Sig.Include {sigexp, ...} =>
            elab_sigexp (sigexp, ctx)
            <<| SPinclude
        | Sig.IncludeIDs {sigids, ...} =>
            elab_seq
              tok_to_sym
              sigids
            |> expand SPinclude_ids ctx
        | Sig.SharingType {spec, elems, ...} =>
            let
              val (spec, ctx) = elab_spec (spec, ctx)
            in
              elab_seq
                ml_tok_to_longid
                elems
              <<| (fn tycons => {spec = spec, tycons = tycons})
              |> expand SPsharing ctx
            end
        | Sig.Multiple {elems, ...} =>
            elab_seq_ctx
              elab_spec
              elems
              ctx

      and elab_colon colon =
        case Token.toString colon of
          ":" => Transparent
        | ":>" => Opaque
        | _ => raise Fail "invalid colon for seal"

      and elab_strexp (strexp, ctx) =
        case strexp of
          Str.Ident tok => (Mident (ml_tok_to_longid tok), ctx)
        | Str.Struct {strdec, ...} =>
            elab_strdec (strdec, ctx)
            <<| Mstruct
        | Str.Constraint {strexp, sigexp, colon} =>
            let
              val (module, ctx) = elab_strexp (strexp, ctx)
              val opacity = elab_colon colon
              val (signat, ctx) = elab_sigexp (sigexp, ctx)
            in
              ( Mseal {module=module, opacity=opacity, signat=signat}
              , ctx
              )
            end
        | Str.FunAppExp {funid, strexp, ...} =>
            elab_strexp (strexp, ctx)
            <<| (fn module => Mapp {functorr = tok_to_sym funid, arg = Normal_app module})
        | Str.FunAppDec {funid, strdec, ...} =>
            elab_strdec (strdec, ctx)
            <<| (fn strdec => Mapp {functorr = tok_to_sym funid, arg = Sugar_app strdec})
        | Str.LetInEnd {strdec, strexp, ...} =>
            let
              val (strdec, ctx) = elab_strdec (strdec, ctx)
              val (module, ctx) = elab_strexp (strexp, ctx)
            in
              ( Mlet {dec = strdec, module = module}
              , ctx
              )
            end

      and elab_strdec (strdec, ctx) =
        case strdec of
          DecEmpty => (DMseq [], ctx)
        | DecCore dec =>
            elab_dec (dec, ctx)
            <<| DMdec
        | DecStructure {elems, ...} =>
            elab_seq_ctx
              (fn ({strid, constraint, strexp, ...}, ctx) =>
                let
                  val (module, ctx) = elab_strexp (strexp, ctx)
                  val (seal, ctx) =
                    case constraint of
                      NONE => (NONE, ctx)
                    | SOME {colon, sigexp} =>
                        elab_sigexp (sigexp, ctx)
                        <<| (fn signat =>
                          { opacity = elab_colon colon
                          , signat = signat
                          } )
                in
                  ( { id = tok_to_sym strid
                    , seal = seal
                    , module = module
                    }
                  , ctx
                  )
                end
              )
              elems
              ctx
            <<| DMstruct
        | DecMultiple {elems, ...} =>
            elab_seq_ctx
              elab_strdec
              elems
              ctx
            <<| DMseq
        | DecLocalInEnd {strdec1, strdec2, ...} =>
            let
              val (strdec1, ctx) = elab_strdec (strdec1, ctx)
              val (strdec2, ctx) = elab_strdec (strdec2, ctx)
            in
              ( DMlocal {left_dec = strdec1, right_dec = strdec2}
              , ctx
              )
            end
        | MLtonOverload _ => raise Fail "mlton not supported rn"

      and elab_funarg (funarg, ctx) =
        case funarg of
          ArgIdent {strid, sigexp, ...} =>
            ( elab_sigexp (sigexp, ctx)
              <<| (fn signat => Normal {id = tok_to_sym strid, signat = signat})
            , ctx
            )
        | ArgSpec spec =>
            elab_spec (spec, ctx)
            <<| sugar

      and elab_fundec (fundec, ctx) =
        case fundec of
          DecFunctor {elems, ...} =>
            elab_seq
              (fn {funid, funarg, constraint, strexp, ...} =>
                let
                  val (funarg, ctx) = elab_funarg funarg
                  val (constraint, ctx) =
                    case constraint of
                      NONE => (NONE, ctx)
                    | SOME {colon, sigexp} =>
                        elab_sigexp (sigexp, ctx)
                        <<| (fn signat => { signat = signat, opacity = elab_colon colon})
                  val (strexp, ctx) = elab_strexp (strexp, ctx)
                in
                  { id = tok_to_sym funid
                  , funarg = funarg
                  , seal = constraint
                  , body = strexp
                  }
                end
              )
              elems

      and elab_datbind datbind =
        elab_seq
          (fn {elems, ...} =>
            elab_seq
              (fn {tyvars, tycon, elems, ...} =>
                { tyvars = elab_tyvars tycons
                , tycon = tok_to_sym tycon
                , conbinds =
                    elab_seq
                      (fn { opp, id, arg } =>
                        { opp = opt_to_bool opp
                        , id = tok_to_sym id
                        , ty = Option.map #ty arg
                        }
                      )
                    elems
                }
              )
              elems
          )
          datbind


      and elab_dec (dec, ctx) =
        case dec of
          Exp.DecEmpty => Dseq []
        | Exp.DecVal {tyvars, elems, ...} =>
            elab_seq_ctx
              (fn ({recc, pat, exp, ...}, ctx) =>
                let
                  val (pat, ctx) = elab_pat (pat, ctx)
                  val (exp, ctx) = elab_exp (exp, ctx)
                in
                  ( { recc = opt_to_bool recc
                    , pat = pat
                    , exp = exp
                    }
                  , ctx
                  )
                end
              )
              elems
              ctx
            <<| (fn valbinds =>
                  Dval {tyvars = elab_tyvars tyvars, valbinds = valbinds}
                )
        | Exp.DecFun {tyvars, fvalbind, ...} =>
            elab_fvalbind (fvalbind, ctx)
            <<| (fn fvalbind => {tyvars = elab_tyvars tyvars, fvalbinds = fvalbind})
        | Exp.DecType {typbind, ...} =>
            elab_typbind typbind
            |> expand Dtype ctx
        | Exp.DecDatatype {datbind, withtypee, ...} =>
            let
              val datbinds = elab_datbind datbind
              val withtypee =
                Option.map (fn {typbind, ...} => elab_typbind typbind) withtypee
            in
              ( Ddatdec {datbinds = datbinds, withtypee = withtypee}
              , ctx
              )
            end
        | Exp.DecReplicateDatatype {left_id, right_id, ...} =>
            ( Ddatrepl { left_tycon = tok_to_sym left_id
                       , right_tycon = ml_tok_to_longid right_Id
                       }
            , ctx
            )
        | Exp.DecAbstype {datbind, withtypee, dec, ...} =>
            let
              val datbinds = elab_datbind datbind
              val withtypee =
                Option.map (fn {typbind, ...} => elab_typbind typbind) withtypee
              val (dec, ctx) = elab_dec (dec, ctx)
            in
              ( Dabstype { datbinds = datbinds
                         , withtypee = withtypee
                         , withh = dec
                         }
              , ctx
              )
            end
        | Exp.DecException {elems, ...} =>
            elab_seq
              elab_exbind
              elems
            |> expand Dexception ctx
        | Exp.DecLocal {left_dec, right_dec, ...} =>
            let
              val (left_dec, ctx) = elab_dec (left_dec, ctx)
              val (right_dec, ctx) = elab_dec (right_dec, ctx)
            in
              ( Dlocal {left_dec = left_dec, right_dec = right_dec}
              , ctx
              )
            end
        | Exp.DecOpen {elems, ...} =>
            elab_seq
              ml_tok_to_longid
              elems
            |> expand Dopen ctx
        | Exp.DecMultiple {elems, ...} =>
            elab_seq_ctx
              elab_dec
              elems
              ctx
            |> expand Dseq ctx
        | Exp.DecInfix {precedence, elems, ...} =>
            let
              val precedence =
                Option.map (Option.valOf o Int.fromString o Token.toString) precedence
              val ids = elab_seq tok_to_sym elems
            in
              ( Dinfix {precedence = precedence, ids = ids}
              , ctx
              )
            end
        | Exp.DecInfixr {precedence, elems, ...} =>
            let
              val precedence =
                Option.map (Option.valOf o Int.fromString o Token.toString) precedence
              val ids = elab_seq tok_to_sym elems
            in
              ( Dinfixr {precedence = precedence, ids = ids}
              , ctx
              )
            end
        | Exp.DecNonfix {elems, ...} =>
            ( Dnonfix (elab_seq tok_to_sym elems)
            , ctx
            )

        and elab_exbind exbind =
          case exbind of
            Exp.ExnNew {opp, id, args} =>
              Xnew { opp =  opt_to_bool opp
                   , id = tok_to_sym id
                   , ty = Option.map (elab_ty o #ty) arg
                   }
          | Exp.ExnReplicate {opp, left_id, right_id, ...} =>
              Xrepl { opp = opt_to_bool opp
                    , left_id = tok_to_sym left_id
                    , right_id = ml_tok_to_longid right_id
                    }

        and elab_exp (exp, ctx) =
          let
            val elab_exp = fn exp => elab_exp (exp, ctx)
          in
            case exp of
              Exp.Const tok => elab_constant tok
            | Exp.Ident {opp, id} =>
                (* TODO: check if it is a constructor *)
            | Exp.Record {elems, ...} =>
                elab_seq
                  (fn {lab, exp, ...} =>
                    { lab = tok_to_sym lab
                    , exp = elab_exp exp
                    }
                  )
                  elems
            | Exp.Select {label, ...} =>
                Eselect (tok_to_sym lablel)
            | Exp.Unit _ => Eunit
            | Exp.Tuple {elems, ...} =>
                Etuple (elab_seq elab_exp elems)
            | Exp.List {elems, ...} =>
                Elist (elab_seq elab_exp elems)
            | Exp.Sequence {elems, ...} =>
                Eseq (elab_seq elab_exp elems)
            | Exp.LetInEnd {dec, exps, ...} =>
                Elet { dec = #1 (elab_dec (dec, ctx))
                     , exps = elab_seq elab_exp exps
                     }
            | Exp.Parens {exp, ...} =>
                Eparens (elab_exp exp)
            | Exp.App {left, right} =>
                (* if left is constr can do smth *)
                Eapp { left = elab_exp left
                     , right = elab_exp right
                     }
            | Exp.Infix {left, right} =>
                Einfix { left = elab_exp left
                       , right = elab_exp right
                       }
            | Exp.Typed {exp, ty, ...} =>
                Etyped { exp = elab_exp exp
                       , ty = elab_ty ty
                       }
            | Exp.Andalso {left, right, ...} =>
                Eandalso { left = elab_exp left
                         , right = elab_exp right
                         }
            | Exp.Orelse {left, right, ...} =>
                Eorelse { left = elab_exp left
                        , right = elab_exp right
                        }
            | Exp.Handle {exp, elems, ...} =>
                Ehandle { exp = elab_exp exp
                        , matches =
                            elab_seq
                              (fn {pat, exp, ...} =>
                                { pat = elab_pat (pat, ctx)
                                , exp = elab_exp exp
                                }
                              )
                              elems
                        }
            | Exp.Raise {exp, ...} =>
                Eraise (elab_exp exp)
            | Exp.IfThenElse {exp1, exp2, exp3, ...} =>
                Eif { exp1 = elab_exp exp1
                    , exp2 = elab_exp exp2
                    , exp3 = elab_exp exp3
                    }
            | Exp.While {exp1, exp2, ...} =>
                Ewhile { exp1 = elab_exp exp1
                       , exp2 = elab_exp exp2
                       }
            | Exp.Case {exp, elems, ...} =>
                Ecase { exp = elab_exp exp
                      , matches =
                          elab_seq
                            (fn {pat, exp, ...} =>
                              { pat = elab_pat (pat, ctx)
                              , exp = elab_exp exp
                              }
                            )
                            elems
                      }
            | Exp.Fn {elems, ...} =>
                Efn (elab_seq (fn {pat, exp, ...} =>
                      { pat = elab_pat (pat, ctx)
                      , exp = elab_exp exp
                      }
                    ) elems)
            | Exp.MLtonSpecific _ => raise Fail "mlton not supported rn"
          end

        and elab_fname_args fname_args =
          case fname_args of
            Exp.PrefixedFun {opp, id, args} =>
              Fprefix { opp = opt_to_bool opp
                      , id = tok_to_sym id
                      , args = elab_seq elab_pat args
                      }
          | Exp.InfixedFun {larg, id, rarg} =>
              Finfix { left = elab_pat larg
                     , id = tok_to_sym id
                     , right = elab_pat rarg
                     }
          | Exp.CurriedInfixedFun {larg, id, rarg, args, ...} =>
              Fcurried_infix { left = elab_pat larg
                             , id = tok_to_sym id
                             , rarg = elab_pat rarg
                             , args = elab_seq elab_pat args
                             }

        and elab_fvalbind {elems, delims} =
          elab_seq
            (fn {elems, ...} =>
              elab_seq
                (fn {fname_args, ty, exp, ...} =>
                  { fname_args = elab_fname_args fname_args
                  , ty = Option.map elab_ty ty
                  , exp = elab_exp exp
                  }
                )
                elems
            )
            elems

        and elab_typbind {elems, delims} =
          elab_seq
            (fn {tyvars, tycon, ty, ...} =>
              { tyvars = elab_tyvars tyvars
              , tycon = tok_to_sym tycon
              , ty = elab_ty ty
              }
            )
            elems

        and elab_tyvars tyvars =
          elab_sseq tok_to_sym tyvars

        and elab_pat (pat, ctx) =
          case pat of
            Pat.Wild _ => Pwild
          | Pat.Const tok =>
              elab_constant_pat tok
          | Pat.Unit _ => Punit
          | Pat.Ident {opp, id} =>
              (* TODO: constr or not *)
          | Pat.List {elems, ...} =>
              Plist (elab_seq_ctx elab_pat elems ctx)
          | Pat.Tuple {elems, ...} =>
              Ptuple (elab_seq_ctx elab_pat elems ctx)
          | Pat.Record {elems, ...} =>
              Precord (elab_seq_ctx elab_patrow elems ctx)
          | Pat.Parens {pat, ...} =>
              Pparens (elab_pat (pat, ctx))
          | Pat.Con {opp, id, atpat} =>
              Papp { opp = opt_to_bool opp
                   , id = ml_tok_to_longid id
                   , atpat = elab_pat (pat, ctx)
                   }
          | Pat.Infix {left, id, right} =>
              Pinfix { left = elab_pat (left, ctx)
                     , id = tok_to_sym id
                     , right = elab_pat (right, ctx)
                     }
          | Pat.Typed {pat, ty, ...} =>
              Ptyped { pat = elab_pat (pat, ctx)
                     , ty = elab_ty ty
                     }
          | Pat.Layered {opp, id, ty, pat, ...} =>
              Playered { opp = opt_to_bool opp
                       , id = tok_to_sym id
                       , ty = Option.map (elab_ty o #ty) ty
                       , pat = elab_pat (pat, ctx)
                       }

        and elab_patrow (patrow, ctx) =
          case patrow of
            Pat.DotDotDot _ => PRellipsis
          | Pat.LabEqPat {lab, pat, ...} =>
              PRlab { lab = tok_to_sym lab
                    , pat = elab_pat (pat, ctx)
                    }
          | Pat.LabAsPat { id, ty, aspat } =>
              PRas { id = tok_to_sym id
                   , ty = Option.map (elab_ty o #ty) ty
                   , aspat = Option.map (fn {pat, ...} => elab_pat (pat, ctx)) aspat
                   }

        and elab_ty ty =
          case ty of
            Ty.Var tok => Ttyvar (tok_to_sym tok)
          | Ty.Record {elems, ...} =>
              elab_seq
                (fn {lab, ty, ...} =>
                  { lab = tok_to_sym lab
                  , ty = elab_ty ty
                  }
                )
                elems
              |> Trecord
          | Ty.Tuple {elems, ...} =>
              elab_seq
                elab_ty
                elems
              |> Tprod
          | Ty.Con {args, id} =>
              Tapp ( elab_sseq elab_ty args
                   , ml_tok_to_longid id
                   )
          | Ty.Arrow {from, to, ...} =>
              Tarrow ( elab_ty from
                     , elab_ty to
                     )
          | Ty.Parens {ty, ...} =>
              Tparens (elab_ty ty)
    and

  end
