(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

open SMLSyntax

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to go along with the types from `SMLSyntax`
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature SMLSYNTAXHELPERS =
  sig
    val map_sym : symbol -> (string -> string) -> symbol
    val longid_eq : longid * longid -> bool
    val longid_to_str : longid -> string
    val tyvar_eq : tyvar * tyvar -> bool
    val guard_tyscheme : type_scheme -> type_scheme
    val concrete_tyscheme : tyval -> type_scheme
    val number_eq : number * number -> bool

    val sym : string -> symbol

    val sym_true : symbol
    val sym_false : symbol
    val tyval_of_instantiated_synonym :
      SMLSyntax.tyval list
      -> SMLSyntax.synonym
      -> SMLSyntax.tyval
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure SMLSyntaxHelpers =
  struct
    type symbol = symbol
    type longid = symbol list

    fun map_sym sym f =
      Symbol.fromValue (f (Symbol.toValue sym))
    fun longid_eq (l1, l2) =
      ListPair.allEq Symbol.eq (l1, l2)
    fun longid_to_str longid =
      String.concatWith "." (List.map Symbol.toValue longid)

    fun tyvar_eq (t1, t2) =
      case (t1, t2) of
        (Proper s1, Proper s2) => Symbol.eq (s1, s2)
      | (Unconstrained r1, Unconstrained r2) => r1 = r2
      | _ => false

    fun guard_tyscheme (n, ty_fn) =
      ( n
      , fn tyvals =>
          if List.length tyvals <> n then
            raise Fail "Instantiated type scheme with incorrect number of tyargs"
          else
            ty_fn tyvals
      )
    fun concrete_tyscheme tyval =
      guard_tyscheme (0, fn _ => tyval)

    fun number_eq (n1, n2) =
      case (n1, n2) of
        (Int i1, Int i2) => i1 = i2
      | (Real _, Real _) => raise Fail "comparing reals for equality"
      | (Word w1, Word w2) => w1 = w2
      | _ => false

    val sym = Symbol.fromValue

    val sym_true = sym "true"
    val sym_false = sym "false"

    fun tyval_of_instantiated_synonym tyvals synonym =
      case synonym of
        Datatype tyid =>
          TVapp (tyvals, tyid)
      | Abs absid =>
          TVabs (tyvals, absid)
      | Scheme (_, ty_fn) => ty_fn tyvals

    (*
    fun make_maps f_exp f_dec =
      let
        fun map_exp exp =
          ( case exp of
              ( Enumber _
              | Estring _
              | Echar _
              | Eunit
              | Eselect _
              | Eident _
              | Ehole
              ) => exp
            | Erecord fields =>
                List.map
                  (fn {lab, exp} => {lab = lab, map_exp exp})
                  fields
                |> Erecord
            | Etuple exps =>
                List.map map_exp exps
                |> Etuple
            | Elist exps =>
                List.map map_exp exps
                |> Elist
            | Eseq exps =>
                List.map map_exp exps
                |> Elist
            | Elet {dec, exps} =>
                { dec = map_dec dec
                , exps = List.map map_exp exps
                }
                |> Elet
            | Eparens exp =>
                Eparens (map_exp exp)
            | Eapp {left, right} =>
                Eapp {left = map_exp left, right = map_exp right}
            | Einfix {left, id, right} =>
                Einfix { left = map_exp left, id = id, right = map_exp right }
            | Etyped {exp, ty} =>
                Etyped {exp = map_exp exp, ty = map_ty ty}
            | Eandalso {left, right} =
                Eandalso {left = map_exp left, right = map_exp right}
            | Eorelse {left, right} =
                Eorelse {left = map_exp left, right = map_exp right}
            | Ehandle {exp, matches} =>
                Ehandle {exp = map_exp exp, matches = map_matches matches}
            | Eraise exp =>
                Eraise (map_exp exp)
            | Eif {exp1, exp2, exp3} =>
                Eif { exp1 = map_exp exp1
                    , exp2 = map_exp exp2
                    , exp3 = map_exp exp3
                    }
            | Ewhile {exp1, exp2} =>
                Ewhile { exp1 = map_exp exp1
                       , exp2 = map_exp exp2
                       }
            | Ecase {exp, matches} =>
                Ecase {exp = map_exp exp, matches = map_matches matches}
            | Efn (matches, ctxopt) =>
                Efn (map_matches matches, ctxopt)
          ) |> f_exp

        and map_dec f_exp dec =
          ( case dec of
              Dval {tyvars, valbinds} =>
                Dval { tyvars = tyvars
                     , valbinds =
                         List.map
                           (fn {recc, pat, exp} =>
                             { recc = recc
                             , pat = map_pat pat
                             , exp = map_exp exp
                             }
                           )
                           valbinds
                    }
            | Dfun {tyvars, fvalbinds} =>
                Dfun { tyvars = tyvars
                     , fvalbinds =
                         List.map
                           (List.map
                             (fn {fname_args, ty, exp} =>
                               { fname_args = map_fname_args fname_args
                               , ty = Option.map map_ty ty
                               }
                             )
                           )
                           fvalbinds
                     }
            | Dtype typbinds =>
                Dtype (
                  List.map
                    (fn {tyvars, tycon, ty} => {tyvars = tyvars, tycon = tycon, ty = map_ty ty})
                    typbinds
                )
            | Ddatdec {datbinds, withtypee} =>
                Ddatdec
                  { datbinds = List.map map_datbind datbinds
                  , withtypee = Option.map (List.map map_typbind) withtypee
                  }
            | Dabstype {datbinds, withtypee, withh} =>
                Dabstype
                  { datbinds = List.map map_datbind datbinds
                  , withtypee = Option.map (List.map map_typbind) withtypee
                  , withh = map_dec withh
                  }
            | Dexception exbinds =>
                Dexception
                  (List.map
                    (fn Xnew {opp, id, ty} =>
                        Xnew {opp = opp, id = id, ty = Option.map map_ty ty}
                    | other as Xrepl _ => other
                    )
                    exbinds
                  )
            | Dlocal {left_dec, right_dec} =>
                Dlocal { left_dec = map_dec left_dec, right_dec = map_dec }
            | Dseq decs =>
                Dseq (List.map map_dec decs)
            | ( Dopen _
              | Ddatrepl _
              | Dinfix
              | Dinfixr
              | Dnonfix
              | Dhole _ ) => dec
          ) |> f_dec

        and map_ty ty =
          case ty of
            ( Tident _
            | Ttyvar _
            ) => ty
          | Tapp (tys, longid) => Tapp (List.map map_ty tys, longid)
          | Tprod tys => Tprod (List.map map_ty tys)
          | Tarrow (ty1, ty2) => Tarrow (map_ty ty1, map_ty ty2)
          | Trecord fields =>

      in

      end
     *)
  end
