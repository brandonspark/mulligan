
structure SymbolOrdered =
  struct
    type t = Symbol.symbol

    val compare = Symbol.compare
    val eq = Symbol.eq
  end
structure SymDict = RedBlackDict(structure Key = SymbolOrdered)

structure SymSet = SymbolRedBlackSet

structure PreSMLSyntax =
  struct
    type symbol = Symbol.symbol
    type longid = symbol list

    type 'a dict = 'a SymDict.dict

    (****************************)
    (*         TYPES            *)
    (****************************)

    datatype ty =
        Tident of longid
      | Ttyvar of symbol
      | Tapp of ty list * longid
      | Tprod of ty list
      | Tarrow of ty * ty
      | Trecord of {lab: symbol, ty: ty} list
      | Tparens of ty

    datatype tyval =
        TVtyvar of symbol
      | TVapp of tyval list * TyId.t
      | TVprod of tyval list
      | TVarrow of tyval * tyval
      | TVrecord of {lab: symbol, tyval: tyval} list
      | TVvar of restrict option Ref.t
      | TVabs of tyval list * AbsId.t

    and restrict =
        Rows of {lab: symbol, tyval: tyval} list
      | Ty of tyval

    type type_scheme = int * (tyval list -> tyval)

    datatype synonym =
        Datatype of TyId.t
      | Scheme of type_scheme

    (* These are the type variables which the type inference algorithm has so
     * far been able to figure out are currently in use.
     * This means that later on, when we discover different type variables, we
     * can generalize them at their bind site.
     *)
    datatype tyvar =
        Proper of symbol
      | Unconstrained of restrict option Ref.t

    fun tyvar_eq (t1, t2) =
      case (t1, t2) of
        (Proper s1, Proper s2) => Symbol.eq (s1, s2)
      | (Unconstrained r1, Unconstrained r2) => r1 = r2
      | _ => false

    (****************************)
    (*        PATTERNS          *)
    (****************************)

    datatype patrow =
        PRellipsis
      | PRlab of {
          lab : symbol,
          pat : pat
        }
      | PRas of {
          id : symbol,
          ty : ty option,
          aspat : pat option
        }

    and pat =
      (* scons *)
        Pnumber of int
      | Pword of symbol
      | Pstring of symbol
      | Pchar of char

      (* atpats *)
      | Pwild
      | Pident of {
          opp : bool,
          id : longid
        }
      | Precord of patrow list
      | Pparens of pat
      | Punit
      | Ptuple of pat list
      | Plist of pat list
      | Por of pat list

      (* pats *)
      | Papp of {
          opp : bool,
          id : longid,
          atpat : pat
        }
      | Pinfix of {
          left : pat,
          id : symbol,
          right : pat
        }
      | Ptyped of {
          pat : pat,
          ty : ty
        }
      | Playered of {
          opp : bool,
          id : symbol,
          ty : ty option,
          aspat : pat
        }

    (****************************)
    (*       EXPRESSIONS        *)
    (****************************)

    datatype exbind =
        Xnew of {
          opp : bool,
          id : symbol,
          ty : ty option
        }
      | Xrepl of {
          opp : bool,
          left_id : symbol,
          right_id : longid
        }

    datatype number =
        Int of int
      | Word of string
      | Real of real

    type conbind = {
        opp : bool,
        id : symbol,
        ty : ty option
      }
    type typbind = {
        tyvars : symbol list,
        tycon : symbol,
        ty : ty
      }
    type datbind = {
        tyvars : symbol list,
        tycon : symbol,
        conbinds : conbind list
      }

    type dtyinfo = { arity : int
                   , cons : { id : symbol, tyscheme : type_scheme } list
                   }
    type dtydict = dtyinfo TyIdDict.dict

    type settings =
      { break_assigns : SymSet.set ref
      , substitute : bool ref
      , step_app : bool ref
      , step_arithmetic : bool ref
      , print_dec : bool ref
      , print_depth : int ref
      }

    datatype exp =
        Enumber of number (* int, real, hex, ... *)
      | Estring of symbol
      | Echar of char
      | Erecord of {
          lab : symbol,
          exp : exp
        } list
      | Eselect of symbol
      | Eunit
      | Eident of {
          opp : bool,
          id : longid
        }
      | Etuple of exp list
      | Elist of exp list
      | Eseq of exp list
      | Elet of {
          dec : dec,
          exps : exp list
        }
      | Eparens of exp
      | Eapp of {
          left : exp,
          right : exp
        }
      | Einfix of {
          left : exp,
          id : symbol,
          right : exp
        }
      | Etyped of {
          exp : exp,
          ty : ty
        }
      | Eandalso of {
          left : exp,
          right : exp
        }
      | Eorelse of {
          left : exp,
          right : exp
        }
      | Ehandle of {
          exp : exp,
          matches : { pat : pat, exp : exp } list
        }
      | Eraise of exp
      | Eif of {
          exp1 : exp,
          exp2 : exp,
          exp3 : exp
        }
      | Ewhile of {
          exp1 : exp,
          exp2 : exp
        }
      | Ecase of {
          exp : exp,
          matches : { pat : pat, exp : exp } list
        }
      | Efn of { pat : pat, exp : exp } list * context option

      | Ehole (* just for debugging purposes *)

    and fname_args =
        Fprefix of { opp : bool
                   , id : symbol
                   , args : pat list
                   }
      | Finfix of { left : pat
                  , id : symbol
                  , right : pat
                  }
      | Fcurried_infix of { left : pat
                          , id : symbol
                          , right : pat
                          , args : pat list
                          }

    and dec =
        Dval of {
          tyvars : symbol list,
          valbinds : valbinds
        }
      | Dfun of { (* need to do something about infixed function names *)
          tyvars : symbol list,
          fvalbinds : fvalbinds
        }
      | Dtype of typbind list
      | Ddatdec of {
          datbinds : datbind list,
          withtypee : typbind list option
        }
      | Ddatrepl of {
          left_tycon : symbol,
          right_tycon : longid
        }
      | Dabstype of {
          datbinds : datbind list,
          withtypee : typbind list option,
          withh : dec
        }
      | Dexception of exbind list
      | Dlocal of {
          left_dec : dec,
          right_dec : dec
        }
      | Dopen of longid list
      | Dseq of dec list (* should not be nested *)
      | Dinfix of {
          precedence : int option,
          ids : symbol list
        }
      | Dinfixr of {
          precedence : int option,
          ids : symbol list
        }
      | Dnonfix of symbol list
      | Dhole


    (****************************)
    (*          VALUES          *)
    (****************************)


    and value =
        Vnumber of number
      | Vstring of symbol
      | Vchar of char
      | Vrecord of
          { lab : symbol
          , value : value
          } list
      | Vunit
      | Vconstr of
          { id : longid
          , arg : value option
          }
      | Vselect of symbol
      | Vtuple of value list
      | Vlist of value list
      | Vinfix of
          { left : value
          , id : symbol
          , right : value
          }
      | Vfn of
          { matches : { pat : pat, exp : exp } list
          , env : context
          , abstys : type_scheme AbsIdDict.dict
          , rec_env : scope option
          , break : symbol option ref
          }
      | Vbasis of { name : symbol, function : value -> value }

    and typspec_status =
        Abstract of int * AbsId.t
      | Concrete of type_scheme

    and sigval =
      Sigval of
        { valspecs : (type_scheme * AbsId.t list) dict
        , tyspecs : { equality : bool, status : typspec_status } dict
        , dtyspecs : { arity : int
                     , tyid : TyId.t
                     , cons : { id : symbol, tyscheme : type_scheme } list
                     } dict
        (* TODO: type stuff , tyspecs : ty option dict *)
        , exnspecs : type_scheme dict
        , modspecs : sigval dict
        }


    and functorval =
      Functorval of
        { arg_seal : { id : symbol option, sigval : sigval }
        , seal : { opacity : opacity, sigval : sigval } option
        , body : module
        }

    and id_info =
        V of value
      | C of TyId.t
      | E of ExnId.t

    and sign =
        Vsign
      | Csign
      | Esign

    and scope =
      Scope of
          (* TODO: combine these three *)
        { identdict : identdict (* identifiers -> values *)
        , valtydict : valtydict (* val identifiers -> types *)
        , moddict : moddict (* maps to module scopes *)
        , infixdict : infixdict (* all currently infixed operators *)
        , tynamedict : tynamedict
        }

    and infixity = LEFT | RIGHT

    (****************************)
    (*         MODULES          *)
    (****************************)

    and strdec =
        DMdec of dec
      | DMstruct of {
          id : symbol,
          seal : { opacity : opacity, signat : signat } option,
          module : module
        } list
      | DMlocal of {
          left_dec : strdec,
          right_dec : strdec
        }
      | DMseq of strdec list
      | DMhole

    and module =
        Mident of longid
      | Mstruct of strdec
      | Mseal of {
          module : module,
          opacity : opacity,
          signat : signat
        }
      | Mapp of {
          functorr : symbol,
          arg : funarg_app
        }
      | Mlet of {
          dec : strdec,
          module : module
        }
      | Mhole

    and signat =
        Sspec of spec
      | Sident of symbol
      | Swhere of {
          signat : signat,
          wheretypee : {
            tyvars : symbol list,
            id : longid,
            ty : ty
          } list
        }

    and spec =
        SPval of {
          id : symbol,
          ty : ty
        } list
      | SPtype of typdesc list
      | SPeqtype of { tyvars : symbol list, tycon : symbol } list
      | SPdatdec of {
          tyvars : symbol list,
          tycon : symbol,
          condescs : condesc list
        } list
      | SPdatrepl of {
          left_tycon : symbol,
          right_tycon : longid
        }
      | SPexception of {
          id : symbol,
          ty : ty option
        } list
      | SPmodule of {
          id : symbol,
          signat : signat
        } list
      | SPinclude of signat
      | SPinclude_ids of symbol list
      | SPsharing_type of {
          spec : spec,
          tycons : longid list (* longtycon1 = .. = longtycon_n *)
        }
      | SPsharing of {
          spec : spec,
          tycons : longid list (* longstrid1 = .. = longstrid_n *)
        }
      | SPseq of spec list

    and opacity =
        Transparent
      | Opaque

    and funarg =
        Normal of {id : symbol, signat : signat}
      | Sugar of spec

    and funarg_app =
        Normal_app of module
      | Sugar_app of strdec

    withtype condesc = {
        id : symbol,
        ty : ty option
      }

    and identdict = id_info dict
    and infixdict = (infixity * int) dict
    and valtydict = (sign * type_scheme) dict
    and moddict = scope dict
    and tynamedict = synonym dict

    and typdesc = {
        tyvars : symbol list,
        tycon : symbol,
        ty : ty option
      }

    and context =
      { scope : scope
      , outer_scopes : scope list
      , dtydict : dtydict ref
      , sigdict : sigval dict
      , functordict : functorval dict
      , tyvars : SymSet.set
      , hole_print_fn : unit -> PrettySimpleDoc.t
      , settings : settings
      , abstys : type_scheme AbsIdDict.dict
      }

    and fvalbinds =
      { fname_args : fname_args
      , ty : ty option
      , exp : exp
      } list list

    and valbinds =
      { recc : bool
      , pat : pat
      , exp : exp
      } list

    type sigbinds = {id : symbol, signat : signat} list
    type sigdec = sigbinds

    (****************************)
    (*        FUNCTORS          *)
    (****************************)

    type funbind = {
        id : symbol,
        funarg : funarg,
        seal : { signat : signat, opacity : opacity } option,
        body : module
      }
    type fundec = funbind list

    (****************************)
    (*         TOPDECS          *)
    (****************************)

    datatype topdec =
        Strdec of strdec
      | Sigdec of sigdec
      | Fundec of fundec
      | Thole

    type ast = topdec list
  end

signature SMLSYNTAX =
  sig
    type symbol = PreSMLSyntax.symbol
    type longid = PreSMLSyntax.symbol list

    val map_sym : symbol -> (string -> string) -> symbol
    val longid_eq : longid * longid -> bool
    val longid_to_str : longid -> string
    val tyvar_eq : PreSMLSyntax.tyvar * PreSMLSyntax.tyvar -> bool
    val guard_tyscheme : PreSMLSyntax.type_scheme -> PreSMLSyntax.type_scheme
    val number_eq : PreSMLSyntax.number * PreSMLSyntax.number -> bool

    (* TYPES *)

    datatype ty = datatype PreSMLSyntax.ty
    datatype tyval = datatype PreSMLSyntax.tyval
    datatype restrict = datatype PreSMLSyntax.restrict
    datatype tyvar = datatype PreSMLSyntax.tyvar
    type type_scheme = PreSMLSyntax.type_scheme
    datatype synonym = datatype PreSMLSyntax.synonym

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    datatype pat = datatype PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    datatype dec = datatype PreSMLSyntax.dec
    datatype exp = datatype PreSMLSyntax.exp
    datatype fname_args = datatype PreSMLSyntax.fname_args

    type fvalbinds = PreSMLSyntax.fvalbinds
    type valbinds = PreSMLSyntax.valbinds

    datatype value = datatype PreSMLSyntax.value
    datatype typspec_status = datatype PreSMLSyntax.typspec_status
    datatype sigval = datatype PreSMLSyntax.sigval
    datatype functorval = datatype PreSMLSyntax.functorval

    datatype scope = datatype PreSMLSyntax.scope
    datatype id_info = datatype PreSMLSyntax.id_info
    datatype sign = datatype PreSMLSyntax.sign

    datatype infixity = datatype PreSMLSyntax.infixity

    type identdict = PreSMLSyntax.identdict
    type valtydict = PreSMLSyntax.valtydict
    type infixdict = PreSMLSyntax.infixdict
    type dtydict = PreSMLSyntax.dtydict
    type moddict = PreSMLSyntax.moddict
    type tynamedict = PreSMLSyntax.tynamedict

    type dtyinfo = PreSMLSyntax.dtyinfo
    type settings = PreSMLSyntax.settings

    type context = PreSMLSyntax.context

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype opacity = datatype PreSMLSyntax.opacity
    datatype funarg_app = datatype PreSMLSyntax.funarg_app

    datatype module = datatype PreSMLSyntax.module
    datatype strdec = datatype PreSMLSyntax.strdec
    datatype signat = datatype PreSMLSyntax.signat
    datatype spec = datatype PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    datatype funarg = datatype PreSMLSyntax.funarg

    type funbind = PreSMLSyntax.funbind
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)

    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end

structure SMLSyntax : SMLSYNTAX =
  struct
    type symbol = PreSMLSyntax.symbol
    type longid = PreSMLSyntax.symbol list

    fun map_sym sym f =
      Symbol.fromValue (f (Symbol.toValue sym))
    fun longid_eq (l1, l2) =
      ListPair.allEq Symbol.eq (l1, l2)
    fun longid_to_str longid =
      String.concatWith "." (List.map Symbol.toValue longid)

    val tyvar_eq = PreSMLSyntax.tyvar_eq
    fun guard_tyscheme (n, ty_fn) =
      ( n
      , fn tyvals =>
          if List.length tyvals <> n then
            raise Fail "Instantiated type scheme with incorrect number of tyargs"
          else
            ty_fn tyvals
      )

    local
      open PreSMLSyntax
    in
      fun number_eq (n1, n2) =
        case (n1, n2) of
          (Int i1, Int i2) => i1 = i2
        | (Real _, Real _) => raise Fail "comparing reals for equality"
        | (Word w1, Word w2) => w1 = w2
        | _ => false
    end

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





    (* TYPES *)

    datatype ty = datatype PreSMLSyntax.ty
    datatype tyval = datatype PreSMLSyntax.tyval
    datatype restrict = datatype PreSMLSyntax.restrict
    datatype tyvar = datatype PreSMLSyntax.tyvar
    type type_scheme = PreSMLSyntax.type_scheme
    datatype synonym = datatype PreSMLSyntax.synonym

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    datatype pat = datatype PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    datatype dec = datatype PreSMLSyntax.dec
    datatype exp = datatype PreSMLSyntax.exp
    datatype fname_args = datatype PreSMLSyntax.fname_args

    type fvalbinds = PreSMLSyntax.fvalbinds
    type valbinds = PreSMLSyntax.valbinds

    datatype value = datatype PreSMLSyntax.value
    datatype typspec_status = datatype PreSMLSyntax.typspec_status
    datatype sigval = datatype PreSMLSyntax.sigval
    datatype functorval = datatype PreSMLSyntax.functorval

    datatype scope = datatype PreSMLSyntax.scope
    datatype id_info = datatype PreSMLSyntax.id_info
    datatype sign = datatype PreSMLSyntax.sign

    datatype infixity = datatype PreSMLSyntax.infixity

    type identdict = PreSMLSyntax.identdict
    type valtydict = PreSMLSyntax.valtydict
    type infixdict = PreSMLSyntax.infixdict
    type dtydict = PreSMLSyntax.dtydict
    type moddict = PreSMLSyntax.moddict
    type tynamedict = PreSMLSyntax.tynamedict

    type dtyinfo = PreSMLSyntax.dtyinfo
    type settings = PreSMLSyntax.settings

    type context = PreSMLSyntax.context

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype opacity = datatype PreSMLSyntax.opacity
    datatype funarg_app = datatype PreSMLSyntax.funarg_app

    datatype module = datatype PreSMLSyntax.module
    datatype strdec = datatype PreSMLSyntax.strdec
    datatype signat = datatype PreSMLSyntax.signat
    datatype spec = datatype PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    datatype funarg = datatype PreSMLSyntax.funarg

    type funbind = PreSMLSyntax.funbind
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)
    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end
