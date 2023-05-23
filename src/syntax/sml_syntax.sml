(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The representation for SML syntax, along with some other types which 
 * must be manipulated over the debugger's runtime.
 *)

(*****************************************************************************)
(* Helper structures *)
(*****************************************************************************)

structure SymbolOrdered =
  struct
    type t = Symbol.symbol

    val compare = Symbol.compare
    val eq = Symbol.eq
  end
structure SymDict = RedBlackDict(structure Key = SymbolOrdered)

structure SymSet = SymbolRedBlackSet

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type settings =
  { break_assigns : SymSet.set ref
  , substitute : bool ref
  , print_all : bool ref
      (** Should we just print the entire program? 
        * Supersedes `print_dec` 
        *)
  , print_dec : bool ref
      (** Should we just print until the nearest val binding?
        * Supersedes `print_depth` 
        *)
  , print_depth : int ref
      (** How many layers of the evaluation context should be reported by
        * the evaluation trace during pretty printing? 
        *)
  , pause_currying : bool ref
      (** Should we trigger an event on the intermediate steps of a
        * curried function application? Such as:
        * (fn x => fn y => 1) 2 3 
        *)
  , pause_app : bool ref
  , pause_arithmetic : bool ref
      (** Should ordinary arithmetic operators such as `+`, `-`, `div`, `*`
        * be stepped immediately, rather than triggering an event?
        *)
  }

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure SMLSyntax =
  struct
    type symbol = Symbol.symbol
    type longid = symbol list

    type settings = settings

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

    (****************************)
    (*        TYPE VALUES       *)
    (****************************)

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
      | SPtype of 
          { tyvars : symbol list,
            tycon : symbol,
            ty : ty option
          } list
      | SPeqtype of { tyvars : symbol list, tycon : symbol } list
      | SPdatdec of {
          tyvars : symbol list,
          tycon : symbol,
          condescs : 
            { id : symbol,
              ty : ty option
            } list
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

    (****************************)
    (*          VALUES          *)
    (****************************)

    (* While we have a representation for expressions already, the debugger 
     * is interested in trying to step expressions to values. In this case, 
     * it is sometimes useful to have a first-class notion of values.
     * 
     * This type is that representation. It represents a step away from the
     * literal concrete text of the manipulated program, and goes towards
     * something more abstract that can be used. 
     *)
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
      | Vexn of
          { name : longid
          , exnid : ExnId.t
          , arg : value option
          }
      | Vfn of
          { matches : { pat : pat, exp : exp } list
          , env : context
          , abstys : type_scheme AbsIdDict.dict
          , rec_env : scope option
          , break : symbol option ref
          }
      | Vbasis of { name : symbol, function : value -> value, is_infix : bool }

    (* We also have a notion of values for signatures. 
     * 
     * A signature is parsed into a `SMLSyntax.signat`, which does not
     * do anything like resolving abstract types, minting new TyIds for
     * datatypes, or otherwise processing the textual data into something
     * that can be used. This datatype can be more nicely manipulated
     * when we do things like signature ascription in the statics. 
     *)
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

    (* Type specifications can be abstract or concrete.
     *)
    and typspec_status =
        Abstract of int * AbsId.t
      | Concrete of type_scheme

    and functorval =
      Functorval of
        { arg_seal : { id : symbol option, sigval : sigval }
        , seal : { opacity : opacity, sigval : sigval } option
        , body : module
        }

    (****************************)
    (*          SCOPE          *)
    (****************************)

    (* Scopes are a reified notion of the bindings which exist at a 
     * particular lexical scope.
     * 
     * In particular, we need to keep track of things like value bindings,
     * type bindings, and module bindings, which all have separate
     * namespaces. There are also infixities.
     *
     *)
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
          (* TODO: combine these two `identdict` and `valtydict` *)
        { identdict : identdict (* identifiers -> values *)
        , valtydict : valtydict (* val identifiers -> types *)
        , moddict : moddict (* maps to module scopes *)
        , infixdict : infixdict (* all currently infixed operators *)
        , tynamedict : tynamedict
        }

    and infixity = LEFT | RIGHT

    withtype identdict = id_info dict
    and infixdict = (infixity * int) dict
    and valtydict = (sign * type_scheme) dict
    and moddict = scope dict
    and tynamedict = synonym dict

    (****************************)
    (*       EXPOSED TYPES      *)
    (****************************)

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

    (****************************)
    (*     CONTEXT AND SCOPE    *)
    (****************************)

    (* The context we carry around throughout evaluation of the debugger.
     * 
     * There's a few things in here that are useful:
     * - scope and outer_scopes, which detail the various scopes we are in
     * - sigdict and functordict, which are top-level module information
     * - dtydict, which is a persistent source of information about each
     *   datatype's definition.
     *   We need this around because we may have to still reference a
     *   datatype long after it's out of scope.
     *   THINK: I'm not convinced this (or the statics) are necessary.
     * - tyvars, which is a set of all the tyvars currently in scope.
     * - hole_print_fn, which we use for the debugger's pretty-printer
     * - settings, which holds global settings for the debugger
     *)

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
      | Sigdec of {id : symbol, signat : signat} list
      | Fundec of fundec
      | Thole

    type ast = topdec list
  end