
structure PreSMLSyntax =
  struct
    type symbol = Symbol.symbol
    type longid = symbol list

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
          id : symbol
        }
      | Pconstr of { opp : bool, id : longid }
      | Precord of patrow list
      | Pparens of pat
      | Punit
      | Ptuple of pat list
      | Plist of pat list
      | Por of pat list

      (* pats *)
      | Papp of {
          opp : bool
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
        ty : ty,
      }
    type datbind = {
        tyvars : symbol list,
        tycon : symbol,
        conbinds : conbind list,
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
      | Econstr of {
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
      | Efn of { pat : pat, exp : exp } list

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

    and fvalbinds =
      { fname_args : fname_args
      , ty : ty option
      , exp : exp
      } list list

    and dec =
        Dval of {
          tyvars : symbol list,
          valbinds : { recc : bool, pat : pat, exp : exp } list
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

    (****************************)
    (*         MODULES          *)
    (****************************)

    type condesc = {
        id : symbol,
        ty : ty option
      }

    type typdesc = {
        tyvars : symbol list,
        tycon : symbol,
        ty : ty option,
      }

    datatype strdec =
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
        }
      | SPtype of typdesc
      | SPeqtype of typdesc
      | SPdatdec of {
          tyvars : symbol list,
          tycon : symbol,
          condescs : condesc list,
        }
      | SPdatrepl of {
          left_tycon : symbol,
          right_tycon : longid
        }
      | SPexception of {
          id : symbol,
          ty : ty option
        }
      | SPmodule of {
          id : symbol,
          signat : signat
        }
      | SPinclude of signat
      | SPinclude_ids of symbol list
      | SPsharing of {
          spec : spec
          tycons : longid list (* longtycon1 = .. = longtycon_n *)
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

    type ast = topdec list
  end

signature SMLSYNTAX =
  sig
    type symbol = PreSMLSyntax.symbol
    type span = PreSMLSyntax.span
    type symbol = PreSMLSyntax.symbol
    type longid = PreSMLSyntax.longid

    val map_sym : symbol -> (string -> string) -> symbol

    (* DERIVING *)
    type setting = symbol * symbol
    type settings = setting list

    type plugin = symbol * settings
    type plugins = plugin list

    (* TYPES *)

    type ty = PreSMLSyntax.ty

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    type pat = PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    type dec = PreSMLSyntax.dec
    type exp = PreSMLSyntax.exp

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype opacity = datatype PreSMLSyntax.opacity
    datatype funarg_app = datatype PreSMLSyntax.funarg_app

    type module = PreSMLSyntax.module
    type strdec = PreSMLSyntax.strdec
    type signat = PreSMLSyntax.signat
    type spec = PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    datatype funarg = datatype PreSMLSyntax.funarg

    type funbind = PreSMLSyntax.funbind
    type funbinds = PreSMLSyntax.funbinds
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)

    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end

structure SMLSyntax : SMLSYNTAX =
  struct
    type symbol = PreSMLSyntax.symbol
    type span = PreSMLSyntax.span
    type symbol = PreSMLSyntax.symbol
    type longid = PreSMLSyntax.longid

    fun map_sym sym f =
      Symbol.fromValue (f (Symbol.toValue sym))

    (* DERIVING *)
    type setting = PreSMLSyntax.setting
    type settings = PreSMLSyntax.settings

    type plugin = PreSMLSyntax.plugin
    type plugins = PreSMLSyntax.plugins

    (* TYPES *)

    type ty = PreSMLSyntax.ty

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    type pat = PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    type dec = PreSMLSyntax.dec
    type exp = PreSMLSyntax.exp

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype opacity = datatype PreSMLSyntax.opacity
    datatype funarg_app = datatype PreSMLSyntax.funarg_app

    type module = PreSMLSyntax.module
    type strdec = PreSMLSyntax.strdec
    type signat = PreSMLSyntax.signat
    type spec = PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    datatype funarg = datatype PreSMLSyntax.funarg

    type funbind = PreSMLSyntax.funbind
    type funbinds = PreSMLSyntax.funbinds
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)
    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end
