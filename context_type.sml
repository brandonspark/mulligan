
structure SymbolOrdered =
  struct
    type t = SMLSyntax.symbol

    val compare = Symbol.compare
    val eq = Symbol.eq
  end
structure SymDict = RedBlackDict(structure Key = SymbolOrdered)

structure SymSet = SymbolRedBlackSet

structure ContextType =
  struct
    type symbol = SMLSyntax.symbol
    type longid = SMLSyntax.longid
    type pat = SMLSyntax.pat
    type exp = SMLSyntax.exp
    type ty = SMLSyntax.ty

    type 'a dict = 'a SymDict.dict

    type tyval = { arity : int
                 , cons : { id : symbol, ty : ty option } list
                 }

    datatype value =
        Vnumber of SMLSyntax.number
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
          , env : t
          , rec_env : scope option
          , break : symbol option ref
          }
      | Vbasis of { name : symbol, function : value -> value }

    and sigval =
      Sigval of
        { valspecs : SymSet.set
        , dtyspecs : { arity : int
                     , cons : { id : symbol, ty : ty option } list
                     } dict
        (* TODO: type stuff , tyspecs : ty option dict *)
        , exnspecs : SymSet.set
        , modspecs : sigval dict
        }


    and functorval =
      Functorval of
        { arg_seal : { id : symbol option, sigval : sigval }
        , seal : { opacity : SMLSyntax.opacity, sigval : sigval } option
        , body : SMLSyntax.module
        }

    and scope =
      Scope of
        { valdict : valdict
        , condict : condict
        , exndict : exndict
        , moddict : moddict
        , infixdict : infixdict
        , tydict : tydict
        }

    and infixity = LEFT | RIGHT

    withtype valdict = value dict
    and condict = SymSet.set
    and exndict = SymSet.set
    and infixdict = (infixity * int) dict
    and tydict = tyval dict
    and moddict = scope dict

    and t =
      { scope : scope
      , outer_scopes : scope list
      , sigdict : sigval dict
      , functordict : functorval dict
      , hole_print_fn : unit -> PrettySimpleDoc.t
      , settings :
          { break_assigns : SymSet.set ref
          , substitute : bool ref
          }
      }
  end

signature CONTEXTTYPE =
  sig
    type symbol = ContextType.symbol
    type longid = ContextType.longid
    type pat = ContextType.pat
    type exp = ContextType.exp
    type ty = ContextType.ty

    type 'a dict = 'a ContextType.dict

    type valdict = ContextType.valdict
    type condict = ContextType.condict
    type exndict = ContextType.exndict
    type infixdict = ContextType.infixdict
    type tydict = ContextType.tydict
    type tyval = ContextType.tyval
    type moddict = ContextType.moddict

    datatype value = datatype ContextType.value

    datatype sigval = datatype ContextType.sigval

    datatype functorval = datatype ContextType.functorval

    datatype scope = datatype ContextType.scope

    datatype infixity = datatype ContextType.infixity

    type t = ContextType.t
  end
