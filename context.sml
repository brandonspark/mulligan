
structure SymbolOrdered =
  struct
    type t = SMLSyntax.symbol

    val compare = Symbol.compare
    val eq = Symbol.eq
  end
structure SymDict = RedBlackDict(structure Key = SymbolOrdered)

structure SymSet = SymbolRedBlackSet

structure Context :
  sig
    type symbol = SMLSyntax.symbol

    datatype infixity = LEFT | RIGHT

    (* The outer scopes are a list, in order of ascending outwardness.
     * Each scope maps to the stuff in it, and all things in scope are within
     * the current value of outer_scopes.
     *)

    type 'a dict = 'a SymDict.dict

    type t

    type scope

    datatype value =
      Vnumber of SMLSyntax.number
    | Vstring of symbol
    | Vchar of char
    | Vrecord of
        { lab : symbol
        , value : value
        } list
    | Vunit
    | Vident of SMLSyntax.longid
    | Vconstr of
        { id : SMLSyntax.longid
        , arg : value option
        }
    | Vselect of SMLSyntax.symbol
    | Vtuple of value list
    | Vlist of value list
    | Vinfix of
        { left : value
        , id : SMLSyntax.symbol
        , right : value
        }
    | Vfn of { pat : SMLSyntax.pat, exp : SMLSyntax.exp } list
           * t
           * scope

    and sigval =
      Sigval of
        { valspecs : SymSet.set
        , dtyspecs : { arity : int
                     , cons : { id : SMLSyntax.symbol, ty : SMLSyntax.ty option } list
                     } dict
        (* TODO: type stuff , tyspecs : ty option dict *)
        , exnspecs : SymSet.set
        , modspecs : sigval dict
        }


    and functorval =
      Functorval of
        { arg_seal : { id : SMLSyntax.symbol option, sigval : sigval }
        , seal : { opacity : SMLSyntax.opacity, sigval : sigval } option
        , body : SMLSyntax.module
        }

    (* Context stuff. *)

    val scope_empty : scope

    val get_val : t -> SMLSyntax.longid -> value
    val get_module : t -> SMLSyntax.longid -> scope
    val get_sig : t -> SMLSyntax.symbol -> sigval
    val get_functor : t -> SMLSyntax.symbol -> functorval

    val add_exn : t -> SMLSyntax.symbol -> t
    val add_con : t -> SMLSyntax.symbol -> t
    val add_infix : t -> (SMLSyntax.symbol * infixity * int) -> t
    val add_module : t -> SMLSyntax.symbol -> scope -> t

    val add_funbind : t -> SMLSyntax.funbind -> t

    val remove_infix : t -> SMLSyntax.symbol -> t

    val enter_scope : t -> t
    val exit_scope : SMLSyntax.symbol -> t -> t
    val pop_scope : t -> scope
    val pop_penultimate : t -> t

    val open_scope : t -> scope -> t
    val open_path : t -> SMLSyntax.symbol list -> t

    val add_bindings : t -> (SMLSyntax.symbol * value) list -> t
    val add_rec_bindings : t -> (SMLSyntax.symbol * value) list -> t

    (* Value stuff. *)

    val value_to_exp : value -> SMLSyntax.exp

    val apply_fn :
           {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list * t * scope
        -> value
        -> t * SMLSyntax.exp

    val match_against :
         t
      -> {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list
      -> value
      -> t * SMLSyntax.exp

    val match_pat : SMLSyntax.pat -> value -> (SMLSyntax.symbol * value) list

    val evaluate_signat : t -> SMLSyntax.signat -> sigval

    val generate_sigbinding :
         t
      -> {id : SMLSyntax.symbol, signat : SMLSyntax.signat}
      -> SMLSyntax.symbol * sigval

    val add_sigbindings :
         t
      -> (SMLSyntax.symbol * sigval) list
      -> t

    (* Type stuff. *)

    val add_typbinds : t -> SMLSyntax.typbind list -> t
    val replicate_datatype : t -> SMLSyntax.symbol * SMLSyntax.symbol list -> t
    val replicate_exception : t -> SMLSyntax.symbol * SMLSyntax.symbol list -> t

    val add_datatype : t -> SMLSyntax.datbind -> t

    val ascribe : scope -> { opacity : SMLSyntax.opacity
                           , sigval : sigval
                           } option -> scope


  end =
  struct
    open SMLSyntax

    (* Helpers *)

    infix |>
    fun x |> f = f x

    fun concatMap f = List.concat o List.map f

    fun snoc l =
      ( List.take (l, List.length l - 1)
      , List.nth (l, List.length l - 1)
      )

    fun longid_eq (l1, l2) =
      ListPair.allEq Symbol.eq (l1, l2)

    fun dict_from_list l =
      List.foldl
        (fn ((key, elem), dict) =>
          SymDict.insert dict key elem
        )
        SymDict.empty
        l

    (* Some types *)

    type symbol = symbol

    type 'a dict = 'a SymDict.dict

    datatype value =
      Vnumber of number
    | Vstring of symbol
    | Vchar of char
    | Vrecord of
        { lab : symbol
        , value : value
        } list
    | Vunit
    | Vident of longid
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
    | Vfn of { pat : pat, exp : exp } list * t * scope

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
        , seal : { opacity : opacity, sigval : sigval } option
        , body : module
        }

    and scope =
      Scope of
        { valdict : value dict
        , condict : SymSet.set
        , exndict : SymSet.set
        , moddict : scope dict
        , infixdict : (infixity * int) dict
        , tydict : { arity : int
                   , cons : { id : symbol, ty : ty option } list
                   } dict
        }

    and infixity = LEFT | RIGHT

    withtype t =
      { scope : scope
      , outer_scopes : scope list
      , sigdict : sigval dict
      , functordict : functorval dict
      }

    fun lift f {scope, outer_scopes, sigdict, functordict} =
      let
        val (scope, outer_scopes) = f (scope, outer_scopes)
      in
        { scope = scope
        , outer_scopes = outer_scopes
        , sigdict = sigdict
        , functordict = functordict
        }
      end

    (* Evaluate each expression to a "normal form" for the value.

    exception Raise of exp

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

    (* CONTEXT STUFF *)

    val scope_empty =
      Scope
        { valdict = SymDict.empty
        , condict = SymSet.empty
        , exndict = SymSet.empty
        , moddict = SymDict.empty
        , infixdict = SymDict.empty
        , tydict = SymDict.empty
        }

    fun scope_valdict (Scope {valdict, ...}) = valdict
    fun scope_condict (Scope {condict, ...}) = condict
    fun scope_exndict (Scope {exndict, ...}) = exndict
    fun scope_moddict (Scope {moddict, ...}) = moddict
    fun scope_infixdict (Scope {infixdict, ...}) = infixdict
    fun scope_tydict (Scope {tydict, ...}) = tydict

    fun scope_set_valdict (Scope { valdict, condict, exndict, moddict
                                 , infixdict, tydict}) new =
      Scope { valdict = new
            , condict = condict
            , exndict = exndict
            , moddict = moddict
            , infixdict = infixdict
            , tydict = tydict
            }
    fun scope_set_condict (Scope { valdict, condict, exndict, moddict
                                 , infixdict, tydict}) new =
      Scope { valdict = valdict
            , condict = new
            , exndict = exndict
            , moddict = moddict
            , infixdict = infixdict
            , tydict = tydict
            }
    fun scope_set_exndict (Scope { valdict, condict, exndict, moddict
                                 , infixdict, tydict}) new =
      Scope { valdict = valdict
            , condict = condict
            , exndict = new
            , moddict = moddict
            , infixdict = infixdict
            , tydict = tydict
            }
    fun scope_set_moddict (Scope { valdict, condict, exndict, moddict
                                 , infixdict, tydict}) new =
      Scope { valdict = valdict
            , condict = condict
            , exndict = exndict
            , moddict = new
            , infixdict = infixdict
            , tydict = tydict
            }
    fun scope_set_infixdict (Scope { valdict, condict, exndict, moddict
                                   , infixdict, tydict}) new =
      Scope { valdict = valdict
            , condict = condict
            , exndict = exndict
            , moddict = moddict
            , infixdict = new
            , tydict = tydict
            }
    fun scope_set_tydict (Scope { valdict, condict, exndict, moddict
                                   , infixdict, tydict}) new =
      Scope { valdict = valdict
            , condict = condict
            , exndict = exndict
            , moddict = moddict
            , infixdict = infixdict
            , tydict = new
            }

    fun merge_scope ctx scope =
      lift (fn (cur_scope, ctx) =>
        ( Scope
            { valdict =
                SymDict.union
                  (scope_valdict cur_scope)
                  (scope_valdict scope)
                  (fn (_, _, snd) => snd)
            , condict =
                SymSet.union
                  (scope_condict cur_scope)
                  (scope_condict scope)
            , moddict =
                SymDict.union
                  (scope_moddict cur_scope)
                  (scope_moddict scope)
                  (fn (_, _, snd) => snd)
            , exndict =
                SymSet.union
                  (scope_exndict cur_scope)
                  (scope_exndict scope)
            , infixdict =
                SymDict.union
                  (scope_infixdict cur_scope)
                  (scope_infixdict scope)
                  (fn (_, _, snd) => snd)
            , tydict =
                SymDict.union
                  (scope_tydict cur_scope)
                  (scope_tydict scope)
                  (fn (_, _, snd) => snd)
            }
        , ctx
        )
      ) ctx

    fun ctx_rec (scope as Scope { valdict, ... }) =
      SymDict.map
        (fn Vfn (matches, E, VE) => Vfn (matches, E, scope)
        | other => other
        )
        valdict
      |> scope_set_valdict scope

    fun map_module f (ctx as {sigdict, functordict, ...} : t) id =
      raise Fail "lol"
      (* lift (fn (scope, ctx) =>
        let
          fun map_module' scope id =
            case id of
              [] => raise Fail "looking for empty id"
            | [name] => f scope
            | mod_name::path =>
                (* look for the outer module *)
                case SymDict.find (scope_moddict scope) mod_name of
                  NONE => NONE
                | SOME mod_scope =>
                    Option.map
                      (fn new_scope =>
                        (* recursively modify the outer module *)
                        SymDict.insert
                          (SymDict.remove (scope_moddict scope) mod_name)
                          mod_name
                          new_scope
                        |> scope_set_moddict scope
                      )
                      (map_module' mod_scope path)
        in
          case (map_module' scope id, ctx) of
            (NONE, []) => raise Fail "could not find module"
          | (SOME mapped_scope, _) => (mapped_scope, ctx)
          | (NONE, scope::rest) =>
              let
                val (inner_scope, rest) =
                  map_module f
                    { scope = scope
                    , outer_scopes = rest
                    , sigdict = sigdict
                    , functordict = functordict
                    } id
              in
                (scope, inner_scope :: rest)
              end
        end
      ) ctx
      *)


    fun drop_last l = List.nth (l, List.length l - 1)

    fun get_module (ctx as {scope, ...} : t) id =
      let
        exception Return of scope
      in
        ( map_module
            (fn scope =>
              raise Return scope
            )
            ctx
            id
        ; raise Fail "shouldn't get here"
        )
        handle Return scope => scope
      end

    fun get_base f orig_ctx id =
      let
        val (xs, x) = snoc id
      in
        SymDict.lookup
          (f (get_module orig_ctx xs))
          x
      end

    val get_val = get_base scope_valdict

    val get_datatype = get_base scope_tydict

    val get_infixdict = get_base scope_infixdict

    fun get_sig ({sigdict, ...} : t) id =
      SymDict.lookup sigdict id

    fun get_functor ({functordict, ...} : t) id =
      SymDict.lookup functordict id

    fun add_exn ctx id =
      lift (fn (scope, rest) =>
        let
          val exndict = scope_exndict scope
        in
          ( scope_set_exndict scope (SymSet.insert exndict id)
          , rest
          )
        end
      ) ctx

    fun add_con ctx id =
      lift (fn (scope, rest) =>
        let
          val condict = scope_condict scope
        in
          ( scope_set_condict scope (SymSet.insert condict id)
          , rest
          )
        end
      ) ctx

    fun add_infix ctx (id, infixity, precedence) =
      lift (fn (scope, rest) =>
        let
          val infixdict = scope_infixdict scope
        in
          ( scope_set_infixdict
              scope
              (SymDict.insert infixdict id (infixity, precedence))
          , rest
          )
        end
      ) ctx

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
                (get_datatype ctx right_tycon)
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


    fun add_module ctx id new_scope =
      lift (fn (scope, rest) =>
        let
          val moddict = scope_moddict scope
        in
          ( scope_set_moddict
              scope
              (SymDict.insert moddict id new_scope)
          , rest
          )
        end
      ) ctx

    fun add_funbind (ctx as {scope, outer_scopes, sigdict, functordict})
                    {id, funarg, seal, body} =
      { scope = scope
      , outer_scopes = outer_scopes
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
      }

    fun remove_infix ctx id =
      lift (fn (scope, rest) =>
        let
          val infixdict = scope_infixdict scope
        in
          ( scope_set_infixdict scope (SymDict.remove infixdict id)
          , rest
          )
        end
      ) ctx


    fun enter_scope ctx =
      lift (fn (scope, ctx) =>
        (scope_empty, scope :: ctx)
      ) ctx

    fun exit_scope name ctx =
      lift (fn (scope, ctx) =>
        case ctx of
          [] => raise Fail "exiting scope without outer scope"
        | outer'::scopes =>
            let
              val moddict = scope_moddict outer'
            in
              (scope_set_moddict outer' (SymDict.insert moddict name scope), scopes)
            end
      ) ctx

    fun pop_scope ({scope, ...} : t) = scope

    fun pop_penultimate ctx =
      lift (fn (scope, rest) =>
        case rest of
          [] => raise Fail "pop penultimate on single contextX"
        | scope'::rest =>
            (scope, rest)
      ) ctx

    fun open_scope ctx new_scope =
      merge_scope ctx new_scope

    fun open_path ctx path =
      merge_scope
        ctx
        (get_module ctx path)

    fun add_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val valdict = scope_valdict scope
        in
          List.foldl
            (fn ((id, value), valdict) =>
              SymDict.insert valdict id value
            )
            valdict
            bindings
          |> scope_set_valdict scope
          |> (fn scope => (scope, rest))
        end
      ) ctx

    fun add_rec_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val valdict = scope_valdict scope

          val rec_valdict =
            List.foldl
              (fn ((id, value), valdict) =>
                SymDict.insert valdict id value
              )
              SymDict.empty
              bindings
            |> scope_set_valdict scope
            |> ctx_rec
            |> scope_valdict
        in
          ( scope_set_valdict scope
              (SymDict.union valdict rec_valdict (fn (_, _, snd) => snd))
          , rest
          )
        end
      ) ctx

    (* TYPE STUFF *)

    fun add_datatype ctx {tyvars, tycon, conbinds} =
      lift (fn (scope, rest) =>
        let
          val condict = scope_condict scope
          val tydict = scope_tydict scope
          val cons =
            List.map
              (fn {id, ty, opp} => {id = id, ty = ty})
              conbinds

          val new_condict =
            List.foldl
              (fn ({id, ty}, condict) =>
                SymSet.insert condict id
              )
              condict
              cons

          val new_tydict =
            SymDict.insert tydict tycon { arity = List.length tyvars
                                        , cons = cons
                                        }
        in
          ( scope_set_tydict
              (scope_set_condict scope new_condict)
              new_tydict
          , rest
          )
        end
      ) ctx

    (* TODO: implement type stuff *)
    fun add_typbinds ctx typbinds = ctx

    fun replicate_datatype orig_ctx (left_id, right_id) =
      lift (fn ctx as (scope, rest) =>
        let
          val tydict = scope_tydict scope
          val condict= scope_condict scope
          val {arity, cons} = get_datatype orig_ctx right_id
          val new_condict =
            List.foldl
              (fn ({id, ty}, condict) =>
                SymSet.insert condict id
              )
              condict
              cons
          val new_tydict =
              SymDict.insert tydict left_id { arity = arity
                                            , cons = cons
                                            }
        in
          ( scope_set_condict
              ( scope_set_tydict
                scope
                new_tydict
              )
              new_condict
          , rest
          )
        end
      ) orig_ctx


    fun replicate_exception ctx (left_id, right_id) =
      lift (fn (scope, rest) =>
        (* TODO: type stuff, and exception generativity *)
        let
          val (xs, x) = snoc right_id
        in
          ( scope_set_exndict
              scope
              (SymSet.insert (scope_exndict scope) x)
          , rest
          )
        end
      ) ctx

    fun ascribe scope seal =
      case seal of
        NONE => scope
      | SOME {opacity, sigval = Sigval { valspecs, dtyspecs, exnspecs, modspecs }} =>
          let
            val valdict = scope_valdict scope
            val tydict = scope_tydict scope
            val exndict = scope_exndict scope
            val moddict = scope_moddict scope

            val new_valdict =
              SymSet.foldl
                (fn (id, acc_valdict) =>
                  SymDict.insert
                    acc_valdict
                    id
                    (SymDict.lookup valdict id)
                )
                SymDict.empty
                valspecs

            (* TODO: vals here too *)
            val (cons, new_tydict) =
              SymDict.foldl
                (fn (id, {arity, cons}, (acc_cons, acc_tydict)) =>
                  let
                    val {arity = arity', cons = cons'} =
                      SymDict.lookup tydict id

                  in
                    (* TODO: type stuff *)
                    if arity' = arity
                       andalso
                        ListPair.allEq
                          Symbol.eq
                          (List.map #id cons, List.map #id cons') then
                      ( List.map #id cons @ acc_cons
                      , SymDict.insert
                          acc_tydict
                          id
                          {arity = arity, cons = cons}
                      )
                    else
                      raise Fail "failure to ascribe datatypes"
                  end
                )
                ([], SymDict.empty)
                dtyspecs

            val new_exndict =
              SymSet.foldl
                (fn (id, acc_exndict) =>
                  if SymSet.member exndict id then
                    SymSet.insert
                      acc_exndict
                      id
                  else
                    raise Fail "failure to ascribe exns"
                )
                SymSet.empty
                exnspecs

            val new_moddict =
              SymDict.foldl
                (fn (id, sigval, acc_moddict) =>
                  SymDict.insert
                    acc_moddict
                    id
                    ( ascribe
                        (SymDict.lookup moddict id)
                        ( SOME
                            { opacity = Transparent
                            , sigval = sigval
                            }
                        )
                    )
                )
                SymDict.empty
                modspecs
          in
            Scope
              { valdict = new_valdict
              , condict =
                  List.foldl
                    (fn (id, acc) =>
                      SymSet.insert acc id
                    )
                    SymSet.empty
                    cons
              , exndict = new_exndict
              , moddict = new_moddict
              , infixdict = SymDict.empty
              , tydict = new_tydict
              }
          end

    (* VALUE STUFF *)

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
      | Vident id => Eident {opp = false, id = id}
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
      | Vfn (matches, E, VE) => Efn matches

    exception Mismatch of string

    fun match_pat pat v =
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
      | (Pident {opp, id}, _) => [(id, v)]
      | (Pconstr {opp, id}, Vconstr {id = id', arg = NONE}) =>
          if longid_eq (id, id') then
            []
          else
            raise Mismatch "constr pats did not match"
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
                  | SOME {lab, value} => match_pat pat value @ acc
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
      | (Punit, Vunit) => []
      | (Ptuple pats, Vtuple vals) =>
          ListPair.zipEq (pats, vals)
          |> concatMap (fn (x, y) => match_pat x y)
      | (Plist pats, Vlist vals) =>
          ListPair.zipEq (pats, vals)
          |> concatMap (fn (x, y) => match_pat x y)
      | (Por pats, _) =>
          ( case
              List.foldl
                (fn (pat, NONE) =>
                  ( SOME (match_pat pat v)
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
            match_pat atpat v
          else
            raise Mismatch "failed to match constructors with args"
      | (Pinfix {left, id, right}, Vinfix {left = left', id = id', right = right'}) =>
          (* TODO: constructors need more than just name equality *)
          if Symbol.eq (id, id') then
            match_pat left left' @ match_pat right right'
          else
            raise Mismatch "idents not equal"
      | (Ptyped {pat, ty}, _) => match_pat pat v
      | (Playered {opp, id, ty, aspat}, _) =>
          (id, v) :: match_pat aspat v
      | _ => raise Fail "does not match pat"


    fun match_against orig_ctx matches value =
      let
        val (bindings, exp) =
          case
            List.foldl
              (fn ({exp, pat}, NONE) =>
                ( SOME (match_pat pat value, exp)
                  handle Mismatch _ => NONE
                )
              | ({exp, pat}, SOME ans) => SOME ans
              )
              NONE
              matches
          of
            NONE => raise Mismatch "failed to match against any"
          | SOME ans => ans
      in
        (add_bindings orig_ctx bindings, exp)
      end

    fun apply_fn (matches, E, VE) value =
      match_against
        (merge_scope E (ctx_rec VE))
        matches
        value

    fun generate_sigbinding ctx {id, signat} =
      (id, evaluate_signat ctx signat)

    fun add_sigbindings {scope, outer_scopes, sigdict, functordict} sigbindings =
      { scope = scope
      , outer_scopes = outer_scopes
      , sigdict =
            ( List.foldl
                (fn ((id, sigval), sigdict) =>
                  SymDict.insert sigdict id sigval
                )
                sigdict
                sigbindings
            )
      , functordict = functordict
      }
  end
