
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
    | Vfn of
        { matches : { pat : SMLSyntax.pat, exp : SMLSyntax.exp } list
        , env : t
        , rec_env : scope option
        , break : symbol option ref
        }
    | Vbasis of value -> value

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

    exception Raise of value

    (* Context stuff. *)

    val scope_empty : scope

    val get_val : t -> SMLSyntax.longid -> value
    val get_val_opt : t -> SMLSyntax.longid -> value option
    val get_module : t -> SMLSyntax.longid -> scope
    val get_sig : t -> SMLSyntax.symbol -> sigval
    val get_functor : t -> SMLSyntax.symbol -> functorval

    val is_con : t -> SMLSyntax.longid -> bool
    val is_exn : t -> SMLSyntax.longid -> bool

    val add_exn : t -> SMLSyntax.symbol -> t
    val add_con : t -> SMLSyntax.symbol -> t
    val add_infix : t -> (SMLSyntax.symbol * infixity * int) -> t
    val add_module : t -> SMLSyntax.symbol -> scope -> t

    val add_funbind : t -> SMLSyntax.funbind -> t
    val add_hole_print_fn : t -> (unit -> PrettySimpleDoc.t) -> t
    val get_hole_print_fn : t -> (unit -> PrettySimpleDoc.t)
    val get_break_assigns : t -> SymSet.set ref
    val set_substitute : t -> bool -> unit
    val is_substitute : t -> bool


    val break_fn : t -> SMLSyntax.longid -> bool -> t * symbol option ref
    val remove_infix : t -> SMLSyntax.symbol -> t

    val enter_scope : t -> t
    val exit_scope : SMLSyntax.symbol -> t -> t
    val pop_scope : t -> scope
    val pop_penultimate : t -> t
    val exit_local : t -> t

    val open_scope : t -> scope -> t
    val open_path : t -> SMLSyntax.symbol list -> t

    val add_bindings : t -> (SMLSyntax.symbol * value) list -> t
    val add_rec_bindings : t -> (SMLSyntax.symbol * value) list -> t

    val initial : t

    (* Value stuff. *)

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

    val get_pat_bindings : t -> SMLSyntax.pat -> PrettyPrintContext.MarkerSet.set
    val get_pat_ids : t -> SMLSyntax.pat -> SMLSyntax.symbol list
    val get_fname_args_bindings :
      t -> SMLSyntax.fname_args -> PrettyPrintContext.MarkerSet.set
    val remove_bound_ids : t -> PrettyPrintContext.MarkerSet.set -> t
    val open_bound_ids : t -> (SMLSyntax.symbol list) list -> PrettyPrintContext.MarkerSet.set
    val get_funarg_bound_ids : t -> SMLSyntax.funarg -> PrettyPrintContext.MarkerSet.set

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
    open PrettyPrintContext
    open SMLSyntax

    val sym_true = Symbol.fromValue "true"
    val sym_false = Symbol.fromValue "false"

    val empty_set = MarkerSet.empty

    fun union_sets l =
      List.foldl
        (fn (set, acc) =>
          MarkerSet.union set acc
        )
        MarkerSet.empty
        l

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
    | Vbasis of value -> value

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
      , hole_print_fn : unit -> PrettySimpleDoc.t
      , settings :
          { break_assigns : SymSet.set ref
          , substitute : bool ref
          }
      }

    exception Raise of value

    fun lift f {scope, outer_scopes, sigdict, functordict, hole_print_fn,
    settings } =
      let
        val (scope, outer_scopes) = f (scope, outer_scopes)
      in
        { scope = scope
        , outer_scopes = outer_scopes
        , sigdict = sigdict
        , functordict = functordict
        , hole_print_fn = hole_print_fn
        , settings = settings
        }
      end

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
        (fn Vfn {matches, env, rec_env, break} =>
              Vfn { matches = matches
                  , env = env
                  , rec_env = SOME scope
                  , break = break
                  }
        | other => other
        )
        valdict
      |> scope_set_valdict scope

    exception CouldNotFind

    fun map_scope_id f (ctx as {scope, outer_scopes, sigdict, functordict,
    hole_print_fn, settings} : t) id =
      let
        fun map_thing' scopes =
          case scopes of
            [] => raise CouldNotFind
          | scope::rest =>
            ( case f scope of
                NONE => scope :: map_thing' rest
              | SOME ans => ans :: rest
            )
        val (scope, rest) =
          case map_thing' (scope::outer_scopes) of
            [] => raise Fail "impossible"
          | scope::rest => (scope, rest)
      in
        { scope = scope
        , outer_scopes = rest
        , sigdict = sigdict
        , functordict = functordict
        , hole_print_fn = hole_print_fn
        , settings = settings
        }
      end

    (* map_module f [] should map the current scope
     * map_module f (x::xs)
     *)
    fun map_module f (ctx as {sigdict, functordict, hole_print_fn,
    settings, ...} : t) id =
      case id of
        [] => raise Fail "map module on an empty path is a bad idea, because it can pick up extra scopes"
      | _ =>
        lift (fn (scope, ctx) =>
          let
            fun map_module' scope id =
              case id of
                [] => SOME (f scope)
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
              (NONE, []) => raise CouldNotFind
            | (SOME mapped_scope, _) => (mapped_scope, ctx)
            | (NONE, scope::rest) =>
                let
                  val { scope = inner_scope, outer_scopes = rest, ...} =
                    map_module f
                      { scope = scope
                      , outer_scopes = rest
                      , sigdict = sigdict
                      , functordict = functordict
                      , hole_print_fn = hole_print_fn
                      , settings = settings
                      } id
                in
                  (scope, inner_scope :: rest)
                end
          end
        ) ctx


    fun drop_last l = List.nth (l, List.length l - 1)

    fun get_module (ctx as {scope, ...} : t) id =
      case id of
        [] => raise Fail "getting the empty path is a bad idea"
      | _ =>
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
        handle SymDict.Absent => raise CouldNotFind
      end

    fun search_id f ({scope, outer_scopes, ...} : t) id =
      let
        fun search_id' scope =
          SOME
            ( SymDict.lookup
                (f scope)
                id
            )
          handle SymDict.Absent => NONE

        fun search_iter scopes =
          case scopes of
            [] => raise Fail "CouldNotFind"
          | scope::rest =>
              (case search_id' scope of
                NONE => search_iter rest
              | SOME ans => ans
              )
      in
        search_iter (scope::outer_scopes)
      end

    fun ctx_scopes ({scope, outer_scopes, ...} : t) =
      scope :: outer_scopes

    fun iter_scopes f scopes =
      case scopes of
        [] => raise CouldNotFind
      | scope::rest =>
          (case f scope of
            NONE => iter_scopes f rest
          | SOME ans => ans
          )

    fun is_con ctx id =
      let
        val (xs, x) = snoc id
      in
        case xs of
          [] =>
            ( iter_scopes
                (fn scope => if SymSet.member (scope_condict scope) x then SOME true
                             else NONE)
                (ctx_scopes ctx)
              handle CouldNotFind => false
            )
        | _ => SymSet.member (scope_condict (get_module ctx xs)) x
      end

    fun is_exn ctx id =
      let
        val (xs, x) = snoc id
      in
        case xs of
          [] =>
            ( iter_scopes
                (fn scope => if SymSet.member (scope_exndict scope) x then SOME true
                             else NONE)
                (ctx_scopes ctx)
              handle CouldNotFind => false
            )
        | _ => SymSet.member (scope_exndict (get_module ctx xs)) x
      end

    fun get_val ctx id =
      case id of
        [x] =>
          iter_scopes
            (fn scope => (SymDict.find (scope_valdict scope) x))
            (ctx_scopes ctx)
      | _ => get_base scope_valdict ctx id

    fun get_val_opt ctx id =
      SOME (get_val ctx id) handle CouldNotFind => NONE

    fun get_datatype ctx id =
      case id of
        [x] =>
          iter_scopes
            (fn scope => SymDict.find (scope_tydict scope) x)
            (ctx_scopes ctx)
      | _ => get_base scope_tydict ctx id

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

    fun add_funbind (ctx as {scope, outer_scopes, sigdict, functordict,
    hole_print_fn, settings})
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
      , hole_print_fn = hole_print_fn
      , settings = settings
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

    fun exit_local ctx =
      let
        val new_ctx = pop_penultimate ctx
        val scope = pop_scope new_ctx
      in
        merge_scope new_ctx scope
      end

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

    fun add_hole_print_fn
        {scope, outer_scopes, sigdict, functordict, hole_print_fn, settings} f =
      { scope = scope
      , outer_scopes = outer_scopes
      , sigdict = sigdict
      , functordict = functordict
      , hole_print_fn = f
      , settings = settings
      }

    fun get_hole_print_fn ({hole_print_fn, ...} : t) = hole_print_fn

    fun get_break_assigns ({settings = {break_assigns, ...}, ...} : t) = break_assigns

    fun set_substitute ({settings = {substitute, ...}, ...} : t) b =
      substitute := b

    fun is_substitute ({settings = {substitute, ...}, ...} : t) =
      !substitute

    fun break_fn ctx id do_break =
      let
        val res = ref NONE
        val (xs, x) = snoc id
        val setting =
          if do_break then SOME x
          else NONE

        fun new_vfn {matches, env, rec_env, break} f =
          (case (break, do_break) of
            (ref (SOME _), true) =>
              raise Fail "breaking already broken function"
          | (ref NONE, false) =>
              raise Fail "clearing a not broken function"
          | _ =>
              Vfn { matches = matches
                  , env = env
                  , rec_env = rec_env
                  , break = (res := SOME break; break := setting; break)
                  }
              |> f
          )
      in
        ( case id of
          [x] =>
            map_scope_id (fn scope =>
              case SymDict.find (scope_valdict scope) x of
                NONE => NONE
              | SOME (Vfn info) =>
                  new_vfn info (fn vfn =>
                    SymDict.insert (scope_valdict scope) x vfn
                    |> scope_set_valdict scope
                    |> SOME
                  )
              | _ => raise Fail "breaking a non function"
            ) ctx x
        | _ =>
          case get_val_opt ctx id of
            NONE => raise Fail "breaking nonexistent function"
          | SOME (Vfn info) =>
              new_vfn info (fn vfn =>
                map_module
                  (fn scope =>
                    SymDict.insert (scope_valdict scope) x vfn
                    |> scope_set_valdict scope
                  )
                  ctx
                  xs
              )
          | _ => raise Fail "breaking a non function"
        , case !res of
            NONE => raise Fail "shouldn't be possible"
          | SOME ans => ans
        )
      end


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
      | Vfn {matches, env, rec_env, ...} => Efn matches
      (* For basis values *)
      | Vbasis f => raise Fail "TODO"

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
          if is_con ctx id then
            SOME (Vconstr {id = id, arg = NONE})
          else
            SOME (get_val ctx id)
      | Efn matches =>
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
          if is_con ctx id then
            Option.map
              (fn exp => Vconstr {id = id, arg = SOME exp})
              (exp_to_value ctx right)
          else
            NONE
      | Einfix {left, id, right} =>
          if is_con ctx [id] then
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
          if is_con ctx id then
            if longid_eq (id, id') then
              []
            else
              raise Mismatch "constructors did not match"
          else
            (case id of
              [id] => [(id, v)]
            | _ => raise Fail "nonexistent con"
            )
      | (Pident {opp, id}, _) =>
          if is_con ctx id then
            raise Mismatch "cannot match nullary constructor"
          else
            (case id of
              [id] => [(id, v)]
            | _ => raise Fail "nonexistent con"
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
          |> concatMap (fn (x, y) => match_pat ctx x y)
      | (Plist pats, Vlist vals) =>
          ListPair.zipEq (pats, vals)
          |> concatMap (fn (x, y) => match_pat ctx x y)
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
      | _ => raise Fail "does not match pat"

    fun marker_set_of_list l =
      List.foldl
        (fn (x, acc) =>
          MarkerSet.insert acc x
        )
        MarkerSet.empty
        l

    fun get_patrow_bindings ctx patrow =
      case patrow of
        PRellipsis => MarkerSet.empty
      | PRlab {pat, ...} => get_pat_bindings ctx pat
      | PRas {id, aspat, ...} =>
          MarkerSet.union
            (MarkerSet.singleton (VAL id))
            (case aspat of
              NONE => MarkerSet.empty
            | SOME pat => get_pat_bindings ctx pat
            )

    and get_pat_bindings ctx pat =
      case pat of
        ( Pnumber _
        | Pword _
        | Pstring _
        | Pchar _
        | Pwild
        | Punit ) => MarkerSet.empty
      | Pident {id, ...} =>
          if is_con ctx id then
            MarkerSet.empty
          else
            (case id of
              [id] => MarkerSet.singleton (VAL id)
            | _ => raise Fail "shouldn't be possible, constructor not found"
            )
      | Precord patrows => List.map (get_patrow_bindings ctx) patrows |> union_sets
      | Pparens pat => get_pat_bindings ctx pat
      | Ptuple pats => List.map (get_pat_bindings ctx) pats |> union_sets
      | Plist pats => List.map (get_pat_bindings ctx) pats |> union_sets
      | Por pats => List.map (get_pat_bindings ctx) pats |> union_sets
      | Papp {id, atpat, ...} =>
          if is_con ctx id then
            get_pat_bindings ctx atpat
          else
            (case id of
              [id] =>
                MarkerSet.union
                  (MarkerSet.singleton (VAL id))
                  (get_pat_bindings ctx atpat)
            | _ => raise Fail "shouldn't happen, constructor not found"
            )
      | Pinfix {left, id, right} =>
          if is_con ctx [id] then
            MarkerSet.union
              (get_pat_bindings ctx left)
              (get_pat_bindings ctx right)
          else
            union_sets
              [ MarkerSet.singleton (VAL id)
              , get_pat_bindings ctx left
              , get_pat_bindings ctx right
              ]
      | Ptyped {pat, ...} => get_pat_bindings ctx pat
      | Playered {id, aspat, ...} =>
          MarkerSet.union
            (MarkerSet.singleton (VAL id))
            (get_pat_bindings ctx aspat)

    fun get_pat_ids ctx pat =
      List.map (fn VAL id => id | _ => raise Fail "shouldn't happen")
      (MarkerSet.toList (get_pat_bindings ctx pat))

    fun get_fname_args_bindings ctx fname_args =
      case fname_args of
        Fprefix {id, args, ...} =>
          union_sets
            ( [ MarkerSet.singleton (VAL id) ]
              @ List.map (get_pat_bindings ctx) args
            )
      | Finfix {left, id, right} =>
          union_sets
            [ get_pat_bindings ctx left
            , MarkerSet.singleton (VAL id)
            , get_pat_bindings ctx right
            ]
      | Fcurried_infix {left, id, right, args} =>
          union_sets
            ( [ get_pat_bindings ctx left
              , MarkerSet.singleton (VAL id)
              , get_pat_bindings ctx right
              ]
              @ List.map (get_pat_bindings ctx) args
            )

    fun remove_val_scope_bound_id scope id =
      let
        val valdict = scope_valdict scope
      in
        scope_set_valdict scope (SymDict.remove valdict id)
      end

    fun remove_mod_scope_bound_id scope id =
      let
        val moddict = scope_moddict scope
      in
        scope_set_moddict scope (SymDict.remove moddict id)
      end

    fun remove_bound_id_base f ({scope, outer_scopes, sigdict, functordict,
    hole_print_fn, settings} : t) id =
      let
        val scope = f scope id
        val outer_scopes =
          List.map
            (fn scope => f scope id)
            outer_scopes
      in
        { scope = scope
        , outer_scopes = outer_scopes
        , sigdict = sigdict
        , functordict = functordict
        , hole_print_fn = hole_print_fn
        , settings = settings
        }
      end

    fun remove_bound_ids ctx ids =
      MarkerSet.foldl
        (fn (VAL id, ctx) =>
          remove_bound_id_base remove_val_scope_bound_id ctx id
        | (MOD id, ctx) =>
          remove_bound_id_base remove_mod_scope_bound_id ctx id
        )
        ctx
        ids

    fun get_sigval_bound_ids ctx (Sigval {valspecs, modspecs, ...})=
      marker_set_of_list
        ( List.map VAL (SymSet.toList valspecs)
        @ List.map MOD (SymDict.domain modspecs)
        )

    and get_signat_bound_ids ctx signat =
      case signat of
        Sspec spec => get_spec_bound_ids ctx spec
      | Sident sym =>
          get_sigval_bound_ids ctx (get_sig ctx sym)
      | Swhere {signat, ...} => get_signat_bound_ids ctx signat

    and get_spec_bound_ids ctx spec =
      case spec of
        SPval valdescs =>
          List.map (VAL o #id) valdescs |> marker_set_of_list
      | ( SPtype _
        | SPeqtype _
        | SPdatdec _
        | SPdatrepl _
        | SPexception _ ) => MarkerSet.empty
      | SPmodule moddescs =>
          List.map (MOD o #id) moddescs |> marker_set_of_list
      | SPinclude signat =>
          get_signat_bound_ids ctx signat
      | SPinclude_ids syms =>
          List.map (get_signat_bound_ids ctx) (List.map Sident syms)
          |> union_sets
      | SPsharing_type {spec, ...} => get_spec_bound_ids ctx spec
      | SPsharing {spec, ...} => get_spec_bound_ids ctx spec
      | SPseq specs =>
          List.map (get_spec_bound_ids ctx) specs
          |> union_sets

    fun get_funarg_bound_ids ctx funarg =
      case funarg of
        Normal {id, ...} => MarkerSet.singleton (MOD id)
      | Sugar spec => get_spec_bound_ids ctx spec

    fun open_bound_id ctx longid =
      let
        val valdict = scope_valdict (get_module ctx longid)
        val moddict = scope_moddict (get_module ctx longid)
      in
        marker_set_of_list
          (List.map VAL (SymDict.domain valdict)
          @
          List.map MOD (SymDict.domain moddict)
          )
      end

    fun open_bound_ids ctx longids =
      union_sets
        (List.map (open_bound_id ctx) longids)

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
            NONE => raise Mismatch "failed to match against any"
          | SOME ans => ans
      in
        (bindings, add_bindings ctx bindings, exp)
      end

    fun apply_fn (matches, E, VE) value =
      match_against
        (merge_scope E (ctx_rec (Option.getOpt (VE, scope_empty))))
        matches
        value

    fun generate_sigbinding ctx {id, signat} =
      (id, evaluate_signat ctx signat)

    fun add_sigbindings {scope, outer_scopes, sigdict, functordict,
    hole_print_fn, settings} sigbindings =
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
      , hole_print_fn = hole_print_fn
      , settings = settings
      }

    val sym = Symbol.fromValue



    (* TODO: word stuff *)
    val initial_values =
      [ ( "+"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 + i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 + r2))
          | _ => raise Fail "invalid args to +"
          )
        )
      , ( "-"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 - i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 - r2))
          | _ => raise Fail "invalid args to -"
          )
        )
      , ( "*"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 * i2))
          | Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 * r2))
          | _ => raise Fail "invalid args to *"
          )
        )
      , ( "div"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 div i2))
          | _ => raise Fail "invalid args to div"
          )
        )
      , ( "mod"
        , (fn Vtuple [Vnumber (Int i1), Vnumber (Int i2)] => Vnumber (Int (i1 mod i2))
          | _ => raise Fail "invalid args to div"
          )
        )
      , ( "/"
        , (fn Vtuple [Vnumber (Real r1), Vnumber (Real r2)] => Vnumber (Real (r1 / r2))
          | _ => raise Fail "invalid args to /"
          )
        )
      , ( "not"
        , (fn Vconstr {id = [x], arg = NONE} =>
            if Symbol.eq (x, sym_true) then
              Vconstr {id = [sym_false], arg = NONE}
            else if Symbol.eq (x, sym_false) then
              Vconstr {id = [sym_true], arg = NONE}
            else
              raise Fail "invalid arg to `not`"
          | _ => raise Fail "invalid arg to `not`"
          )
        )
      , ( "^"
        , (fn Vtuple [Vstring s1, Vstring s2] =>
              Vstring (Symbol.fromValue (Symbol.toValue s1 ^ Symbol.toValue s2))
          | _ => raise Fail "invalid args to ^"
          )
        )
      , ( "chr"
        , (fn Vnumber (Int i) => Vchar (Char.chr i)
          | _ => raise Fail "invalid args to `chr`"
          )
        )
      , ( "explode"
        , (fn Vstring s => Vlist (List.map Vchar (String.explode (Symbol.toValue s)))
          | _ => raise Fail "invalid args to `explode`"
          )
        )
      , ( "floor"
        , (fn Vnumber (Real r) => Vnumber (Int (Real.floor r))
          | _ => raise Fail "invalid args to `floor`"
          )
        )
      , ( "ord"
        , (fn Vchar c => Vnumber (Int (Char.ord c))
          | _ => raise Fail "invalid arg to `ord`"
          )
        )
      , ( "real"
        , (fn Vnumber (Int i) => Vnumber (Real (real i))
          | _ => raise Fail "invalid arg to `real`"
          )
        )
      , ( "size"
        , (fn Vstring s => Vnumber (Int (String.size (Symbol.toValue s)))
          | _ => raise Fail "invalid arg to `size`"
          )
        )
      , ( "str"
        , (fn Vchar c => Vstring (Symbol.fromValue (str c))
          | _ => raise Fail "invalid arg to `str`"
          )
        )
      , ( "round"
        , (fn Vnumber (Real r) => Vnumber (Int (round r))
          | _ => raise Fail "invalid arg to `round`"
          )
        )
      , ( "substring"
        , (fn Vtuple [Vstring s, Vnumber (Int i1), Vnumber (Int i2)] =>
            ( Vstring
              (Symbol.fromValue (String.substring (Symbol.toValue s, i1, i2))) handle Subscript =>
                raise Raise (Vconstr {id = [Symbol.fromValue "Subscript"], arg = NONE})
            )
          | _ => raise Fail "invalid args to `substring`"
          )
        )
      , ( "~"
        , (fn Vnumber (Int i) => Vnumber (Int (~i))
          | Vnumber (Real i) => Vnumber (Real (~i))
          | _ => raise Fail "invalid arg to `~`"
          )
        )
      ]
      |> List.map (fn (x, y) => (sym x, Vbasis y))

    val initial_cons =
      [ "SOME", "NONE", "true", "false", "::", "LESS", "EQUAL", "GREATER",
         "nil", "Match", "Bind", "Div", "Fail"]
      |> List.map sym

    val initial_exns =
      [ "Fail", "Bind", "Match", "Div", "Subscript" ]
      |> List.map sym

    val initial_mods = []

    val initial_infix =
      [ ( "div", (LEFT, 7) )
      , ( "mod", (LEFT, 7) )
      , ( "*", (LEFT, 7) )
      , ( "/", (LEFT, 7) )

      , ( "+", (LEFT, 6) )
      , ( "-", (LEFT, 6) )
      , ( "^", (LEFT, 6) )

      , ( "::", (RIGHT, 5) )
      , ( "@", (RIGHT, 5) )

      , ( "<>", (LEFT, 4) )
      , ( "=", (LEFT, 4) )
      , ( "<", (LEFT, 4) )
      , ( ">", (LEFT, 4) )
      , ( "<=", (LEFT, 4) )
      , ( ">=", (LEFT, 4) )

      , ( ":=", (LEFT, 3) )
      , ( "o", (LEFT, 3) )

      , ( "before", (LEFT, 0) )
      ]
      |> List.map (fn (x, y) => (sym x, y))

    val initial_tys =
      [ ( "order"
        , ( 0
          , [ ("LESS", NONE)
            , ("GREATER", NONE)
            , ("EQUAL", NONE)
            ]
          )
        )
      , ( "option"
        , ( 1
          , [ ("SOME", SOME (Ttyvar (sym "'a")))
            , ("NONE", NONE)
            ]
          )
        )
      , ( "list"
        , ( 1
          , [ ("::", SOME ( Tprod [ Ttyvar (sym "'a")
                                  , Tapp ([Ttyvar (sym "'a")], [sym "list"])
                                  ]
                          )
              )
            , ("nil", NONE)
            ]
          )
        )
      , ( "unit"
        , ( 0
          , [ ("()", NONE) ]
          )
        )
        (* TODO: int, real, types with weird constructors? *)
      ]
      |> List.map
           (fn (tycon, (arity, cons)) =>
             (sym tycon
             , { arity = arity
               , cons = List.map (fn (x, y) => { id = sym x, ty = y }) cons
               }
             )
           )

    fun sym_from_list l =
      List.foldl
        (fn (x, acc) =>
          SymSet.insert acc x
        )
        SymSet.empty
        l

    val initial_scope =
      Scope
        { valdict = dict_from_list initial_values
        , condict = sym_from_list initial_cons
        , exndict = sym_from_list initial_exns
        , moddict = dict_from_list initial_mods
        , infixdict = dict_from_list initial_infix
        , tydict = dict_from_list initial_tys
        }


    val initial =
      { scope = initial_scope
      , outer_scopes = []
      , sigdict = SymDict.empty
      , functordict = SymDict.empty
      , hole_print_fn = fn () => PrettySimpleDoc.text TerminalColors.white "<hole>"
      , settings =
          { break_assigns = ref SymSet.empty
          , substitute = ref true
          }
      }
  end
