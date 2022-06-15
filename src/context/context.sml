
structure Context :
  sig

    (* The outer scopes are a list, in order of ascending outwardness.
     * Each scope maps to the stuff in it, and all things in scope are within
     * the current value of outer_scopes.
     *)

    (* Context stuff. *)

    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type valdict = SMLSyntax.valdict
    type t = SMLSyntax.context

    exception Raise of SMLSyntax.value

    val scope_empty : scope
    val merge_scope : t -> scope -> t
    val ctx_rec : scope -> scope

    val scope_valdict : scope -> valdict
    val scope_moddict : scope -> scope SymDict.dict

    val scope_set_valdict : scope -> valdict -> scope
    val scope_set_moddict : scope -> scope SymDict.dict -> scope

    val get_val : t -> SMLSyntax.longid -> value
    val get_val_opt : t -> SMLSyntax.longid -> value option
    val get_sigval_opt : t -> SMLSyntax.symbol -> SMLSyntax.sigval option
    val get_functorval_opt : t -> SMLSyntax.symbol -> SMLSyntax.functorval option
    val get_module : t -> SMLSyntax.longid -> scope
    val get_sig : t -> SMLSyntax.symbol -> SMLSyntax.sigval
    val get_functor : t -> SMLSyntax.symbol -> SMLSyntax.functorval
    val get_datatype : t -> SMLSyntax.longid -> SMLSyntax.tyval
    val get_infixity : t -> SMLSyntax.symbol -> SMLSyntax.infixity * int

    val is_con : t -> SMLSyntax.longid -> bool
    val is_exn : t -> SMLSyntax.longid -> bool

    val add_exn : t -> SMLSyntax.symbol -> t
    val add_con : t -> SMLSyntax.symbol -> t
    val add_infix : t -> (SMLSyntax.symbol * SMLSyntax.infixity * int) -> t
    val add_module : t -> SMLSyntax.symbol -> scope -> t
    val add_sig : t -> SMLSyntax.symbol -> SMLSyntax.sigval -> t
    val add_functor : t -> SMLSyntax.symbol -> SMLSyntax.functorval -> t

    val cm_export :
      string -> t -> t ->
                { structs : SMLSyntax.symbol list
                , sigs : SMLSyntax.symbol list
                , functors : SMLSyntax.symbol list
                } -> t

    val add_hole_print_fn : t -> (unit -> PrettySimpleDoc.t) -> t
    val get_hole_print_fn : t -> (unit -> PrettySimpleDoc.t)
    val get_break_assigns : t -> SymSet.set ref
    val set_substitute : t -> bool -> unit
    val is_substitute : t -> bool

    val get_settings : t -> SMLSyntax.settings
    val get_print_depth : t -> int

    val break_fn : t -> SMLSyntax.longid -> bool -> t * SMLSyntax.symbol option ref
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

    (* Type stuff. *)

    val add_typbinds : t -> SMLSyntax.typbind list -> t
    val replicate_datatype : t -> SMLSyntax.symbol * SMLSyntax.symbol list -> t
    val replicate_exception : t -> SMLSyntax.symbol * SMLSyntax.symbol list -> t

    val add_datatype : t -> SMLSyntax.datbind -> t

    val ascribe : scope -> { opacity : SMLSyntax.opacity
                           , sigval : SMLSyntax.sigval
                           } option -> scope

    (* Value stuff *)
  end =
  struct
    open PrettyPrintContext
    open SMLSyntax
    open Error

    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type valdict = SMLSyntax.valdict
    type t = SMLSyntax.context

    fun lightblue s = TerminalColors.text TerminalColors.lightblue s

    val sym_true = Symbol.fromValue "true"
    val sym_false = Symbol.fromValue "false"

    val empty_set = MarkerSet.empty

    (* Helpers *)

    infix |>
    fun x |> f = f x

    fun concatMap f = List.concat o List.map f

    fun snoc l =
      ( List.take (l, List.length l - 1)
      , List.nth (l, List.length l - 1)
      )

    (* Some types *)

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

    exception Raise of value

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

    (* For looking backwards through all contexts, stopping on the first scope
     * that returns SOME.
     *)
    fun map_scope_id f (ctx as {scope, outer_scopes, sigdict, functordict,
    hole_print_fn, settings}) id =
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

    (* For mapping the first module fitting the identifier, stopping on the
     * first instance found.
     *)
    fun map_module f (ctx as {sigdict, functordict, hole_print_fn,
    settings, ...} : SMLSyntax.context) id =
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

    fun get_module (ctx as {scope, ...} : SMLSyntax.context) id =
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

    fun ctx_scopes ({scope, outer_scopes, ...} : SMLSyntax.context) =
      scope :: outer_scopes

    (* Search through all scopes for a scope which returns SOME, stopping on the
     * first instance found.
     *)
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
        ( case xs of
            [] =>
              iter_scopes
                (fn scope => if SymSet.member (scope_condict scope) x then SOME true
                           else NONE)
                (ctx_scopes ctx)
          | _ =>
              SymSet.member (scope_condict (get_module ctx xs)) x
        )
        handle CouldNotFind => false
            (* NOTE: this allows looking in nonexistent modules *)
      end

    fun is_exn ctx id =
      let
        val (xs, x) = snoc id
      in
        ( case xs of
            [] =>
              ( iter_scopes
                  (fn scope => if SymSet.member (scope_exndict scope) x then SOME true
                               else NONE)
                  (ctx_scopes ctx)
              )
          | _ =>
              SymSet.member (scope_exndict (get_module ctx xs)) x
        ) handle CouldNotFind => false
      end

    fun open_path ctx path =
      merge_scope
        ctx
        (get_module ctx path)
      handle CouldNotFind =>
        prog_err
          ("Could not open nonexistent module "
          ^ TerminalColors.text TerminalColors.lightblue (longid_to_str path)
          )

    fun get_val ctx id =
      ( case id of
          [x] =>
            iter_scopes
              (fn scope => (SymDict.find (scope_valdict scope) x))
              (ctx_scopes ctx)
        | _ => get_base scope_valdict ctx id
      ) handle CouldNotFind =>
          prog_err ("Nonexistent value binding to identifier " ^ lightblue (longid_to_str id))

    fun get_datatype ctx id =
      ( case id of
          [x] =>
            iter_scopes
              (fn scope => SymDict.find (scope_tydict scope) x)
              (ctx_scopes ctx)
        | _ => get_base scope_tydict ctx id
      ) handle CouldNotFind =>
          prog_err ("Nonexistent datatype " ^ lightblue (longid_to_str id))

    fun get_infixity ctx id =
      iter_scopes
        (fn scope => SymDict.find (scope_infixdict scope) id)
        (ctx_scopes ctx)
      handle CouldNotFind =>
        prog_err ("Nonexistent infix identifier " ^ lightblue (Symbol.toValue id))

    fun get_val_opt ctx id =
      SOME (get_val ctx id) handle Signal (SigError (InvalidProgramError _)) => NONE

    fun get_sigval_opt ({sigdict, ...} : SMLSyntax.context) id =
      SymDict.find sigdict id

    fun get_functorval_opt ({functordict, ...} : SMLSyntax.context) id =
      SymDict.find functordict id

    fun get_sig ({sigdict, ...} : SMLSyntax.context) id =
      SymDict.lookup sigdict id
      handle SymDict.Absent =>
        prog_err ("Nonexistent signature " ^ lightblue (Symbol.toValue id))

    fun get_functor ({functordict, ...} : SMLSyntax.context) id =
      SymDict.lookup functordict id
      handle SymDict.Absent =>
        prog_err ("Nonexistent functor " ^ lightblue (Symbol.toValue id))

    fun add_exn ctx id =
      lift (fn (scope, rest) =>
        let
          val exndict = scope_exndict scope
          val condict = scope_condict scope
        in
          ( scope_set_condict (scope_set_exndict scope (SymSet.insert exndict
              id)) (SymSet.insert condict id)
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

    fun add_sig {scope, outer_scopes, sigdict, functordict, hole_print_fn,
    settings} id sigval =
      { scope = scope
      , outer_scopes = outer_scopes
      , sigdict = SymDict.insert sigdict id sigval
      , functordict = functordict
      , hole_print_fn = hole_print_fn
      , settings = settings
      }

    fun add_functor {scope, outer_scopes, sigdict, functordict, hole_print_fn,
    settings} id functorval =
      { scope = scope
      , outer_scopes = outer_scopes
      , sigdict = sigdict
      , functordict = SymDict.insert functordict id functorval
      , hole_print_fn = hole_print_fn
      , settings = settings
      }

    fun cm_export path ctx ctx' {structs, sigs, functors} =
      let
        fun orange s = TerminalColors.text TerminalColors.orange s
        val modules =
          List.map
            (fn s =>
              (s, get_module ctx' [s])
              handle CouldNotFind =>
                err
                  (GeneralError
                    { filename = path
                    , reason =
                          "Failed to find module " ^ (orange (Symbol.toValue s))
                        ^ " to export in " ^ (lightblue path)
                    }
                  )
            )
            structs

        val sigs =
          List.map
            (fn s =>
              case get_sigval_opt ctx' s of
                NONE =>
                  err
                    (GeneralError
                      { filename = path
                      , reason =
                            "Failed to find signature " ^ (orange (Symbol.toValue s))
                          ^ " to export in " ^ (lightblue path)
                      }
                    )
              | SOME ans => (s, ans)
            )
            sigs

        val functors =
          List.map
            (fn s =>
              case get_functorval_opt ctx' s of
                NONE =>
                  err
                    (GeneralError
                      { filename = path
                      , reason =
                            "Failed to find functor " ^ (orange (Symbol.toValue s))
                          ^ " to export in " ^ (lightblue path)
                      }
                    )
              | SOME ans => (s, ans)
            )
            functors

        fun add_things f ctx l =
          List.foldl
            (fn ((id, elem), ctx) =>
              f ctx id elem
            )
            ctx
            l

        val add_modules = fn ctx => add_things add_module ctx modules
        val add_sigs = fn ctx => add_things add_sig ctx sigs
        val add_functors = fn ctx => add_things add_functor ctx functors
      in
        ctx
        |> add_modules
        |> add_sigs
        |> add_functors
      end

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

    fun pop_scope ({scope, ...} : SMLSyntax.context) = scope

    fun pop_penultimate ctx =
      lift (fn (scope, rest) =>
        case rest of
          [] => raise Fail "pop penultimate on single context"
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

    fun get_hole_print_fn ({hole_print_fn, ...} : SMLSyntax.context) = hole_print_fn

    fun get_break_assigns ({settings = {break_assigns, ...}, ...} : SMLSyntax.context) = break_assigns

    fun set_substitute ({settings = {substitute, ...}, ...} : SMLSyntax.context) b =
      substitute := b

    fun is_substitute ({settings = {substitute, ...}, ...} : SMLSyntax.context) =
      !substitute

    fun get_settings ({settings, ...} : SMLSyntax.context) = settings
    fun get_print_depth ({settings = {print_depth, ...}, ...} :
      SMLSyntax.context) = !print_depth

    fun break_fn ctx id do_break =
      let
        val name = lightblue (longid_to_str id)
        val res = ref NONE
        val (xs, x) = snoc id
        val setting =
          if do_break then SOME x
          else NONE

        fun new_vfn {matches, env, rec_env, break} f =
          (case (break, do_break) of
            (ref (SOME _), true) =>
              user_err ("Breaking already broken function " ^ name)
          | (ref NONE, false) =>
              user_err ("Clearing unbroken function " ^ name)
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
            ( map_scope_id (fn scope =>
                case SymDict.find (scope_valdict scope) x of
                  NONE => NONE
                | SOME (Vfn info) =>
                    new_vfn info (fn vfn =>
                      SymDict.insert (scope_valdict scope) x vfn
                      |> scope_set_valdict scope
                      |> SOME
                    )
                | _ =>
                    user_err ("Breaking/clearing non-function identifier " ^ name)
              ) ctx x
              handle CouldNotFind =>
                user_err ("Trying to break/clear nonexistent function " ^ name)
            )
        | _ =>
          case get_val_opt ctx id of
            NONE => user_err ("Breaking/clearing nonexistent function " ^ name )
          | SOME (Vfn info) =>
              new_vfn info (fn vfn =>
                ( map_module
                    (fn scope =>
                      SymDict.insert (scope_valdict scope) x vfn
                      |> scope_set_valdict scope
                    )
                    ctx
                    xs
                  handle CouldNotFind =>
                    user_err ("Trying to break/clear nonexistent function " ^ name)
                )
              )
          | _ => user_err ("Breaking/clearing non-function identifier " ^ name)
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
                  handle SymDict.Absent =>
                    prog_err
                      ("Failed to find value identifier " ^ lightblue (Symbol.toValue id)
                      ^ " during signature ascription"
                      )
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
                      handle SymDict.Absent =>
                      prog_err
                        ("Failed to find type " ^ lightblue (Symbol.toValue id)
                        ^ " during signature ascription"
                        )

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
                      prog_err
                        ("Arity mismatch for type " ^ lightblue (Symbol.toValue id)
                        ^ " during signature ascription"
                        )
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
                    prog_err
                      ("Failed to find exception " ^ lightblue (Symbol.toValue id)
                      ^ " during signature ascription"
                      )
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
                  handle SymDict.Absent =>
                  prog_err
                    ("Could not find module " ^ lightblue (Symbol.toValue id)
                    ^ " during signature ascription"
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
  end
