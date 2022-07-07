
structure Context :
  sig

    (* The outer scopes are a list, in order of ascending outwardness.
     * Each scope maps to the stuff in it, and all things in scope are within
     * the current value of outer_scopes.
     *)

    (* Context stuff. *)

    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type t = SMLSyntax.context
    type identdict = SMLSyntax.identdict

    (* The longid is purely for the pretty printer. We have to have an idea of
     * what to call the exception.
     *)
    exception Raise of SMLSyntax.longid * ExnId.t * SMLSyntax.value option

    val scope_empty : scope
    val merge_scope : t -> scope -> t
    val ctx_rec : scope -> scope

    val scope_identdict : scope -> identdict
    val scope_moddict : scope -> scope SymDict.dict

    val scope_set_identdict : scope -> identdict -> scope
    val scope_set_moddict : scope -> scope SymDict.dict -> scope

    val get_val : t -> SMLSyntax.longid -> value
    val get_val_opt : t -> SMLSyntax.longid -> value option
    val get_con_opt : t -> SMLSyntax.longid -> TyId.t option
    val get_exn_opt : t -> SMLSyntax.longid -> ExnId.t option
    val get_ident_ty_opt : t -> SMLSyntax.longid -> (SMLSyntax.sign * SMLSyntax.type_scheme) option
    val get_type_synonym : t -> SMLSyntax.longid -> SMLSyntax.synonym
    val get_sigval_opt : t -> SMLSyntax.symbol -> SMLSyntax.sigval option
    val get_functorval_opt : t -> SMLSyntax.symbol -> SMLSyntax.functorval option
    val get_ident_ty : t -> SMLSyntax.longid -> SMLSyntax.sign * SMLSyntax.type_scheme
    val get_module : t -> SMLSyntax.longid -> scope
    val get_sig : t -> SMLSyntax.symbol -> SMLSyntax.sigval
    val get_functor : t -> SMLSyntax.symbol -> SMLSyntax.functorval
    val get_dtydict : t -> SMLSyntax.dtydict
    val get_datatype_with_longid : t -> SMLSyntax.longid -> SMLSyntax.dtyinfo
    val get_infixity : t -> SMLSyntax.symbol -> SMLSyntax.infixity * int

    val is_con : t -> SMLSyntax.longid -> bool
    val is_exn : t -> SMLSyntax.longid -> bool

    val add_exn : t -> ExnId.t -> SMLSyntax.symbol * SMLSyntax.type_scheme -> t
    val add_type_synonym : t -> SMLSyntax.symbol -> SMLSyntax.synonym -> t
    val add_val_ty_bindings : t -> (SMLSyntax.symbol * SMLSyntax.type_scheme) list -> t
    val add_unquantified_val_ty_bindings :
      t -> (SMLSyntax.symbol * SMLSyntax.tyval) list -> t
    (*val add_con : t -> SMLSyntax.symbol -> t *)
    val add_infix : t -> (SMLSyntax.symbol * SMLSyntax.infixity * int) -> t
    val add_module : t -> SMLSyntax.symbol -> scope -> t
    val add_sig : t -> SMLSyntax.symbol -> SMLSyntax.sigval -> t
    val add_functor : t -> SMLSyntax.symbol -> SMLSyntax.functorval -> t
    val add_abstys : t -> SMLSyntax.type_scheme AbsIdDict.dict -> t
    val add_scoped_tyvars : t -> SymSet.set -> t

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
    val add_bindings_combined : t -> (SMLSyntax.symbol * value * SMLSyntax.tyval) list -> t
    val add_rec_bindings : t -> (SMLSyntax.symbol * value) list -> t

    (* Type stuff. *)

    val synth_ty :
         (SMLSyntax.symbol * SMLSyntax.tyval list -> SMLSyntax.tyval option)
      -> (SMLSyntax.symbol -> SMLSyntax.tyval option)
      -> SMLSyntax.context
      -> SMLSyntax.ty
      -> SMLSyntax.tyval
    val synth_ty' :
         SMLSyntax.context
      -> SMLSyntax.ty
      -> SMLSyntax.tyval

    val norm_tyval :
         t
      -> SMLSyntax.tyval
      -> SMLSyntax.tyval

    val mk_type_scheme :
         (SMLSyntax.symbol * SMLSyntax.tyval list -> SMLSyntax.tyval option)
      -> SMLSyntax.symbol list
      -> SMLSyntax.ty
      -> t
      -> SMLSyntax.type_scheme

    val replicate_datatype : t -> SMLSyntax.symbol * SMLSyntax.symbol list -> t

    val get_current_tyvars : (SMLSyntax.tyval -> SMLSyntax.tyvar list) -> t -> SMLSyntax.tyvar list

    val add_datbind :
        (SMLSyntax.symbol * SMLSyntax.tyval list -> SMLSyntax.tyval option)
     -> t
     -> TyId.t * SMLSyntax.datbind
     -> t

  end =
  struct
    open PrettyPrintContext
    open SMLSyntax
    open Error
    open Printf

    type value = SMLSyntax.value
    type scope = SMLSyntax.scope
    type identdict = SMLSyntax.identdict
    type t = SMLSyntax.context

    fun lightblue s = TerminalColors.text TerminalColors.lightblue s

    val sym_true = Symbol.fromValue "true"
    val sym_false = Symbol.fromValue "false"

    val empty_set = MarkerSet.empty

    (* Helpers *)

    infix |>
    fun x |> f = f x
    fun enum l =
       List.foldl
         (fn (elem, (i, acc)) =>
           (i + 1, (i, elem) :: acc)
         )
         (0, [])
         l
       |> (fn (_, l) => List.rev l)

    fun concatMap f = List.concat o List.map f

    fun snoc l =
      ( List.take (l, List.length l - 1)
      , List.nth (l, List.length l - 1)
      )

    (* Some types *)

    fun lift f {scope, outer_scopes, dtydict, sigdict, functordict, tyvars, hole_print_fn,
    settings, abstys } =
      let
        val (scope, outer_scopes) = f (scope, outer_scopes)
      in
        { scope = scope
        , outer_scopes = outer_scopes
        , dtydict = dtydict
        , sigdict = sigdict
        , tyvars = tyvars
        , functordict = functordict
        , hole_print_fn = hole_print_fn
        , settings = settings
        , abstys = abstys
        }
      end

    (* CONTEXT STUFF *)

    exception Raise of SMLSyntax.longid * ExnId.t * value option

    val scope_empty =
      Scope
        { identdict = SymDict.empty
        , valtydict = SymDict.empty
        , moddict = SymDict.empty
        , infixdict = SymDict.empty
        , tynamedict = SymDict.empty
        }

    fun scope_identdict (Scope {identdict, ...}) = identdict
    fun scope_valtydict (Scope {valtydict, ...}) = valtydict
    fun scope_moddict (Scope {moddict, ...}) = moddict
    fun scope_infixdict (Scope {infixdict, ...}) = infixdict
    fun scope_tynamedict (Scope {tynamedict, ...}) = tynamedict

    fun scope_set_identdict (Scope {  identdict, valtydict, moddict, infixdict,
                                    tynamedict}) new =
      Scope { identdict = new
            , valtydict = valtydict
            , moddict = moddict
            , infixdict = infixdict
            , tynamedict = tynamedict
            }
    fun scope_set_valtydict (Scope {  identdict, valtydict, moddict, infixdict,
                                    tynamedict}) new =
      Scope { identdict = identdict
            , valtydict = new
            , moddict = moddict
            , infixdict = infixdict
            , tynamedict = tynamedict
            }
    fun scope_set_moddict (Scope {  identdict, valtydict, moddict, infixdict,
                                    tynamedict}) new =
      Scope { identdict = identdict
            , valtydict = valtydict
            , moddict = new
            , infixdict = infixdict
            , tynamedict = tynamedict
            }
    fun scope_set_infixdict (Scope {  identdict, valtydict, moddict, infixdict,
                                    tynamedict}) new =
      Scope { identdict = identdict
            , valtydict = valtydict
            , moddict = moddict
            , infixdict = new
            , tynamedict = tynamedict
            }
    fun scope_set_tynamedict (Scope {  identdict, valtydict, moddict, infixdict,
                                    tynamedict}) new =
      Scope { identdict = identdict
            , valtydict = valtydict
            , moddict = moddict
            , infixdict = infixdict
            , tynamedict = new
            }

    fun merge_scope ctx scope =
      lift (fn (cur_scope, ctx) =>
        ( Scope
            { identdict =
                SymDict.union
                  (scope_identdict cur_scope)
                  (scope_identdict scope)
                  (fn (_, _, snd) => snd)
            , valtydict =
                SymDict.union
                  (scope_valtydict cur_scope)
                  (scope_valtydict scope)
                  (fn (_, _, snd) => snd)
            , moddict =
                SymDict.union
                  (scope_moddict cur_scope)
                  (scope_moddict scope)
                  (fn (_, _, snd) => snd)
            , infixdict =
                SymDict.union
                  (scope_infixdict cur_scope)
                  (scope_infixdict scope)
                  (fn (_, _, snd) => snd)
            , tynamedict =
                SymDict.union
                  (scope_tynamedict cur_scope)
                  (scope_tynamedict scope)
                  (fn (_, _, snd) => snd)
            }
        , ctx
        )
      ) ctx

    fun ctx_rec (scope as Scope { identdict, ... }) =
      SymDict.map
        (fn V (Vfn {matches, env, rec_env, break, abstys}) =>
            V (Vfn { matches = matches
                  , env = env
                  , rec_env = SOME scope
                  , break = break
                  , abstys = abstys
                  }
              )
        | other => other
        )
        identdict
      |> scope_set_identdict scope

    exception CouldNotFind

    (* For looking backwards through all contexts, stopping on the first scope
     * that returns SOME.
     *)
    fun map_scope_id f (ctx as {scope, outer_scopes, dtydict, sigdict, functordict,
    tyvars, hole_print_fn, settings, abstys}) id =
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
        , dtydict = dtydict
        , sigdict = sigdict
        , functordict = functordict
        , tyvars = tyvars
        , hole_print_fn = hole_print_fn
        , settings = settings
        , abstys = abstys
        }
      end

    (* For mapping the first module fitting the identifier, stopping on the
     * first instance found.
     *)
    fun map_module f (ctx as {dtydict, sigdict, functordict, tyvars, hole_print_fn,
    settings, abstys, ...} : SMLSyntax.context) id =
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
                      , dtydict = dtydict
                      , sigdict = sigdict
                      , functordict = functordict
                      , tyvars = tyvars
                      , hole_print_fn = hole_print_fn
                      , settings = settings
                      , abstys = abstys
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
        (f (x, get_module orig_ctx xs))
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

    fun get_con_opt ctx id =
      SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope =>
                case (SymDict.find (scope_identdict scope) x) of
                  SOME (C tyid) => SOME tyid
                | _ => NONE)
              (ctx_scopes ctx)
        | _ =>
            get_base (fn (id, scope) =>
                       case SymDict.find (scope_identdict scope) id of
                         SOME (C tyid) => tyid
                       | _ => raise CouldNotFind) ctx id
      ) handle CouldNotFind => NONE

    fun get_con ctx id =
      case get_con_opt ctx id of
        NONE =>
          prog_err ("Nonexistent constructor " ^ lightblue (longid_to_str id))
      | SOME ans => ans

    fun is_con ctx id = case get_con_opt ctx id of NONE => false | _ => true

    fun get_exn_opt ctx id =
      SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope =>
                case (SymDict.find (scope_identdict scope) x) of
                  SOME (E exnid) => SOME exnid
                | _ => NONE)
              (ctx_scopes ctx)
        | _ =>
            get_base (fn (id, scope) =>
                       case SymDict.find (scope_identdict scope) id of
                         SOME (E exnid) => exnid
                       | _ => raise CouldNotFind) ctx id
      ) handle CouldNotFind => NONE

    fun is_exn ctx id = case get_exn_opt ctx id of NONE => false | _ => true

    (* Exceptions are also cons.
     *)
    val is_con = fn ctx => fn id => is_con ctx id orelse is_exn ctx id

    fun open_path ctx path =
      merge_scope
        ctx
        (get_module ctx path)
      handle CouldNotFind =>
        prog_err
          ("Could not open nonexistent module "
          ^ TerminalColors.text TerminalColors.lightblue (longid_to_str path)
          )

    fun get_val_opt ctx id =
      SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope =>
                case (SymDict.find (scope_identdict scope) x) of
                  SOME (V value) => SOME value
                | _ => NONE)
              (ctx_scopes ctx)
        | _ =>
            get_base (fn (id, scope) =>
                       case SymDict.find (scope_identdict scope) id of
                         SOME (V value) => value
                       | _ => raise CouldNotFind) ctx id
      ) handle CouldNotFind => NONE

    fun get_val ctx id =
      case get_val_opt ctx id of
        NONE =>
          prog_err ("Nonexistent value binding to identifier " ^ lightblue (longid_to_str id))
      | SOME ans => ans

    fun get_ident_ty_opt ctx id =
      SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope =>
                SymDict.find (scope_valtydict scope) x
              )
              (ctx_scopes ctx)
        | _ =>
            get_base (fn (id, scope) =>
                       case SymDict.find (scope_valtydict scope) id of
                         SOME ans => ans
                       | _ => raise CouldNotFind) ctx id
      ) handle CouldNotFind => NONE

    fun get_type_synonym_opt ctx id =
      SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope =>
                SymDict.find (scope_tynamedict scope) x
              )
              (ctx_scopes ctx)
        | _ =>
            get_base (fn (id, scope) =>
                       case SymDict.find (scope_tynamedict scope) id of
                         SOME ans => ans
                       | _ => raise CouldNotFind) ctx id
      ) handle CouldNotFind => NONE

    fun get_type_synonym ctx id =
      case get_type_synonym_opt ctx id of
        NONE => prog_err "could not find type synonym"
      | SOME ans => ans


    fun get_ident_ty ctx id =
      case get_ident_ty_opt ctx id of
        NONE => prog_err "could not find ident to get ty of"
      | SOME ans => ans

    fun get_infixity ctx id =
      iter_scopes
        (fn scope => SymDict.find (scope_infixdict scope) id)
        (ctx_scopes ctx)
      handle CouldNotFind =>
        prog_err ("Nonexistent infix identifier " ^ lightblue (Symbol.toValue id))

    fun get_type_synonym_opt ctx id =
      ( SOME ( case id of
          [x] =>
            iter_scopes
              (fn scope => SymDict.find (scope_tynamedict scope) x)
              (ctx_scopes ctx)
        | _ =>
            get_base
              (fn (id, scope) => SymDict.lookup (scope_tynamedict scope) id
                handle SymDict.Absent => raise CouldNotFind
              )
              ctx id
        )
      )
      handle CouldNotFind => NONE

    fun get_type_synonym ctx id =
      case get_type_synonym_opt ctx id of
        NONE =>
          prog_err ("Nonexistent type synonym " ^ lightblue (longid_to_str id))
      | SOME ans => ans

    fun get_dtydict (ctx : SMLSyntax.context) = ! (#dtydict ctx)

    fun get_datatype ctx tyid =
      TyIdDict.lookup (! (#dtydict ctx)) tyid
      handle TyIdDict.Absent => prog_err "could not find datatype with tyid"

    fun get_datatype_with_longid ctx id =
      (case get_type_synonym ctx id of
        Datatype tyid => get_datatype ctx tyid
      | _ => prog_err "could not find datatype with id"
      )

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

    fun add_exn ctx exn_id (id, tyscheme) =
      lift (fn (scope, rest) =>
        let
          val identdict = scope_identdict scope
          val valtydict = scope_valtydict scope
        in
          ( scope_set_valtydict
              (scope_set_identdict scope (SymDict.insert identdict id (E exn_id)))
              (SymDict.insert valtydict id (Esign, tyscheme))
          , rest
          )
        end
      ) ctx

    fun add_type_synonym ctx id synonym =
      lift (fn (scope, rest) =>
        let
          val tynamedict = scope_tynamedict scope
        in
          ( scope_set_tynamedict
              scope
              (SymDict.insert tynamedict id synonym)
          , rest
          )
        end
      ) ctx

    fun add_val_ty_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val valtydict = scope_valtydict scope
        in
          ( List.foldl
              (fn ((id, tyscheme), valtydict) =>
                SymDict.insert valtydict id (Vsign, tyscheme)
              )
              valtydict
              bindings
            |> scope_set_valtydict scope
          , rest
          )
        end
      ) ctx

    fun add_unquantified_val_ty_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val valtydict = scope_valtydict scope
        in
          ( List.foldl
              (fn ((id, tyval), valtydict) =>
                SymDict.insert valtydict id (Vsign, (0, fn _ => tyval))
              )
              valtydict
              bindings
            |> scope_set_valtydict scope
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

    fun add_sig {scope, outer_scopes, dtydict, sigdict, functordict, tyvars, hole_print_fn,
    settings, abstys} id sigval =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = SymDict.insert sigdict id sigval
      , functordict = functordict
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }

    fun add_functor {scope, outer_scopes, dtydict, sigdict, functordict, tyvars, hole_print_fn,
    settings, abstys} id functorval =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = sigdict
      , functordict = SymDict.insert functordict id functorval
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }

  fun add_abstys (ctx as {scope, outer_scopes, dtydict, sigdict, functordict, tyvars
    , hole_print_fn, settings, abstys})
                    new =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = sigdict
      , functordict = functordict
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = new
      }

    fun add_scoped_tyvars {scope, outer_scopes, dtydict, sigdict, functordict, tyvars,
    hole_print_fn, settings, abstys} new_tyvars =
      let
        val tyvars =
          SymSet.foldl
            (fn (tyvar, tyvars) =>
              if SymSet.member tyvars tyvar then
                prog_err "shadowed implicit type variables"
              else
                SymSet.insert tyvars tyvar
            )
            tyvars
            new_tyvars
      in
        { scope = scope
        , outer_scopes = outer_scopes
        , dtydict = dtydict
        , sigdict = sigdict
        , functordict = functordict
        , tyvars = tyvars
        , hole_print_fn = hole_print_fn
        , settings = settings
        , abstys = abstys
        }
      end

    fun cm_export path ctx ctx' {structs, sigs, functors} =
      let
        fun orange s = TerminalColors.text TerminalColors.orange s
        val modules =
          List.map
            (fn s =>
              (s, get_module ctx' [s])
              handle CouldNotFind =>
                Printf.printf
                  (`"Failed to find module "fs" to export in "fs"")
                  (orange (Symbol.toValue s))
                  (lightblue path)
                |> prog_err
            )
            structs

        val sigs =
          List.map
            (fn s =>
              case get_sigval_opt ctx' s of
                NONE =>
                  Printf.printf
                    (`"Failed to find signature "fs" to export in "fs"")
                    (orange (Symbol.toValue s))
                    (lightblue path)
                  |> prog_err
              | SOME ans => (s, ans)
            )
            sigs

        val functors =
          List.map
            (fn s =>
              case get_functorval_opt ctx' s of
                NONE =>
                  Printf.printf
                    (`"Failed to find functor "fs" to export in "fs"")
                    (orange (Symbol.toValue s))
                    (lightblue path)
                  |> prog_err
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

    fun add_bindings_combined ctx bindings =
      lift (fn (scope, rest) =>
        let
          val identdict = scope_identdict scope
          val valtydict = scope_valtydict scope
          val (new_identdict, new_valtydict) =
            List.foldl
              (fn ((id, value, tyval), (identdict, valtydict)) =>
                ( SymDict.insert identdict id (V value)
                , SymDict.insert valtydict id (Vsign, (0, fn _ => tyval))
                )
              )
              (identdict, valtydict)
              bindings
        in
            scope_set_identdict
              (scope_set_valtydict scope new_valtydict)
              new_identdict
          |> (fn scope => (scope, rest))
        end
      ) ctx

    fun add_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val identdict = scope_identdict scope
        in
          List.foldl
            (fn ((id, value), identdict) =>
              SymDict.insert identdict id (V value)
            )
            identdict
            bindings
          |> scope_set_identdict scope
          |> (fn scope => (scope, rest))
        end
      ) ctx

    fun add_rec_bindings ctx bindings =
      lift (fn (scope, rest) =>
        let
          val identdict = scope_identdict scope

          val rec_identdict =
            List.foldl
              (fn ((id, value), identdict) =>
                SymDict.insert identdict id (V value)
              )
              SymDict.empty
              bindings
            |> scope_set_identdict scope
            |> ctx_rec
            |> scope_identdict
        in
          ( scope_set_identdict scope
              (SymDict.union identdict rec_identdict (fn (_, _, snd) => snd))
          , rest
          )
        end
      ) ctx

    fun add_hole_print_fn
        {scope, outer_scopes, dtydict, sigdict, functordict, tyvars,
         hole_print_fn, settings, abstys} f =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = sigdict
      , functordict = functordict
      , tyvars = tyvars
      , hole_print_fn = f
      , settings = settings
      , abstys = abstys
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

        fun new_vfn {matches, env, rec_env, break, abstys} f =
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
                  , abstys = abstys
                  }
              |> f
          )
      in
        ( case id of
          [x] =>
            ( map_scope_id (fn scope =>
                case SymDict.find (scope_identdict scope) x of
                  NONE => NONE
                | SOME (V (Vfn info)) =>
                    new_vfn info (fn vfn =>
                      SymDict.insert (scope_identdict scope) x (V vfn)
                      |> scope_set_identdict scope
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
                      SymDict.insert (scope_identdict scope) x (V vfn)
                      |> scope_set_identdict scope
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

    (* This is here because add_datbind needs mk_type_scheme, which needs
     * synth_ty.
     *)

    fun synth_ty datatype_fn tyvar_fn ctx ty =
      let
        val synth_ty = fn ctx => fn ty => synth_ty datatype_fn tyvar_fn ctx ty

        fun handle_type_synonym tyvals id =
          case get_type_synonym ctx id of
            Datatype tyid => TVapp (tyvals, tyid)
          | Scheme (n, f) =>
              if List.length tyvals <> n then
                prog_err "invalid arity for type synonym"
              else
                f tyvals
      in
        case ty of
          Tident [id] =>
            (case datatype_fn (id, []) of
              NONE => handle_type_synonym [] [id]
            | SOME ty => ty
            )
        | Tident longid =>
            handle_type_synonym [] longid
        | Ttyvar sym =>
            (case tyvar_fn sym of
              NONE => TVtyvar sym
            | SOME tyval => tyval
            )
        | Tapp (tys, [id]) =>
            (case datatype_fn (id, List.map (synth_ty ctx) tys) of
              NONE => handle_type_synonym (List.map (synth_ty ctx) tys) [id]
            | SOME ty => ty
            )
        | Tapp (tys, longid) =>
            handle_type_synonym (List.map (synth_ty ctx) tys) longid
        | Tprod tys =>
            TVprod (List.map (synth_ty ctx) tys)
        | Tarrow (t1, t2) =>
            TVarrow (synth_ty ctx t1, synth_ty ctx t2)
        | Trecord fields =>
            TVrecord
              (List.map (fn {lab, ty} => {lab = lab, tyval = synth_ty ctx ty}) fields)
        | Tparens ty => synth_ty ctx ty
      end

    fun synth_ty' ctx ty = synth_ty (fn _ => NONE) (fn _ => NONE) ctx ty

    fun norm_tyval ctx tyval =
      case tyval of
        TVvar (_, r as ref NONE) => tyval
          (* May loop forever if the tyval contains the same ref.
           *)
      | TVvar (_, r as ref (SOME (Ty tyval))) =>
          norm_tyval ctx tyval
      | TVvar (_, r as ref (SOME (Rows _))) => tyval
      | TVapp (tyvals, tyid) =>
          TVapp (List.map (norm_tyval ctx) tyvals, tyid)
      | TVabs (tyvals, absid) =>
          (* If we have some abstract tyvals that we're allowed to know about,
           * then just evaluate them to what they should be.
           *)
          ( case AbsIdDict.find (#abstys (ctx : SMLSyntax.context)) absid of
            NONE => TVabs (List.map (norm_tyval ctx) tyvals, absid)
          | SOME (_, ty_fn) => ty_fn (List.map (norm_tyval ctx) tyvals)
          )
      | TVprod tyvals =>
          TVprod (List.map (norm_tyval ctx) tyvals)
      | TVrecord fields =>
          TVrecord
            (List.map (fn {lab, tyval} => {lab = lab, tyval = norm_tyval ctx tyval}) fields)
      | TVarrow (t1, t2) =>
          TVarrow (norm_tyval ctx t1, norm_tyval ctx t2)
      | TVtyvar sym => TVtyvar sym

    fun mk_type_scheme datatype_fn tyvars ty ctx =
      let
        val arity = List.length tyvars
      in
        ( arity
        , fn tyvals =>
          let
            val paired = ListPair.zipEq (tyvars, tyvals)
            val tyvar_fn =
              fn sym =>
                  (* This is the index in the original tyvars that we are
                   * replacing.
                   *)
                case List.find (fn (tyvar, _) => Symbol.eq (sym, tyvar)) paired of
                  NONE => NONE
                | SOME (_, ty) => SOME (ty)

            val default_datatype_fn =
              fn (sym, tyvals) =>
                case datatype_fn (sym, tyvals) of
                  SOME ty => SOME ty
                | _ =>
                  case get_type_synonym_opt ctx [sym] of
                    SOME (Datatype tyid) => SOME (TVapp (tyvals, tyid))
                  | SOME (Scheme (n, f)) =>
                      SOME (f tyvals)
                  | NONE => NONE
          in
            if List.length tyvals <> arity then
              prog_err "invalid arity for instantiated type scheme"
            else
              synth_ty default_datatype_fn tyvar_fn ctx ty
          end
        )
      end

    fun add_datbind datatype_fn ctx (tyid, {tyvars, tycon, conbinds}) =
      lift (fn (scope, rest) =>
        let
          val dtydict = ! (#dtydict ctx)
          val tynamedict = scope_tynamedict scope
          val identdict = scope_identdict scope
          val valtydict = scope_valtydict scope

          val arity = List.length tyvars
          val cons =
            List.map
              (fn {id, ty, opp} =>
                { id = id
                , tyscheme =
                  case ty of
                    NONE =>
                      mk_type_scheme
                        datatype_fn
                        tyvars
                        (Tapp (List.map Ttyvar tyvars, [tycon]))
                        ctx
                  | SOME ty =>
                      mk_type_scheme
                        datatype_fn
                        tyvars
                        (Tarrow (ty, Tapp (List.map Ttyvar tyvars, [tycon])))
                        ctx
                }
              )
              conbinds

          val new_dtydict =
            TyIdDict.insert dtydict tyid { arity = arity
                                         , cons = cons
                                         }
          val _ = #dtydict ctx := new_dtydict

          val new_tynamedict =
            SymDict.insert tynamedict tycon (Datatype tyid)

          val new_identdict =
            List.foldl
              (fn ({id, ...}, acc) =>
                SymDict.insert acc id (C tyid)
              )
              identdict
              cons

          val new_valtydict =
            List.foldl
              (fn ({id, tyscheme}, acc) =>
                SymDict.insert acc id (Csign, tyscheme)
              )
              valtydict
              cons
        in
          ( scope_set_identdict
              (scope_set_valtydict
                (scope_set_tynamedict scope new_tynamedict)
                new_valtydict
              )
              new_identdict
          , rest
          )
        end
      ) ctx

    (* TODO: add cons *)
    (* TODO: there may be an issue with scoping of datatypes
     * if a datatype goes out of scope but is copied, we want to still be able
     * to refer to its tydict entry
     *)
    fun replicate_datatype orig_ctx (left_id, right_id) =
      lift (fn ctx as (scope, rest) =>
        let
          val tynamedict = scope_tynamedict scope
          val identdict = scope_identdict scope
          val valtydict = scope_valtydict scope
        in
          case get_type_synonym orig_ctx right_id of
            Datatype tyid =>
              let
                val {arity, cons} = get_datatype orig_ctx tyid
                val (new_identdict, new_valtydict) =
                  List.foldl
                    (fn ({id, tyscheme}, (new_identdict, new_valtydict)) =>
                      ( SymDict.insert new_identdict id (C tyid)
                      , SymDict.insert new_valtydict id (Csign, tyscheme)
                      )
                    )
                    (identdict, valtydict)
                    cons
              in
              ( scope_set_tynamedict
                  (scope_set_identdict
                    (scope_set_valtydict
                      scope
                      new_valtydict
                    )
                    new_identdict
                  )
                  (SymDict.insert tynamedict left_id (Datatype tyid))
              , rest
              )
              end
          | _ => prog_err "copying a non-datatype"
        end
      ) orig_ctx

    local
      fun insert_tyvar tyvar l =
        case
          List.find (fn tyvar' => tyvar_eq (tyvar, tyvar')) l
        of
          NONE => tyvar :: l
        | SOME _ => l
    in
      (* Gets all the tyvars currently in the valtydict (implicit), as well as
       * those which have been deliberately added to the tyvarseqs (explicit).
       *)
      fun get_current_tyvars collect_tyvars_tyval
            ({scope, outer_scopes, tyvars = cur_tyvars, ...} : SMLSyntax.context) =
        List.foldl
          (fn (scope, tyvars) =>
            SymDict.foldl
              (fn (_, (sign, (arity, ty_fn)), tyvars) =>
                collect_tyvars_tyval (ty_fn (List.tabulate (arity, fn _ => TVprod []))) @ tyvars
              )
              tyvars
              (scope_valtydict scope)
          )
          []
          (scope :: outer_scopes)
        |> List.foldl
            (fn (tyvar, acc) => insert_tyvar tyvar acc)
            []
        |> (fn valty_tyvars => valty_tyvars @ List.map Proper (SymSet.toList cur_tyvars))
    end

  end
