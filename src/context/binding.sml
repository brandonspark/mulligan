(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure SH = SMLSyntaxHelpers

structure Binding :
  sig
    type t = SMLSyntax.context
    type sigval = SMLSyntax.sigval

    val get_pat_bindings : t -> SMLSyntax.pat -> PrettyPrintContext.MarkerSet.set
    val get_pat_ids : t -> SMLSyntax.pat -> SMLSyntax.symbol list
    val get_fname_args_bindings :
      t -> SMLSyntax.fname_args -> PrettyPrintContext.MarkerSet.set
    val remove_bound_ids : t -> PrettyPrintContext.MarkerSet.set -> t
    val open_bound_ids : t -> (SMLSyntax.symbol list) list -> PrettyPrintContext.MarkerSet.set
    val get_funarg_bound_ids : t -> SMLSyntax.funarg -> PrettyPrintContext.MarkerSet.set

    val generate_sigbinding :
         t
      -> {id : SMLSyntax.symbol, signat : SMLSyntax.signat}
      -> SMLSyntax.symbol * sigval

    val add_sigbindings :
         t
      -> (SMLSyntax.symbol * sigval) list
      -> t

    val add_funbind : t -> SMLSyntax.funbind -> t
  end =
  struct
    open PrettyPrintContext
    open Context
    open SMLSyntax
    open Error

    infix |>
    fun x |> f = f x

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
            | _ => eval_err ("Cannot find constructor " ^ TerminalColors.text
            TerminalColors.lightblue (SH.longid_to_str id))
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
            | _ =>
              eval_err ("Cannot find constructor " ^ TerminalColors.text
              TerminalColors.lightblue (SH.longid_to_str id))
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
            ( MarkerSet.singleton (VAL id)
              :: List.map (get_pat_bindings ctx) args
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

    (* We should not be able to do this for a constructor or exception, because
     * they will be interpreted as matching against their patterns.
     *)
    fun remove_val_scope_bound_id scope id =
      let
        val identdict = scope_identdict scope
      in
        case SymDict.find identdict id of
          NONE => scope
        | SOME (E _ | C _) => prog_err "trying to remove val bound id of constructor or exception"
        | SOME (V _) => scope_set_identdict scope (SymDict.remove identdict id)
      end

    fun remove_mod_scope_bound_id scope id =
      let
        val moddict = scope_moddict scope
      in
        scope_set_moddict scope (SymDict.remove moddict id)
      end

    fun remove_bound_id_base f {scope, outer_scopes, dtydict, sigdict, functordict,
    tyvars, hole_print_fn, settings, abstys} id =
      let
        val scope = f scope id
        val outer_scopes =
          List.map
            (fn scope => f scope id)
            outer_scopes
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

    fun remove_bound_ids ctx ids =
      MarkerSet.foldl
        (fn (VAL id, ctx) =>
          remove_bound_id_base remove_val_scope_bound_id ctx id
        | (MOD id, ctx) =>
          remove_bound_id_base remove_mod_scope_bound_id ctx id
        )
        ctx
        ids

    fun get_sigval_bound_ids (_ : SMLSyntax.context) (Sigval {valspecs, modspecs, ...})=
      marker_set_of_list
        ( List.map VAL (SymDict.domain valspecs)
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

    (* We only care about killed value bindings.
     * So disregard if it's not.
     *)
    fun open_bound_id ctx longid =
      let
        val identdict = scope_identdict (get_module ctx longid)
        val moddict = scope_moddict (get_module ctx longid)
      in
        ( SymDict.foldl
            (fn (id, elem, acc) =>
              case elem of
                (V _) => VAL id :: acc
              | _ => acc
            )
            []
            identdict
          @
          List.map MOD (SymDict.domain moddict)
        )
        |> marker_set_of_list
      end

    fun open_bound_ids ctx longids =
      union_sets
        (List.map (open_bound_id ctx) longids)

    fun generate_sigbinding ctx {id, signat} =
      (id, Value.evaluate_signat ctx signat)

    fun add_sigbindings {scope, outer_scopes, dtydict, sigdict, functordict,
    tyvars, hole_print_fn, settings, abstys} sigbindings =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict =
            ( List.foldl
                (fn ((id, sigval), sigdict) =>
                  SymDict.insert sigdict id sigval
                )
                sigdict
                sigbindings
            )
      , functordict = functordict
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }

    fun add_funbind (ctx as {scope, outer_scopes, dtydict, sigdict, functordict, tyvars
    , hole_print_fn, settings, abstys})
                    {id, funarg, seal, body} =
      { scope = scope
      , outer_scopes = outer_scopes
      , dtydict = dtydict
      , sigdict = sigdict
      , functordict =
          SymDict.insert
            functordict
            id
            ( Functorval
              { arg_seal =
                  case funarg of
                    Normal {id, signat} =>
                      { id = SOME id, sigval = Value.evaluate_signat ctx signat }
                  | Sugar spec =>
                      { id = NONE, sigval = Value.evaluate_signat ctx (Sspec spec) }
              , seal =
                  Option.map
                    (fn {signat, opacity} =>
                      { opacity = opacity, sigval = Value.evaluate_signat ctx signat }
                    )
                    seal
              , body = body
              }
            )
      , tyvars = tyvars
      , hole_print_fn = hole_print_fn
      , settings = settings
      , abstys = abstys
      }
  end
