(** Brandon Wu
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure SH = SMLSyntaxHelpers
open PrettyPrintContext
open Context
open SMLSyntax
open Error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file deals with "bindings", which are the bindings present in the context
 * at the time of pretty-printing the AST.
 *
 * The basic problem has to do with the fact that the pretty-printer is contextual.
 * If you wanted to print out the following program:
 *
 * val _ = x + x
 * val x = 2
 * val y = 3 + x
 *
 * with an environment of { x -> 1 }
 *
 * you would want to print out `val _ = 1 + 1`, but NOT `val y = 3 + 1`.
 * This is because the `x` mentioned in the second case is actually a different
 * `x` than the one in our environment!
 *
 * To deal with this, we have some logic which removes identifiers from our context
 * based on when we crawl forward or backward in the AST.
 *
 * Essentially, we are doing naming here, but in a dictionary instead of within
 * the AST nodes themselves.
 *)

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature BINDING =
  sig
    type t = SMLSyntax.context
    type bindings = PrettyPrintContext.MarkerSet.set

    val of_pat : t -> SMLSyntax.pat -> bindings
    val ids_of_pat : t -> SMLSyntax.pat -> SMLSyntax.symbol list
    val of_fname_args :
      t -> SMLSyntax.fname_args -> bindings

    val remove_bindings : t -> bindings -> t
    val of_modname : t -> SMLSyntax.longid -> bindings
    val of_funarg : t -> SMLSyntax.funarg -> bindings
  end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

fun marker_set_of_list l =
  List.foldl
    (fn (x, acc) =>
      MarkerSet.insert acc x
    )
    MarkerSet.empty
    l

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure Binding : BINDING =
  struct
    type t = SMLSyntax.context
    type bindings = PrettyPrintContext.MarkerSet.set

    fun get_patrow_bindings ctx patrow =
      case patrow of
        PRellipsis => MarkerSet.empty
      | PRlab {pat, ...} => of_pat ctx pat
      | PRas {id, aspat, ...} =>
          MarkerSet.union
            (MarkerSet.singleton (VAL id))
            (case aspat of
              NONE => MarkerSet.empty
            | SOME pat => of_pat ctx pat
            )

    and of_pat ctx pat =
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
      | Pparens pat => of_pat ctx pat
      | Ptuple pats => List.map (of_pat ctx) pats |> union_sets
      | Plist pats => List.map (of_pat ctx) pats |> union_sets
      | Por pats => List.map (of_pat ctx) pats |> union_sets
      | Papp {id, atpat, ...} =>
          if is_con ctx id then
            of_pat ctx atpat
          else
            (case id of
              [id] =>
                MarkerSet.union
                  (MarkerSet.singleton (VAL id))
                  (of_pat ctx atpat)
            | _ =>
              eval_err ("Cannot find constructor " ^ TerminalColors.text
              TerminalColors.lightblue (SH.longid_to_str id))
            )
      | Pinfix {left, id, right} =>
          if is_con ctx [id] then
            MarkerSet.union
              (of_pat ctx left)
              (of_pat ctx right)
          else
            union_sets
              [ MarkerSet.singleton (VAL id)
              , of_pat ctx left
              , of_pat ctx right
              ]
      | Ptyped {pat, ...} => of_pat ctx pat
      | Playered {id, aspat, ...} =>
          MarkerSet.union
            (MarkerSet.singleton (VAL id))
            (of_pat ctx aspat)

    fun ids_of_pat ctx pat =
      List.map (fn VAL id => id | _ => raise Fail "shouldn't happen")
      (MarkerSet.toList (of_pat ctx pat))

    fun of_fname_args ctx fname_args =
      case fname_args of
        Fprefix {id, args, ...} =>
          union_sets
            ( MarkerSet.singleton (VAL id)
              :: List.map (of_pat ctx) args
            )
      | Finfix {left, id, right} =>
          union_sets
            [ of_pat ctx left
            , MarkerSet.singleton (VAL id)
            , of_pat ctx right
            ]
      | Fcurried_infix {left, id, right, args} =>
          union_sets
            ( [ of_pat ctx left
              , MarkerSet.singleton (VAL id)
              , of_pat ctx right
              ]
              @ List.map (of_pat ctx) args
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

    fun remove_bindings ctx ids =
      MarkerSet.foldl
        (fn (VAL id, ctx) =>
          remove_bound_id_base remove_val_scope_bound_id ctx id
        | (MOD id, ctx) =>
          remove_bound_id_base remove_mod_scope_bound_id ctx id
        )
        ctx
        ids

    fun get_sigval_bindings (_ : SMLSyntax.context) (Sigval {valspecs, modspecs, ...})=
      marker_set_of_list
        ( List.map VAL (SymDict.domain valspecs)
        @ List.map MOD (SymDict.domain modspecs)
        )

    and get_signat_bindings ctx signat =
      case signat of
        Sspec spec => get_spec_bindings ctx spec
      | Sident sym =>
          get_sigval_bindings ctx (get_sig ctx sym)
      | Swhere {signat, ...} => get_signat_bindings ctx signat

    and get_spec_bindings ctx spec =
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
          get_signat_bindings ctx signat
      | SPinclude_ids syms =>
          List.map (get_signat_bindings ctx) (List.map Sident syms)
          |> union_sets
      | SPsharing_type {spec, ...} => get_spec_bindings ctx spec
      | SPsharing {spec, ...} => get_spec_bindings ctx spec
      | SPseq specs =>
          List.map (get_spec_bindings ctx) specs
          |> union_sets

    fun of_funarg ctx funarg =
      case funarg of
        Normal {id, ...} => MarkerSet.singleton (MOD id)
      | Sugar spec => get_spec_bindings ctx spec

    fun of_modname ctx longid =
      let
        val (identdict, moddict) =
          case (get_module_opt ctx longid) of
            NONE => (SymDict.empty, SymDict.empty)
          | SOME scope => (scope_identdict scope, scope_moddict scope)
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
  end
