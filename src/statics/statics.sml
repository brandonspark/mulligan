(** Brandon Wu 
  *
  * Copyright (c) 2022-2023
  * See the file LICENSE for details.
  *)

structure Statics :
  sig
    val set_up_valbinds :
        SMLSyntax.context
     -> { tyvars : SMLSyntax.symbol list
        , valbinds : SMLSyntax.valbinds
        }
     -> SMLSyntax.context * SMLSyntax.context

    val set_up_fvalbinds :
        SMLSyntax.context
     -> { tyvars : SMLSyntax.symbol list
        , fvalbinds : SMLSyntax.fvalbinds
        }
     -> SMLSyntax.context

    datatype dec_status =
        DEC_VAL of Context.t * Context.t * {tyvars : SMLSyntax.symbol list, valbinds : SMLSyntax.valbinds}
      | DEC_FUN of Context.t * {tyvars : SMLSyntax.symbol list, fvalbinds : SMLSyntax.fvalbinds}
      | DEC_CTX of Context.t
      | DEC_CONT of
           Location.location list
         * (Location.location list -> SMLSyntax.dec -> Context.t -> Context.t)
         -> Context.t

    val synth :
        Context.t
     -> SMLSyntax.exp
     -> SMLSyntax.tyval

    val synth_dec :
        Context.t
     -> SMLSyntax.dec
     -> dec_status

    val ascribe :
        Context.t
     -> Context.scope
     -> { opacity : SMLSyntax.opacity, sigval : SMLSyntax.sigval} option
     -> Context.scope

    val nexpansive : Context.t -> SMLSyntax.exp -> bool

    exception Mismatch of string

    val apply_fn :
         {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list * Context.t * SMLSyntax.scope option
      -> Context.value
      -> SMLSyntax.tyval
      -> SMLSyntax.type_scheme AbsIdDict.dict
      -> (SMLSyntax.symbol * Context.value * SMLSyntax.tyval) list * Context.t * SMLSyntax.exp

    val match_against :
         Context.t
      -> {pat : SMLSyntax.pat, exp: SMLSyntax.exp} list
      -> Context.value
      -> SMLSyntax.tyval
      -> (SMLSyntax.symbol * Context.value * SMLSyntax.tyval) list * Context.t * SMLSyntax.exp

    val match_pat :
         Context.t
      -> SMLSyntax.pat
      -> Context.value
      -> SMLSyntax.tyval
      -> (SMLSyntax.symbol * Context.value * SMLSyntax.tyval) list

  end =
  struct
    open Common
    open SMLSyntax
    open Error
    open PrettyPrintAst

    val sym = Symbol.fromValue

    fun iter_list f z l =
      case l of
        [] => z
      | x::xs =>
          iter_list f (f (x, xs, z)) xs

    fun new () = TVvar (Ref.new NONE)

    val bool_ty = Basis.bool_ty

    fun mapi f l =
      List.foldl
        (fn (x, (i, acc)) =>
          (i + 1, f (x, i) :: acc)
        )
        (0, [])
        l
      |> (fn l => List.rev (#2 l))

    (* Unification of types requires checking that unification variables do not
     * occur in the type to be unified with (circularity). This function
     * facilitates that.
     *)
    fun occurs_check r tyval =
      case tyval of
        TVtyvar _ => false
      | TVvar (_, ref NONE) =>
          (* If r = r', then they actually both can unify, so this isn't an
           * issue.
           * So assume not occurring.
           *)
          false
      | ( TVapp (tyvals, _)
        | TVabs (tyvals, _)
        | TVprod tyvals ) =>
          List.foldl
            (fn (tyval, b) =>
              b orelse occurs_check r tyval
            )
            false
            tyvals
      | TVrecord fields =>
          List.foldl
            (fn ({tyval, ...}, b) =>
               b orelse occurs_check r tyval
            )
            false
            fields
      | TVvar (_, r' as ref (SOME (Rows fields))) =>
          r = r' orelse
          List.foldl
            (fn ({tyval, ...}, b) =>
               b orelse occurs_check r tyval
            )
            false
            fields
      | TVvar (_, r' as ref (SOME (Ty tyval))) =>
          r = r' orelse occurs_check r tyval
      | TVarrow (t1, t2) =>
          occurs_check r t1 orelse occurs_check r t2


    (* Non-expansive expressions refer to those expressions which are syntactic
     * values, i.e. do not evaluate significantly when being bound to an
     * identifier. Only non-expansive expressions may have their types
     * generalized at a binding site (due to unsafety with polymorphic references,
     * exceptions, and continuations), so this function just checks for that.
     *)
    fun nexpansive_left_app ctx exp =
      case exp of
        ( Eparens exp
        | Etyped {exp, ...} ) =>
          nexpansive_left_app ctx exp
      | Eident {id, ...} => Context.is_con ctx id
      | _ => false

    fun nexpansive ctx exp =
      case exp of
        ( Enumber _
        | Estring _
        | Echar _
        | Eident _
        ) => true
      | Erecord fields =>
          List.foldl
            (fn ({lab = _, exp}, acc) =>
              acc andalso
              nexpansive ctx exp
            )
            true
            fields
      | Eselect _ => false
      | Eunit => true
      | Etuple exps =>
          List.foldl
            (fn (exp, acc) => acc andalso nexpansive ctx exp)
            true
            exps
      | Elist exps =>
          List.foldl
            (fn (exp, acc) => acc andalso nexpansive ctx exp)
            true
            exps
      | Eparens exp => nexpansive ctx exp
      | Eapp {left, right} =>
          nexpansive ctx right andalso nexpansive_left_app ctx left
      | Einfix {left, right, id} =>
          Context.is_con ctx [id] andalso nexpansive ctx (Etuple [left, right])
      | Etyped {exp, ...} =>
          nexpansive ctx exp
      | Efn _ => true
      | ( Eseq _
        | Elet _
        | Eandalso _
        | Eorelse _
        | Ehandle _
        | Eraise _
        | Eif _
        | Ewhile _
        | Ecase _
        ) => false
      | Ehole => prog_err "should not happen, ehole in statics"

    (* This is so we can facilitate a little bit of code reuse between the
     * statics and the debugger.
     *
     * The debugger needs to do pretty much the exact same stuff as the statics
     * for a dec, plus a little bit more. This only happens in four cases
     * though, so instead the statics will do all the statics stuff, then
     * provide a value of this type, which the debugger will dispatch on to do
     * all the real-time stuff.
     *)
    datatype dec_status =
        DEC_VAL of Context.t * Context.t * {tyvars : SMLSyntax.symbol list, valbinds : SMLSyntax.valbinds}
      | DEC_FUN of Context.t * {tyvars : SMLSyntax.symbol list, fvalbinds : SMLSyntax.fvalbinds}
      | DEC_CTX of Context.t
      | DEC_CONT of
          Location.location list
        * (Location.location list -> SMLSyntax.dec -> Context.t -> Context.t)
        -> Context.t

    (* Need to replace tyvars with their substituted counterparts.
     *)
    fun quantify_tyval tyvar_fn ctx ty =
      let
        val quantify_tyval = fn ctx => fn ty => quantify_tyval tyvar_fn ctx ty
      in
        case (Context.norm_tyval ctx ty) of
          TVtyvar sym =>
            (case tyvar_fn (Proper sym) of
              NONE => TVtyvar sym
            | SOME tyval => tyval
            )
        | TVapp (tys, tyid) =>
            TVapp (List.map (quantify_tyval ctx) tys, tyid)
        | TVabs (tyvals, absid) =>
            TVabs (List.map (quantify_tyval ctx) tyvals, absid)
        | TVprod tys =>
            TVprod (List.map (quantify_tyval ctx) tys)
        | TVarrow (t1, t2) =>
            TVarrow (quantify_tyval ctx t1, quantify_tyval ctx t2)
        | TVrecord fields =>
            TVrecord
              (List.map (fn {lab, tyval} =>
                { lab = lab
                , tyval = quantify_tyval ctx tyval
                }) fields
              )
        | TVvar r =>
            (case tyvar_fn (Unconstrained r) of
              NONE => TVvar r
            | SOME tyval => tyval
            )
      end


    and norm ctx exp = Context.norm_tyval ctx (synth ctx exp)

    and fields_subset ctx fields1 fields2 =
      ( List.foldl
          (fn ({lab, tyval}, ()) =>
            ( case
                List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields2
              of
                NONE =>
                  spf
                    (`"Failed to unify record types, found unshared label "fi".")
                    lab
                  |> type_err
              | SOME {tyval = tyval', ...} => unify ctx tyval tyval'
            ; ()
            )
          )
          ()
          fields1
      ; ()
      )

    and unify ctx t1 t2 =
      ( case (Context.norm_tyval ctx t1, Context.norm_tyval ctx t2) of
        (TVvar (_, r as ref NONE), TVvar (_, r' as ref NONE)) =>
        if r = r' then t2
        else (r := SOME (Ty t2); t2)
      | (TVvar (_, r as ref NONE), t2) =>
          (if occurs_check r t2 then
             spf
               (`"Failed to unify, circularity in type "ftv"")
               t2
             |> type_err
           else
             ()
          ; r := SOME (Ty t2); t2)
      | (t1, TVvar (_, r as ref NONE)) =>
          (if occurs_check r t1 then
             spf
               (`"Failed to unify, circularity in type "ftv"")
               t1
             |> type_err
          else
            ()
         ; r := SOME (Ty t1); t1)
      | (TVvar (_, ref (SOME (Ty t1))), _) =>
          unify ctx t1 t2
      | (_, TVvar (_, ref (SOME (Ty t2)))) =>
          unify ctx t1 t2
      | ( TVvar (_, r1 as ref (SOME (Rows fields1)))
        , TVvar (_, r2 as ref (SOME (Rows fields2)))
        ) =>
          let
            val res =
              List.foldl
                (fn ({lab, tyval}, acc) =>
                  SymDict.insertMerge acc lab tyval (fn tyval' => unify ctx tyval tyval')
                )
                SymDict.empty
                (fields1 @ fields2)
              |> SymDict.toList
              |> List.map (fn (lab, tyval) => {lab=lab, tyval=tyval})
              |> TVrecord
          in
            ( r1 := SOME (Ty res)
            ; r2 := SOME (Ty res)
            ; res
            )
          end
      | (TVvar (_, r as ref (SOME (Rows rows_fields))), TVrecord record_fields) =>
          ( fields_subset ctx rows_fields record_fields
          ; r := SOME (Ty t2)
          ; t2
          )
      | (TVrecord record_fields, TVvar (_, r as ref (SOME (Rows rows_fields)))) =>
          ( fields_subset ctx rows_fields record_fields
          ; r := SOME (Ty t1)
          ; t1
          )
      | (TVrecord fields1, TVrecord fields2) =>
          ( fields_subset ctx fields1 fields2
          ; fields_subset ctx fields2 fields1
          ; t1
          )
      | (TVtyvar sym1, TVtyvar sym2) =>
          if Symbol.eq (sym1, sym2) then t2
          else
            spf
              (`"Failed to unify different tyvars "fi" and "fi"")
              sym1
              sym2
            |> type_err
      | (TVapp (tys1, id1), TVapp (tys2, id2)) =>
          ( List.map (fn (t1, t2) => unify ctx t1 t2) (ListPair.zipEq (tys1, tys2))
          ; if TyId.eq (id1, id2) then () else
              spf
                (`"Type mismatch between types "ftv" and "ftv"")
                t1 t2
              |> type_err
          ; t1
          )
      | (TVprod tys1, TVprod tys2) =>
          ( List.map (fn (t1, t2) => unify ctx t1 t2) (ListPair.zipEq (tys1, tys2))
          ; t1
          )
      | (TVarrow (t1', t2'), TVarrow (t1'', t2'')) =>
          ( unify ctx t1' t1''
          ; unify ctx t2' t2''
          ; t1
          )
      | (TVabs (tyvals, absid), TVabs (tyvals', absid')) =>
          ( List.map (fn (t1, t2) => unify ctx t1 t2) (ListPair.zipEq (tyvals, tyvals'))
          ; if AbsId.eq (absid, absid') then
              t2
            else
              raise Fail "TODO"
          )
      | _ =>
          spf
            (`"Failed to unify types "ftv" and "ftv"")
            t1
            t2
          |> type_err
      ) handle ListPair.UnequalLengths =>
          spf
            (`"Failed to unify types "ftv" and "ftv"")
            t1
            t2
          |> type_err

    (* TODO: extract errors *)
    and unify_row ctx (r : SMLSyntax.restrict option Ref.t) (sym, tyopt) =
      let
        val new =
          case tyopt of
            NONE => new ()
          | SOME ty => ty
      in
        case Ref.force r of
          NONE =>
            ( Ref.assign r (SOME (Rows [{lab = sym, tyval = new}]))
            ; new
            )
        | (SOME (Ty (TVrecord fields))) =>
            (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
              NONE =>
                spf
                  (`"Accessing nonexistent field "fi" in record type "ftv"")
                  sym
                  (TVrecord fields)
                |> type_err
            | SOME {lab = _, tyval} => tyval
            )
        | (SOME (Rows fields)) =>
            (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
              NONE =>
                ( Ref.assign r (SOME (Rows ({lab = sym, tyval = new} :: fields)))
                ; new
                )
            | SOME {lab = _, tyval} => tyval
            )
        | (SOME (Ty (TVprod tys))) =>
            (case Int.fromString (Symbol.toValue sym) of
              NONE =>
                spf
                  (`"Accessing non-numeral field "fi" in product type "ftv"")
                  sym
                  (TVprod tys)
                |> type_err
            | SOME i =>
                if i - 1 < List.length tys then
                  ( unify ctx (List.nth (tys, i - 1)) new
                  ; (List.nth (tys, i - 1))
                  )
                else
                  spf
                    (`"Accessing out-of-bounds numeral field "fi" in product type "ftv"")
                    sym
                    (TVprod tys)
                  |> type_err
            )
        | (SOME (Ty (TVvar r'))) =>
            unify_row ctx r' (sym, tyopt)
        | _ =>
            spf
              (`"Accessing field "fi" of non-record, non-product type")
              sym
            |> type_err
      end

    and synth ctx exp =
      case exp of
        Enumber (Int _) => Basis.int_ty
      | Enumber (Real _) => Basis.real_ty
      | Enumber (Word _) => Basis.int_ty
      | Estring _ => Basis.string_ty
      | Echar _ => Basis.char_ty
      | Erecord fields =>
          List.map
            (fn {lab, exp} =>
              { lab = lab
              , tyval = synth ctx exp
              }
            )
            fields
          |> TVrecord
      | Eselect _ => raise Fail "cannot synth eselect on its own"
      | Eunit => Basis.unit_ty
      | Eident {id, ...} =>
          (case Context.get_ident_ty_opt ctx id of
            NONE =>
              Printf.cprintf ctx
                (`"Cannot find type of value identifier "fli"")
                id
              |> prog_err
          | SOME (_, tyscheme) => instantiate_tyscheme ctx tyscheme
          )
      | Etuple exps =>
          List.map (synth ctx) exps
          |> TVprod
      | Elist exps =>
          List.foldl
            (fn (exp, ty) =>
              unify ctx ty (synth ctx exp)
            )
            (new ())
            exps
          |> (fn ty => TVapp ([ty], Basis.list_tyid))
      | Eseq exps =>
          List.foldl
            (fn (exp, _) =>
              synth ctx exp
            )
            (TVprod []) (* just need whatever here *)
            exps
      | Elet {dec, exps} =>
          let
            val ctx = statics_dec_status (synth_dec ctx dec)
          in
            synth ctx (case exps of [x] => x | _ => Eseq exps)
          end
      | Eparens exp => synth ctx exp
      | Eapp {left = Eselect sym, right} =>
          ( case (norm ctx right) of
              TVvar r => unify_row ctx r (sym, NONE)
            | TVprod tys =>
                (case Int.fromString (Symbol.toValue sym) of
                  NONE =>
                    Printf.cprintf ctx (`"Selecting non-numeral label "fi" from product type "ftv"")
                                                                      sym                 (TVprod tys)
                    |> type_err
                | SOME i =>
                    if i - 1 < List.length tys then
                      List.nth (tys, i - 1)
                    else
                      Printf.cprintf ctx (`"Selecting out-of-bounds numeral label "fi" from product type "ftv"")
                                                                                  sym                 (TVprod tys)
                      |> type_err
                )
            | TVrecord fields =>
                (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
                  NONE =>
                    Printf.cprintf ctx (`"Selecting nonexistent field "fi" in record type "ftv"")
                                                                      sym            (TVrecord fields)
                    |> type_err
                | SOME {lab, tyval} => tyval
                )
            | right_ty =>
                Printf.cprintf ctx (`"Selecting field "fi" from incompatible type "ftv"")
                                                      sym                        right_ty
                |> type_err
          )
      | Eapp {left, right} =>
          (case norm ctx left of
            TVvar (i, r as ref NONE) =>
              let
                val in_ty = synth ctx right
                val _ =
                  if occurs_check r in_ty then
                    Printf.cprintf ctx
                      (`"Impossible to synthesize type due to circularity in "ftv"")
                                                                             in_ty
                    |> type_err
                  else
                    ()
                val out_ty = new ()
              in
                Ref.assign (i, r) (SOME (Ty (TVarrow (in_ty, out_ty))));
                out_ty
              end
          | TVarrow (t1, t2) =>
              (unify ctx t1 (synth ctx right); t2)
          | left_ty =>
              Printf.cprintf ctx (`"Invalid type "ftv" for function application")
                                                left_ty
              |> type_err
          )
      | Einfix {left, id, right} =>
          (case norm ctx (Eident {opp = false, id = [id]}) of
            TVvar (_, r as ref NONE) =>
              let
                val out_ty = new ()
                val left_ty = synth ctx left
                val right_ty = synth ctx right
              in
                r := SOME (Ty (TVarrow (TVprod [left_ty, right_ty], out_ty)));
                out_ty
              end
          | TVarrow (t1, t2) =>
              (unify ctx t1 (TVprod [synth ctx left, synth ctx right]); t2)
          | infix_ty =>
              Printf.cprintf ctx (`"Invalid type "ftv" for infix function application")
                                                infix_ty
              |> type_err
          )
      | Etyped {exp, ty} =>
          unify
            ctx
            (synth ctx exp) (* TODO: check these functions *)
            (Context.synth_ty (fn _ => NONE) (fn _ => NONE) ctx ty)
      | ( Eandalso {left, right} | Eorelse {left, right} ) =>
          ( unify ctx (synth ctx left) bool_ty
          ; unify ctx (synth ctx right) bool_ty
          ; bool_ty
          )
      | Ehandle {exp, matches} =>
          List.foldl
            (fn ({pat, exp}, body_ty) =>
              let
                val (bindings, pat_ty) = synth_pat ctx pat
                (* These will be unquantified, since we cannot generalize
                 * polymorphic bindings at a handle site.
                 *)
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify ctx pat_ty Basis.exn_ty
                ; unify ctx (synth new_ctx exp) body_ty
                )
              end
            )
            (synth ctx exp)
            matches
      | Eraise exp =>
          ( unify ctx (synth ctx exp) Basis.exn_ty
          ; new ()
          )
      | Eif {exp1, exp2, exp3} =>
          ( unify ctx (synth ctx exp1) bool_ty
          ; unify ctx (synth ctx exp2) (synth ctx exp3)
          )
      | Ewhile {exp1, exp2 = _} =>
          ( unify ctx (synth ctx exp1) bool_ty
          ; Basis.unit_ty
          )
      | Ecase {exp, matches} =>
          List.foldl
            (fn ({pat, exp}, (pat_ty, body_ty)) =>
              let
                val (bindings, pat_ty') = synth_pat ctx pat
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify ctx pat_ty' pat_ty
                , unify new_ctx (synth new_ctx exp) body_ty
                )
              end
            )
            (synth ctx exp, new ())
            matches
          |> snd 
      | Efn (matches, _) =>
          List.foldl
            (fn ({pat, exp}, (pat_ty, body_ty)) =>
              let
                val (bindings, pat_ty') = synth_pat ctx pat
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify ctx pat_ty' pat_ty
                , unify new_ctx (synth new_ctx exp) body_ty
                )
              end
            )
            (new (), new ())
            matches
          |> TVarrow
      | Ehole => raise Fail "shouldn't happen"

    and quantify_binding ((id, tyval), is_nexpansive) ctx =
      (* An expansive expression cannot be generalized at its binding site.
       * The NJ way to deal with this is to generate a new type no one has ever
       * heard of before, and assign it to that.
       * However, that's somewhat more annoying, and also MLton will just let
       * the unification variable be specified later.
       * So we'll take the MLton approach.
       *)
      if not is_nexpansive then
        ( id
        , guard_tyscheme
            (0, fn _ => Context.norm_tyval ctx tyval)
        )
      else
        let
          val tyval = Context.norm_tyval ctx tyval

          (* We want to talk about all the unification vars that have been introduced in
           * this type, that have no relation to any unification vars that have
           * been seen before.
           * Somewhat key: This means we have no choice really but to crawl
           * through the context, as a unification var might be spawned at any
           * time by relying on a value which has already been bound in the
           * context.
           * So we'll do the search.
           *)
          val quantified_tyvars =
            let
              fun tyvars_remove l tyvar =
                List.foldr
                  (fn (tyvar', acc) =>
                    if tyvar_eq (tyvar, tyvar') then
                      acc
                    else
                      tyvar' :: acc
                  )
                  []
                  l

              fun tyvars_difference l1 l2 =
                List.foldr
                  (fn (tyvar, acc) =>
                    tyvars_remove acc tyvar
                  )
                  l1
                  l2

              (* This is big quadratic complexity, but we're not using sets because
               * we want the tyvars to be numbered in a "sensible" ordering. This
               * means that we want the order.
               *
               * There shouldn't be that many tyvars anyways. So it should be fine.
               *)
              fun tyvars_dedup l =
                case l of
                  [] => []
                | x::xs => x :: tyvars_dedup (tyvars_remove xs x)

              (* These are all of the tyvars that are within our current binding
               * tyval.
               *)
              val tyvars = CollectTyvars.collect_tyvars_tyval tyval
              val current_tyvars = Context.get_current_tyvars CollectTyvars.collect_tyvars_tyval ctx
            in
              tyvars_dedup (tyvars_difference tyvars current_tyvars)
            end

          (* This leaves behind any implicit tyvars within the current
           * binding that have nothing to do with the outer context, as
           * well as any explicit tyvars introduced within this binding.
           *)
          val num_quantifiers = List.length quantified_tyvars

          fun type_scheme tys =
            let
              val paired_tys =
                ListPair.zipEq (quantified_tyvars, tys)

              (* This search function, given a tyvar to replace, finds the
               * tyvar that replaces it.
               *)
              val search_fn =
                fn tyvar =>
                  case
                    List.find (fn (tyvar', _) => tyvar_eq (tyvar, tyvar')) paired_tys
                  of
                    NONE => NONE
                  | SOME (_, ty) => SOME ty
            in
              quantify_tyval search_fn ctx tyval
            end
        in
          (id, (num_quantifiers, type_scheme))
        end

    and set_up_valbinds orig_ctx {tyvars, valbinds} =
      let
        (* These are all of the tyvars that are scoped at this val
         * declaration.
         *)
        val tyvars =
          SymSet.union
            (set_from_list tyvars)
            ( List.map
                (fn {pat, exp, ...} =>
                  SymSet.union
                    (CollectTyvars.collect_tyvars_pat pat)
                    (CollectTyvars.collect_tyvars exp)
                )
                valbinds
              |> union_sets
            )

        val ctx =
          Context.add_scoped_tyvars orig_ctx tyvars

        (* True if any of the declarations are `rec`
         *)
        val is_rec =
          List.foldl
            (fn ({recc, ...}, acc) =>
              recc orelse acc
            )
            false
            valbinds

        (* Get all the bindings from each pattern, along with the type that
         * that the pat seems to be.
         *)
        val (bindings, pat_tys) =
          List.foldr
            (fn ((pat, exp), (bindings, pat_tys)) =>
              let
                val (bindings', pat_ty) = synth_pat ctx pat
                val bindings_expansive =
                  List.map
                    (fn binding => (binding, nexpansive ctx exp))
                    bindings'
              in
                (bindings @ bindings_expansive, pat_ty :: pat_tys)
              end
            )
            ([], [])
            (List.map (fn {pat, exp, ...} => (pat, exp)) valbinds)

        (* If this is a recursive val binding, then we should evaluate the
         * type of the expression in the context of every mutually recursive
         * val declaration.
         * Otherwise, evaluate it in the original context.
         *)
        (* TODO: Apparently, these val bindings should see each other in an
         * "unquantified" manner.
         *)
        val exp_ctx =
          if is_rec then
            Context.add_unquantified_val_ty_bindings ctx (List.map #1 bindings)
          else
            ctx

        (* Check that each purported pat type unifies with the expression's
         * type.
         *)
        val () =
          List.foldr
            (fn (({exp, ...}, pat_ty), ()) =>
              ( unify ctx (synth exp_ctx exp) pat_ty
              ; ()
              )
            )
            ()
            (ListPair.zipEq (valbinds, pat_tys))

        val quantified_bindings =
          List.map
            (fn (id, tyval) => quantify_binding (id, tyval) orig_ctx)
            bindings

      in
        ( Context.add_val_ty_bindings orig_ctx quantified_bindings
        , exp_ctx
        )
      end

    and set_up_fvalbinds orig_ctx {tyvars, fvalbinds} =
      let
        (* These are all of the tyvars that are scoped at this fun
         * declaration.
         *)
        val tyvars =
          SymSet.union
            (set_from_list tyvars)
            ( List.map
                (fn fvalbind =>
                  List.map
                    (fn {fname_args, ty, exp} =>
                      union_three
                        (CollectTyvars.collect_tyvars_fname_args fname_args)
                        (case ty of
                          NONE => SymSet.empty
                        | SOME ty => CollectTyvars.collect_tyvars_ty ty)
                        (CollectTyvars.collect_tyvars exp)
                    )
                    fvalbind
                  |> union_sets
                )
                fvalbinds
              |> union_sets
            )

        val ctx =
          Context.add_scoped_tyvars orig_ctx tyvars

        (* Get the essential information from each fname_args.
         * We need the function name, pat bindings, and types of the patterns.
         *)
        fun get_fname_args_info fname_args ctx =
          let
            fun get_pat_list_ty_bindings ctx pats =
              List.foldr
                (fn (pat, (bindings, pat_tys)) =>
                  let
                    val (bindings', pat_ty) = synth_pat ctx pat
                  in
                    (bindings @ bindings', pat_ty :: pat_tys)
                  end
                )
                ([], [])
                pats
          in
            case fname_args of
              Fprefix {id, args, ...} =>
                get_pat_list_ty_bindings ctx args
                |> (fn (bindings, ty) => (id, bindings, ty))
            | Finfix {left, id, right} =>
                let
                  val (left_bindings, left_pat_ty) =
                    synth_pat ctx left
                  val (right_bindings, right_pat_ty) =
                    synth_pat ctx right
                in
                  (id, left_bindings @ right_bindings, [TVprod [left_pat_ty, right_pat_ty]])
                end
            | Fcurried_infix {left, right, args, id} =>
                let
                  val (left_bindings, left_pat_ty) =
                    synth_pat ctx left
                  val (right_bindings, right_pat_ty) =
                    synth_pat ctx right
                  val (rest_bindings, rest_pat_tys) =
                    get_pat_list_ty_bindings ctx args
                in
                  ( id
                  , left_bindings @ right_bindings @ rest_bindings
                  , TVprod [left_pat_ty, right_pat_ty] :: rest_pat_tys
                  )
                end
          end


        (* This context has all of the mutually recursive function names
         * added into the context.
         * We will use this to evaluate each individual function.
         *)
        val (fn_bindings, fn_data, new_ctx) =
          List.foldr
            (fn (fvalbind, (acc_bindings, fn_data, new_ctx)) =>
              let
                (* This is the actual output type of this function.
                 * We will enforce constraints on it as we go.
                 *
                 *)
                val out_ty = new ()

                fun some_unify tyopt =
                  case tyopt of
                    NONE => ()
                  | SOME ty =>
                      ( unify
                          ctx
                          out_ty
                          (Context.synth_ty' ctx ty)
                      ; ()
                      )
              in
                (* Make sure that the names for each function clause match.
                 * Also, ensure that their purported out types unify.
                 *)
                List.foldr
                  (fn ({fname_args, ty, exp}, acc) =>
                    let
                      val (id', bindings', pat_tys') = get_fname_args_info fname_args ctx

                      (* If there is a type annotation, then we should unify it
                       * with our prospective output type.
                       *)
                      val _ = some_unify ty
                      val id =
                        case acc of
                          NONE => id'
                        | SOME (id, _, _, _) =>
                          if Symbol.eq (id, id') then id
                          else
                            (* TODO: extract *)
                            spf
                              (`"Function clauses have different names "fi" and "fi"")
                              id
                              id'
                            |> prog_err
                    in
                      (* For each function clause, we're accumulating that
                       * clause's bindings and body expressions.
                       *
                       * Overall, we're also ensuring that the name of the
                       * function and the types of the patterns of its
                       * curried arguments still match.
                       *)
                      case acc of
                        NONE => SOME (id, [bindings'], pat_tys', [exp])
                      | SOME (_, bindings, pat_tys, exps) =>
                          ( List.map
                              (fn (pat_ty1, pat_ty2) => unify ctx pat_ty1 pat_ty2)
                              (ListPair.zipEq (pat_tys, pat_tys'))
                          ; SOME (id, bindings' :: bindings, pat_tys, exp :: exps)
                          )
                    end
                  )
                  NONE
                  fvalbind
                |> (fn NONE => raise Fail "shouldn't happen"
                   | SOME (id, bindings, pat_tys, exps) =>
                     let
                       val fn_binding =
                          ( id
                          , List.foldr
                              (fn (pat_ty, acc) =>
                               TVarrow (pat_ty, acc)
                              )
                              out_ty
                              pat_tys
                          )
                     in
                       (* Here, we want to keep
                        * 1) the binding of each function
                        * 2) the overall information of each function
                        * 3) the context when adding that function binding to it
                        *)
                       ( fn_binding :: acc_bindings
                       , (id, bindings, pat_tys, exps, out_ty) :: fn_data
                       , Context.add_unquantified_val_ty_bindings new_ctx [fn_binding]
                       )
                     end
                   )
              end
            )
            ([], [], ctx)
            fvalbinds

        (* This function just goes and makes sure that each expression's
         * type (in the context of its argument bindings) unifies with every
         * other clause's type.
         *)
        fun validate_clauses fn_data =
          List.foldl
            (* THINK: Verify the `id` and `pat_tys` are not needed here. 
             *)
            (fn ((_, bindings, _, exps, out_ty), ()) =>
              List.foldl
                (fn ((bindings, exp), ()) =>
                  let
                    (* Add each clause's patterns into it, then unify the output
                     * types with the synthesized type of that clause's body.
                     *)
                    val new_ctx =
                      Context.add_unquantified_val_ty_bindings new_ctx bindings
                  in
                    ( unify
                        ctx
                        (synth new_ctx exp)
                        out_ty
                    ; ()
                    )
                  end
                )
                ()
                (ListPair.zipEq (bindings, exps))
            )
            ()
            fn_data

        val _ = validate_clauses fn_data

        (* Set all the new tyvars to quantified variables.
         *)
        val quantified_bindings =
          List.map
            (fn binding => quantify_binding binding orig_ctx)
            (List.map (fn binding => (binding, true)) fn_bindings)
      in
        Context.add_val_ty_bindings orig_ctx quantified_bindings
      end

    and instantiate_tyscheme ctx (arity, ty_fn) =
      Context.norm_tyval ctx (ty_fn (List.tabulate (arity, fn _ => new ())))

    and synth_pat ctx pat =
      case pat of
        Pnumber _ => ([], Basis.int_ty)
      | Pword _ => ([], Basis.int_ty)
      | Pstring _ => ([], Basis.string_ty)
      | Pchar _ => ([], Basis.char_ty)
      | Pwild => ([], new ())
      | Pident {id, ...} =>
          ( case (Context.get_ident_ty_opt ctx id, id) of
              (SOME ((Csign | Esign), tyscheme), _) =>
                ([], instantiate_tyscheme ctx tyscheme)
            | ((SOME (Vsign, _) | NONE), [x]) =>
                let
                  val var = new ()
                in
                  ([(x, var)], var)
                end
            | _ => raise Fail "impossible"
          )
      | Precord patrows =>
          let
            val row_ref : SMLSyntax.restrict option Ref.t = Ref.new NONE
          in
            List.foldr
              (fn (patrow, (bindings, has_ellipses)) =>
                case patrow of
                  PRellipsis => (bindings, true)
                | PRlab {lab, pat} =>
                    let
                      val (bindings', ty) = synth_pat ctx pat
                      val _ = unify_row ctx row_ref (lab, SOME ty)
                    in
                      (bindings @ bindings', has_ellipses)
                    end
                | PRas {id, ty, aspat} =>
                    let
                      val var = new ()
                      val (bindings, ty) =
                        case (ty, aspat) of
                          (NONE, NONE) =>
                            ((id, var) :: bindings, var)
                        | (SOME ty, NONE) =>
                            let
                              val tyval = Context.synth_ty' ctx ty
                            in
                              ((id, tyval) :: bindings, tyval)
                            end
                        | (_, SOME pat) =>
                            let
                              val (bindings', ty') = synth_pat ctx pat
                              val _ =
                                case ty of
                                  NONE => ()
                                | SOME ty'' => (unify ctx ty' (Context.synth_ty' ctx ty''); ())
                            in
                              ((id, ty') :: bindings @ bindings', ty')
                            end
                    in
                      ( unify_row ctx row_ref (id, SOME ty)
                      ; (bindings, has_ellipses)
                      )
                    end
              )
              ([], false)
              patrows
            |> (fn (bindings, has_ellipses) =>
                (* If this is an incomplete record pattern, we can't assert that
                 * we definitely know what the type is.
                 *
                 * So then we just inject it into the Rows variant, and collect
                 * more constraints down the line.
                 *)
                if has_ellipses then
                  (bindings, TVvar row_ref)
                else
                  ( bindings
                  , case row_ref of
                      (_, ref (SOME (Rows fields))) =>
                        ( Ref.assign row_ref (SOME (Ty (TVrecord fields)))
                        ; TVvar row_ref
                        )
                    | _ => raise Fail "impossible"
                  )
               )
          end
      | Pparens pat => synth_pat ctx pat
      | Punit => ([], Basis.unit_ty)
      | Ptuple pats =>
          List.foldr
            (fn (pat, (bindings, tys)) =>
              let
                val (bindings', ty) = synth_pat ctx pat
              in
                (bindings @ bindings', ty::tys)
              end
            )
            ([], [])
            pats
          |> (fn (bindings, tys) => (bindings, TVprod tys))
      | Plist pats =>
          List.foldr
            (fn (pat, (bindings, ty)) =>
              let
                val (bindings', ty') = synth_pat ctx pat
              in
                (bindings @ bindings', TVapp ([unify ctx ty ty'], Basis.list_tyid))
              end
            )
            ([], new ())
            pats
      | Por pats =>
          ( case
              List.foldr
                (fn (pat, NONE) => SOME (synth_pat ctx pat)
                | (pat, SOME (bindings, ty)) =>
                  let
                    (* Need to make sure each or-pattern has the same bindings,
                     * for the same types.
                     *)
                    fun assert_one_sided ctx bindings1 bindings2 =
                      List.foldl
                        (fn ((id, ty), ()) =>
                          case List.find (fn (id', _) => Symbol.eq (id, id')) bindings2 of
                            NONE =>
                              Printf.cprintf ctx
                                (`"Bindings not the same in all clauses in or-pattern "fp"")
                                (Por pats)
                              |> prog_err
                          | SOME (_, ty') => ( unify ctx ty ty'; () )
                        )
                        ()
                        bindings1

                    fun assert_bindings_same ctx bindings1 bindings2 =
                      ( assert_one_sided ctx bindings1 bindings2
                      ; assert_one_sided ctx bindings2 bindings1
                      )

                    val (bindings', ty') = synth_pat ctx pat
                    val _ = assert_bindings_same ctx bindings bindings'
                  in
                    SOME (bindings, unify ctx ty ty')
                  end
                )
                NONE
                pats
            of
              NONE => raise Fail "impossible, empty por"
            | SOME ans => ans
          )
      | Papp {id, atpat, ...} =>
          ( case (Context.get_ident_ty_opt ctx id) of
              NONE =>
                Printf.cprintf ctx
                  (`"Cannot find type of applied pattern identifier "fli"")
                  id
                |> prog_err
            | SOME ((Esign | Csign), tyscheme) =>
                let
                  val (bindings, ty'') = synth_pat ctx atpat
                in
                  ( case instantiate_tyscheme ctx tyscheme of
                      TVarrow (t1, t2) =>
                        ( unify ctx t1 ty''
                        ; (bindings, t2)
                        )
                    | _ => raise Fail "prob shouldn't happen"
                  )
                end
            | SOME (Vsign, _) =>
                Printf.cprintf ctx
                  (`"Applied pattern identifier "fli" is a non-constructor.")
                  id
                |> prog_err
          )
      | Pinfix {left, id, right} =>
          ( case Context.get_ident_ty_opt ctx [id] of
              NONE =>
                Printf.cprintf ctx
                  (`"Infix identifier "fi" has no type.")
                  id
                |> prog_err
            | SOME ((Csign | Esign), tyscheme) =>
                (case instantiate_tyscheme ctx tyscheme of
                  TVarrow (t1, t2) =>
                    let
                      val (bindings, ty'') = synth_pat ctx left
                      val (bindings', ty''') = synth_pat ctx right
                    in
                      ( unify ctx t1 (TVprod [ty'', ty'''])
                      ; (bindings @ bindings', t2)
                      )
                    end
                | _ => raise Fail "should not happen"
                )
            | SOME (Vsign, _) =>
                Printf.cprintf ctx
                  (`"Infix pattern identifier "fi" is a non-constructor.")
                  id
                |> prog_err
          )
      | Ptyped {pat, ty} =>
          let
            val (bindings, ty') = synth_pat ctx pat
            val tyval = Context.synth_ty' ctx ty
          in
            (bindings, unify ctx tyval ty')
          end
      | Playered {id, ty, aspat, ...} =>
          let
            val (bindings, ty') = synth_pat ctx aspat
            val tyval =
              case ty of
                SOME ty => unify ctx ty' (Context.synth_ty' ctx ty)
              | NONE => ty'
          in
            ((id, tyval) :: bindings, tyval)
          end

    (* When only checking the statics, there isn't much left to do after calling
     * `synth_dec`.
     *)
    and statics_dec_status dec_status =
      case dec_status of
        DEC_VAL (new_ctx, _, _) => new_ctx
      | DEC_FUN (ctx, _) => ctx
      | DEC_CTX ctx => ctx
      | DEC_CONT f =>
          let
            fun funarg _ dec ctx =
              statics_dec_status (synth_dec ctx dec)
          in
            f ([], funarg)
          end

    and synth_dec ctx dec =
      case dec of
        Dval valbinds =>
          let
            val (new_ctx, exp_ctx) = set_up_valbinds ctx valbinds
          in
            DEC_VAL (new_ctx, exp_ctx, valbinds)
          end
      | Dfun fvalbinds =>
          DEC_FUN (set_up_fvalbinds ctx fvalbinds, fvalbinds)
      | Dtype typbinds =>
          List.map
            (fn {tycon, tyvars, ty} =>
              ( tycon
              , Scheme (Context.mk_type_scheme (fn _ => NONE) tyvars ty ctx)
              )
            )
            typbinds
          |> List.foldl
               (fn ((id, tyscheme), ctx) =>
                 Context.add_type_synonym ctx id tyscheme
               ) ctx
          |> DEC_CTX
      | Ddatdec {datbinds, withtypee} =>
          let
            val enum_datbinds =
              List.map (fn datbind as {tycon, ...} => (datbind, TyId.new (SOME tycon))) datbinds

            val withtypee_bindings =
              case withtypee of
                NONE => []
              | SOME withtypee =>
                List.map
                  (fn {tyvars, tycon, ty} =>
                    let
                      val num = List.length tyvars

                      (* This function takes in a datatype name, and checks
                       * whether it is the same as the name of these datatypes.
                       * If so, it extracts it into a tyval.
                       *)
                      val datatype_fn =
                        fn (sym, tyvals) =>
                          List.find (fn ({tycon, ...}, _) => Symbol.eq (tycon, sym)) enum_datbinds
                          |> Option.map (fn (_, id) => TVapp (tyvals, id))

                      fun mk_tyvar_fn tys =
                        let
                          val paired = ListPair.zipEq (tyvars, tys)
                        in
                          fn sym =>
                              (* This is the index in the original tyvars list
                               * that the input symbol is.
                               *)
                            case List.find (fn (tyvar, _) => Symbol.eq (sym, tyvar)) paired of
                              NONE => NONE
                            | SOME (_, ty) => SOME ty
                        end

                      fun mk_type_scheme tys =
                        if List.length tys <> num then
                          Printf.cprintf ctx
                            (`"Arity mismatch when instantiating type scheme.")
                          |> type_err
                        else
                          Context.synth_ty datatype_fn (mk_tyvar_fn tys) ctx ty
                    in
                      (* The name of this type maps to the type scheme which
                       * abstracts over the number of type arguments to the type,
                       * but reconstruct the proper type when given them.
                       * It also stores the arity of the type.
                       *)
                      (tycon, (num, mk_type_scheme))
                    end
                  )
                  withtypee

            val ctx =
              List.foldl
                (fn ((id, tyscheme), ctx) =>
                  Context.add_type_synonym ctx id (Scheme tyscheme)
                )
                ctx
                withtypee_bindings

            (* add_datbind is responsible for adding the constructors to the
             * identdict and valtydict.
             *)
            val ctx =
              List.foldl
                (fn ((datbind, tyid), ctx) =>
                  Context.add_datbind (fn (sym, tyvals) =>
                    List.find (fn ({tycon, ...}, _) => Symbol.eq (tycon, sym)) enum_datbinds
                    |> Option.map (fn (_, tyid) => TVapp (tyvals, tyid))
                  ) ctx (tyid, datbind)
                )
                ctx
                enum_datbinds
          in
            DEC_CTX ctx
          end
      | Ddatrepl {left_tycon, right_tycon} =>
          DEC_CTX (Context.replicate_datatype ctx (left_tycon, right_tycon))
      | Dabstype _ => raise Fail "no support for abstype right now"
      | Dexception exbinds =>
          List.foldl
            (fn (exbind, new_ctx) =>
              case exbind of
                Xnew {id, ty, ...} =>
                  Context.add_exn new_ctx (ExnId.new (SOME id))
                    ( id
                    , case ty of
                        NONE =>
                          concrete_tyscheme Basis.exn_ty 
                      | SOME ty =>
                          concrete_tyscheme (TVarrow (Context.synth_ty' ctx ty, Basis.exn_ty))
                    )
              | Xrepl {left_id, right_id, ...} =>
                  ( case Context.get_exn_opt ctx right_id of
                      NONE =>
                        Printf.cprintf ctx (`"Replicating nonexistent exception "fli".")
                                                                               right_id
                        |> prog_err
                    | SOME exn_id =>
                      Context.add_exn
                        new_ctx
                        exn_id
                        ( left_id
                        , case Context.get_ident_ty ctx right_id of
                            (Esign, tyscheme) => tyscheme
                          | _ =>
                            Printf.cprintf ctx (`"Replicating non-exception identifier "fli".")
                                                                                      right_id
                            |> prog_err
                        )
                  )
            )
            ctx
            exbinds
          |> DEC_CTX
      | Dlocal {left_dec, right_dec} =>
          (* Make a temporary scope to evaluate all the bindings in the `local`.
           * Then, make a scope for all of the actual `in` decs.
           * Then pop the penultimate scope to get rid of the temporary
           * bindings.
           *)
          DEC_CONT
            ( fn (location, f) =>
              ( case left_dec of
                  Dseq decs =>
                    iter_list
                      (fn (dec, decs, ctx) =>
                        f (Location.DLOCAL (decs, right_dec) :: Location.CLOSURE ctx :: location) dec ctx
                      )
                      (Context.enter_scope ctx)
                      decs
                    |> Context.enter_scope
                    |> (fn ctx' => f (Location.CLOSURE ctx :: location) right_dec ctx')
                    |> Context.exit_local
                | _ =>
                    f
                      (Location.DLOCAL ([], right_dec) :: Location.CLOSURE ctx :: location)
                      left_dec
                      (Context.enter_scope ctx)
                    |> Context.enter_scope
                    |> (fn ctx' => f (Location.CLOSURE ctx :: location) right_dec ctx')
                    |> Context.exit_local
              )
            )
      | Dopen ids =>
          List.foldl
            (fn (id, ctx) => Context.open_path ctx id)
            ctx
            ids
          |> DEC_CTX
      | Dinfix {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id, LEFT, Option.getOpt (precedence, 0) )
            )
            ctx
            ids
          |> DEC_CTX
      | Dinfixr {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id, RIGHT, Option.getOpt (precedence, 0) )
            )
            ctx
            ids
          |> DEC_CTX
      | Dnonfix ids =>
          List.foldl
            (fn (id, ctx) =>
              Context.remove_infix ctx id
            )
            ctx
            ids
          |> DEC_CTX
      | Dseq decs =>
          DEC_CONT
            (fn (location, f) =>
              iter_list
                (fn (dec, decs, ctx) =>
                  f (Location.DSEQ decs :: location) dec ctx
                )
                ctx
                decs
            )
      | Dhole => raise Fail "shouldn't eval dhole"

    exception Mismatch of string

    fun match_pat ctx pat v tyval =
      ( case (pat, v, Context.norm_tyval ctx tyval) of
          (Pnumber i1, Vnumber (Int i2), _) =>
            if i1 = i2 then
              (unify ctx tyval Basis.int_ty; [])
            else
              raise Mismatch "num pats did not match"
        | (Pword s, Vnumber (Word s'), _) =>
            if Symbol.toValue s = s' then
              (* TODO: words *) []
            else
              raise Mismatch "word pats did not match"
        | (Pstring s, Vstring s', _) =>
            if Symbol.eq (s, s') then
              (unify ctx tyval Basis.string_ty; [])
            else
              raise Mismatch "string pats did not match"
        | (Pchar c, Vchar c', _) =>
            if c = c' then
              (unify ctx tyval Basis.char_ty; [])
            else
              raise Mismatch "char pats did not match"
        | (Pwild, _, _) => []
          (* TODO: check types *)
        | (Pident {opp = _, id}, _, _) =>
            (case (Context.get_ident_opt ctx id, v) of
              (SOME (C _), Vconstr { id = id', arg = NONE}) =>
                if longid_eq (id, id') then
                  []
                else
                  raise Mismatch "constructors did not match"
            | (SOME (E exnid'), Vexn {name = _, exnid, arg = NONE}) =>
                if ExnId.eq (exnid, exnid') then
                  []
                else
                  raise Mismatch "exceptions did not match"
            | (SOME (V _) | NONE, _) =>
                (case id of
                  [id] => [(id, v, tyval)]
                | _ => raise Mismatch "pattern matching against nonexistent constr"
                )
            | _ => raise Mismatch "invalid match"
            )
        | (Precord patrows, Vrecord fields, _) =>
            let
              fun to_tyfields tyval =
                case Context.norm_tyval ctx tyval of
                  TVprod tyvals =>
                    (mapi (fn (tyval, i) => { lab = Symbol.fromValue (Int.toString i)
                                     , tyval = tyval
                                     }
                          )
                          tyvals
                    )
                | TVrecord fields => fields
                | _ => raise Fail "TODO"

              fun unite fields tyfields =
                List.foldl
                  (fn ({lab, value}, acc) =>
                    case List.find (fn {lab = lab', tyval = _} => Symbol.eq (lab, lab'))
                      tyfields
                    of
                      NONE => raise Fail "failed to find label in tyfield"
                    | SOME {tyval, ...} =>
                        { lab = lab, value = value, tyval = tyval } :: acc
                  )
                  []
                  fields

              val combined = unite fields (to_tyfields tyval)
            in
              List.foldl
                (fn (patrow, acc) =>
                  case patrow of
                    PRellipsis => acc
                  | PRlab {lab, pat} =>
                      (case
                        List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) combined
                      of
                        NONE => raise Mismatch "val did not match patrow"
                      | SOME {lab = _, value, tyval} => match_pat ctx pat value tyval @ acc
                      )
                  | PRas {id, ty, aspat} =>
                      (case
                        List.find (fn {lab, ...} => Symbol.eq (lab, id)) combined
                      of
                        NONE => raise Mismatch "val did not match patrow"
                      | SOME {lab = _, value, tyval} =>
                          let
                            fun some_unify tyopt =
                              case tyopt of
                                NONE => tyval
                              | SOME ty => unify ctx tyval (Context.synth_ty' ctx ty)
                          in
                            case aspat of
                              NONE => (id, value, some_unify ty) :: acc
                            | SOME pat => (id, value, some_unify ty) :: match_pat ctx pat value (some_unify ty) @ acc
                          end
                      )
                )
                []
                patrows
            end
        | (Pparens pat, v, _) => match_pat ctx pat v tyval
        | (Punit, Vunit, _) =>
            (unify ctx tyval Basis.unit_ty; [])
        | (Ptuple pats, Vtuple vals, _) =>
            let
              fun to_tuple tyval =
                case Context.norm_tyval ctx tyval of
                  TVrecord fields =>
                    raise Fail "TODO: change record to tuple"
                | TVprod tyvals => tyvals
                | _ => raise Fail "TODO"

              (* May need to change a record type.
               *)
              val tuple_ty = to_tuple tyval
            in
              ListPair.zipEq (ListPair.zipEq (pats, vals), tuple_ty)
              |> (List.concat o List.map (fn ((pat, value), tyval) => match_pat ctx pat value tyval))
            end
        | (Plist pats, Vlist vals, TVapp ([tyval_arg], tyid)) =>
            if TyId.eq (tyid, Basis.list_tyid) then
              ListPair.zipEq (pats, vals)
              |> (List.concat o List.map (fn (pat, value) => match_pat ctx pat value tyval_arg))
            else
              raise Fail "TODO"
        | (Por pats, _, _) =>
            ( case
                List.foldl
                  (fn (pat, NONE) =>
                    ( SOME (match_pat ctx pat v tyval)
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
        | ( Papp {opp = _, id, atpat}
          , _
          , _
          ) =>
          ( case (Context.get_ident_ty_opt ctx id, v, tyval, Context.get_exn_opt ctx id) of
              ( SOME (Csign, tyscheme)
              , Vconstr {id = id', arg = SOME v}
              , TVapp (tyvals, tyid)
              , _
              ) =>
                (* If the constructors match, then... *)
                if longid_eq (id, id') then
                  (* Verify that their types match. *)
                  (case instantiate_tyscheme ctx tyscheme of
                    TVarrow (in_ty, TVapp (tyvals', tyid')) =>
                      if TyId.eq (tyid, tyid') then
                        ( List.map (fn (x, y) => unify ctx x y) (ListPair.zipEq (tyvals, tyvals'))
                        ; match_pat ctx atpat v in_ty
                        )
                      else
                        raise Fail "TODO"
                  | _ => raise Fail "TODO"
                  )
                else
                  raise Mismatch "constructors not same"
            | ( SOME (Esign, tyscheme)
              , Vexn {name = _, exnid, arg = SOME v}
              , TVapp ([], tyid)
              , SOME exnid'
              ) =>
                if TyId.eq (Basis.exn_tyid, tyid) then
                  if ExnId.eq (exnid, exnid') then
                    (case instantiate_tyscheme ctx tyscheme of
                      TVarrow (in_ty, TVapp ([], tyid'')) =>
                        if TyId.eq (Basis.exn_tyid, tyid'') then
                          match_pat ctx atpat v in_ty
                        else
                          prog_err "Esign has tyscheme that doesn't result in exn"
                    | _ =>
                        prog_err "applied exn constructor of incorrect type"
                    )
                  else
                    raise Mismatch "exns did not match"
                else
                  prog_err "matching non-exn tyval to exn pattern"
            | _ => prog_err "non-csign or esign given to applied pattern"
          )
          (* TODO: This technically does not comply with the Definition.
           * If you shadow cons, pattern matching against cons is no longer
           * supposed to work.
           *)
        | ( Pinfix {left, id, right}
          , Vlist values
          , TVapp ([tyval], tyid)
          ) =>
          if TyId.eq (tyid, Basis.list_tyid) andalso Symbol.eq (id, Symbol.fromValue "::") then
            case values of
              [] => raise Mismatch "failed to match cons pattern to empty list"
            | hd::tl =>
                  match_pat ctx left hd tyval
                @ match_pat ctx right (Vlist tl) (TVapp ([tyval], tyid))
          else
            prog_err "TODO"
        (* TODO: match non-infix exn against pinfix? *)
        | ( Pinfix {left, id, right}
          , Vinfix {left = left', id = id', right = right'}
          , TVapp (tyvals, tyid)
          ) =>
            (* TODO: constructors need more than just name equality *)
            if Symbol.eq (id, id') then
              case Context.get_ident_ty_opt ctx [id] of
                SOME (Csign, tyscheme) =>
                  (case instantiate_tyscheme ctx tyscheme of
                    TVarrow (TVprod [left_ty, right_ty], TVapp (tyvals', tyid')) =>
                      if TyId.eq (tyid, tyid') then
                        ( List.map (fn (x, y) => unify ctx x y)  (ListPair.zipEq (tyvals, tyvals'))
                        ; match_pat ctx left left' left_ty
                        @ match_pat ctx right right' right_ty
                        )
                      else
                        raise Fail "TODO"
                  | _ => raise Fail "TODO"
                  )
              | _ => raise Fail "TODO"
            else
              raise Mismatch "idents not equal"
        | (Ptyped {pat, ty}, _, _) =>
            (* TODO: maybe don't ignore this
             * The reason why this doesn't actually check is because this will
             * cause something like:
             * (fn x : 'a => 'a) 1
             * to cause a type error.
             *
             * The thing is, constraints on typed patterns is already checked by
             * the general expression type-checker. The typed pattern contributes
             * to the type of the function, which is checked against the input
             * argument's type.
             *
             * The point of this function is to assign each binding a type, but
             * maybe it doesn't need to do any extra work in terms of rejection.
             * Maybe it can just let things pass, because it would be caught by
             * expression type-checking anyways.
             * *)
            match_pat ctx pat v tyval
        | (Playered {opp = _, id, ty, aspat}, _, _) =>
            let
              val new_tyval =
                case ty of
                NONE => tyval
              | SOME ty => unify ctx (Context.synth_ty' ctx ty) tyval
            in
              (id, v, new_tyval) :: match_pat ctx aspat v new_tyval
            end
        | _ => raise Mismatch "pats don't match"
      ) handle ListPair.UnequalLengths => raise Mismatch "todo"

    (* Matching a value with a given tyval against a bunch of clauses.
     *)
    fun match_against ctx matches value tyval =
      let
        val (bindings, exp) =
          case
            List.foldl
              (fn ({exp, pat}, NONE) =>
                ( SOME (match_pat ctx pat value tyval, exp)
                  handle Mismatch _ => NONE
                )
              | (_, SOME ans) => SOME ans
              )
              NONE
              matches
          of
            NONE => raise Mismatch "failed to match"
          | SOME ans => ans
      in
        (bindings, Context.add_bindings_combined ctx bindings, exp)
      end

    val match_pat = fn ctx => fn pat => fn value => fn tyval =>
      match_pat ctx pat value tyval

    fun apply_fn (matches, env, VE) value tyval abstys =
      let
        val (bindings, ctx, exp) =
          match_against
            (Context.merge_scope env (Context.ctx_rec (Option.getOpt (VE, Context.scope_empty))))
            matches
            value
            tyval

        val ctx = Context.add_abstys ctx abstys
      in
        (bindings, ctx, exp)
      end


    (* TODO: add a wrapper around this that says what module and signature it
     * failed to ascribe
     *)
    fun ascribe ctx (scope as Scope {identdict, valtydict, moddict, infixdict = _, tynamedict}) seal =
      case seal of
        NONE => scope
      | SOME {opacity, sigval = Sigval { valspecs, tyspecs, dtyspecs, exnspecs, modspecs }} =>
          (* THINK: Should I use the opacity here? *)
          let
            val dtydict = Context.get_dtydict ctx

            (* First, add all the abstract types.
             *)
            val abstydict =
              (* TODO: Deal with equality types? 
               *)
              SymDict.foldl
                (fn (tyname, {equality = _, status}, abstydict) =>
                  case status of
                    Concrete _ => abstydict
                  | Abstract (n, absid) =>
                      case SymDict.find tynamedict tyname of
                        NONE =>
                          spf
                            (`"Failed to find type definition for abstract type "fi"\
                              \ during signature matching.")
                            tyname
                          |> prog_err
                      | SOME (Scheme (n', ty_fn)) =>
                          if n <> n' then
                            spf
                              (`"Arity mismatch in type definition for abstract type "fi"\
                                \ during signature matching.")
                              tyname
                            |> type_err
                          else
                            AbsIdDict.insert abstydict absid (n', ty_fn)
                      | SOME (Datatype tyid) =>
                          if n <> #arity (TyIdDict.lookup dtydict tyid) then
                            spf
                              (`"Arity mismatch in type definition for abstract type "fi"\
                                \ during signature matching.")
                              tyname
                            |> type_err
                          else
                            AbsIdDict.insert abstydict absid (n, fn tyvals => TVapp (tyvals, tyid))
                )
                AbsIdDict.empty
                tyspecs

            (* Next, lets establish a map between all the tyids used within the
             * signature and their tyid counterparts in the actual module.
             *)
            val tyiddict =
              SymDict.foldl
                (fn (dtyname, {tyid, ...}, tyiddict) =>
                  case SymDict.find tynamedict dtyname of
                    NONE =>
                      spf (`"Failed to find datatype definition for "fi" during signature matching.")
                                                                             dtyname
                      |> prog_err
                  | SOME (Datatype tyid') =>
                      TyIdDict.insert tyiddict tyid tyid'
                  | SOME (Scheme _) =>
                      spf
                        (`"Found type definition instead of datatype for "fi"\
                          \ during signature matching.")
                        dtyname
                      |> type_err
                )
                TyIdDict.empty
                dtyspecs

            (* verify_tyval will turn types as specified by the signature into
             * types which can be compared with the real, implementation type.
             * Essentially, we need to replace abstract types with their
             * definitions, and signature-defined datatypes with their
             * implementations.
             *)
            fun verify_tyval tyval =
              case Context.norm_tyval ctx tyval of
                TVtyvar _ => tyval
              | TVapp (tyvals, tyid) =>
                  (case TyIdDict.find tyiddict tyid of
                    NONE => tyval
                  | SOME tyid => TVapp (tyvals, tyid)
                  )
              | TVabs (tyvals, absid) =>
                  (case AbsIdDict.find abstydict absid of
                    NONE => prog_err "should not happen"
                  | SOME (_, ty_fn) => ty_fn tyvals
                  )
              | TVprod tyvals =>
                  TVprod (List.map verify_tyval tyvals)
              | TVrecord fields =>
                  TVrecord
                    (List.map (fn {lab, tyval} => {lab=lab, tyval = verify_tyval tyval}) fields)
              | TVarrow (t1, t2) =>
                  TVarrow (verify_tyval t1, verify_tyval t2)
              | TVvar ( (_, ref NONE)
                      | (_, ref (SOME (Rows _)))
                      ) => tyval
              | TVvar (_, r as ref (SOME (Ty ty))) =>
                  ( r := SOME (Ty (verify_tyval ty))
                  ; tyval
                  )

            fun eq_tyschemes (n, ty_fn) (n', ty_fn') =
              if n <> n' then
                spf
                  (`"Arity mismatch when comparing type schemes during signature matching.")
                |> type_err
              else
                let
                  val tyvars = List.tabulate (n, fn _ => new ())
                in
                  unify
                    ctx
                    (verify_tyval (ty_fn tyvars))
                    (verify_tyval (ty_fn' tyvars))
                end

            fun unify_tyschemes tyscheme1 tyscheme2 =
              unify
                ctx
                (verify_tyval (instantiate_tyscheme ctx tyscheme1))
                (verify_tyval (instantiate_tyscheme ctx tyscheme2))

            val new_tynamedict =
              SymDict.foldl
                (fn (tyname, {equality = _, status}, new_tynamedict) =>
                  (* For testing equality of polymorphic types, we just generate
                   * some fresh TVvars and then check equality.
                   *
                   * For the signature's type, we need to first transform it by
                   * substituting all abstract types from the signature with
                   * their instantiated counterparts, as well as all signature
                   * datatypes with these datatypes.
                   *)
                  case (status, SymDict.find tynamedict tyname) of
                    (Concrete (n, ty_fn), SOME (old as Datatype tyid)) =>
                      let
                        val tyvars = List.tabulate (n, fn _ => new ())
                      in
                        ( unify ctx (verify_tyval (ty_fn tyvars)) (TVapp (tyvars, tyid))
                        ; SymDict.insert new_tynamedict tyname old
                        )
                      end
                  | (Concrete (n, ty_fn), SOME (old as Scheme (n', ty_fn'))) =>
                      if n <> n' then
                        spf
                          (`"Arity mismatch when comparing type schemes \
                            \ during signature matching.")
                        |> type_err
                      else
                        let
                          val tyvars = List.tabulate (n, fn _ => new ())
                        in
                          ( unify ctx (verify_tyval (ty_fn tyvars)) (ty_fn' tyvars)
                          ; SymDict.insert new_tynamedict tyname old
                          )
                        end
                  | (Abstract (n, absid), SOME _) =>
                      SymDict.insert new_tynamedict tyname
                        (Scheme (guard_tyscheme (n, fn tyvals => TVabs (tyvals, absid))))
                  | (_, NONE) =>
                        spf
                          (`"Failed to find type definition for "fi"\
                            \ during signature matching.")
                          tyname
                        |> prog_err
                )
                SymDict.empty
                tyspecs

            (* For all the datatypes, check that they have matching
             * constructors.
             *
             * We also want all of the cons, so we can later add them to the
             * environment.
             *)
            val cons =
              SymDict.foldl
                (fn ( _ 
                  , {arity = sig_arity, tyid = sig_tyid, cons = sig_cons}
                  , cons
                  ) =>
                  let
                    val mod_tyid = TyIdDict.lookup tyiddict sig_tyid
                    val {arity = mod_arity, cons = mod_cons} =
                      TyIdDict.lookup dtydict mod_tyid

                    (* Could be more efficient, but the number of constructors
                     * has gotta be real low.
                     *)
                    fun subset cons1 cons2 =
                      List.foldl
                        (fn ({id, tyscheme}, ()) =>
                          case List.find (fn {id = id', ...} => Symbol.eq (id, id')) cons2 of
                            NONE =>
                              spf
                                (`"Found unshared constructor "fi" when comparing\
                                  \ type schemes for equality in signature matching.")
                                id
                              |> prog_err
                          | SOME {tyscheme = tyscheme', ...} =>
                              ( eq_tyschemes tyscheme tyscheme'
                              ; ()
                              )
                        )
                        ()
                        cons1

                    val cons_to_add =
                      List.map (fn {id, tyscheme} => (id, tyscheme, mod_tyid)) mod_cons
                  in
                    if mod_arity <> sig_arity then
                      spf
                        (`"Arity mismatch between datatypes during signature matching.")
                      |> type_err
                    else
                      ( subset mod_cons sig_cons
                      ; subset sig_cons mod_cons
                      ; cons_to_add @ cons
                      )
                  end
                )
                []
                dtyspecs

            (* Populate the valtydict and identdict by iterating over all the
             * specifications for values.
             *)
            val (new_valtydict, new_identdict) =
              SymDict.foldl
                (* TODO: these absids should be used at some point *)
                (fn (id, (sig_tyscheme, absids), (acc_valtydict, acc_identdict)) =>
                  let
                    val (_, mod_tyscheme) = SymDict.lookup valtydict id
                  in
                    (* Check that the types of the values are the same.
                     * If so, then we want to add this identifier with a value
                     * tag to the identdict and valtydict.
                     *)
                     (* TODO: Equality is too strong of a condition.
                      * We want that they can unify to each other.
                      *)
                    ( unify_tyschemes mod_tyscheme sig_tyscheme
                    ; ( SymDict.insert
                          acc_valtydict
                          id
                          (Vsign, sig_tyscheme)
                      , case SymDict.find identdict id of
                          (* TODO: these abstys should be used at some point *)
                          SOME (V (Vfn {matches, env, abstys, rec_env, break})) =>
                            SymDict.insert
                              acc_identdict
                              id
                              (V
                                (Vfn { matches = matches
                                     , env = env
                                     , abstys = abstydict
                                     , rec_env = rec_env
                                     , break = break
                                     }
                                )
                              )
                        | SOME (V value) =>
                            SymDict.insert
                              acc_identdict
                              id
                              (V value)
                          (* TODO: check this
                           * if it was originally a constructor or exception, just
                           * add it back as a function whcih is a constr for its val
                           *)
                        | SOME (C _ | E _) =>
                            SymDict.insert
                              acc_identdict
                              id
                              (V (Vconstr {id = [id], arg = NONE}))
                        | _ =>
                            spf
                              (`"Failed to find value identifier "fi"\
                                \ during signature ascription")
                              id
                            |> prog_err
                    )
                  )
                end
              )
              (SymDict.empty, SymDict.empty)
              valspecs

            (* Add exns *)
            val (new_valtydict, new_identdict) =
              SymDict.foldl
                (fn (id, sig_tyscheme, (acc_valtydict, acc_identdict)) =>
                  let
                    val (_, mod_tyscheme) = SymDict.lookup valtydict id
                  in
                    ( eq_tyschemes sig_tyscheme mod_tyscheme
                    ; case SymDict.find identdict id of
                        SOME (ans as E _) =>
                          ( SymDict.insert acc_valtydict id (Esign, mod_tyscheme)
                          , SymDict.insert acc_identdict id ans
                          )
                      | _ =>
                        spf
                          (`"Failed to find exception "fi"\
                            \ during signature ascription")
                          id
                        |> prog_err
                    )
                  end
                )
                (new_valtydict, new_identdict)
                exnspecs

            (* Add cons *)
            val (new_valtydict, new_identdict) =
               List.foldl
                (fn ((id, tyscheme, tyid), (acc_valtydict, acc_identdict)) =>
                  ( SymDict.insert acc_valtydict id (Csign, tyscheme)
                  , SymDict.insert acc_identdict id (C tyid)
                  )
                )
                (new_valtydict, new_identdict)
                cons

            val new_moddict =
              SymDict.foldl
                (fn (id, sigval, acc_moddict) =>
                  SymDict.insert
                    acc_moddict
                    id
                    ( ascribe ctx
                        (SymDict.lookup moddict id)
                        ( SOME
                            { opacity = Transparent
                            , sigval = sigval
                            }
                        )
                    )
                  handle SymDict.Absent =>
                  spf
                    (`"Failed to find module "fi" during signature ascription")
                    id
                  |> prog_err
                )
                SymDict.empty
                modspecs
          in
            Scope
              { identdict = new_identdict
              , valtydict = new_valtydict
              , moddict = new_moddict
              , infixdict = SymDict.empty
              , tynamedict = new_tynamedict
              }
          end
  end
