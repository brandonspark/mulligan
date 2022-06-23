
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

    val synth_dec :
        SMLSyntax.context
     -> SMLSyntax.dec
     -> dec_status

    val ascribe :
        Context.scope
     -> { opacity : SMLSyntax.opacity, sigval : SMLSyntax.sigval} option
     -> Context.scope
  end =
  struct
    open SMLSyntax
    open Error

    infix |>
    fun x |> f = f x

    val sym = Symbol.fromValue

    fun enum l =
      List.foldl
        (fn (elem, (i, acc)) =>
          (i + 1, (i, elem) :: acc)
        )
        (0, [])
        l
      |> (fn (_, l) => List.rev l)

    fun iter_list f z l =
      case l of
        [] => z
      | x::xs =>
          iter_list f (f (x, xs, z)) xs

    fun nexpansive_left_app ctx exp =
      case exp of
        ( Eparens exp
        | Etyped {exp, ...} ) =>
          nexpansive_left_app ctx exp
      | Eident {id, ...} => Context.is_con ctx id
      | _ => false

    fun new () = TVvar (ref NONE)

    val bool_ty = Basis.bool_ty

    fun union_three s1 s2 s3 =
      SymSet.union
        (SymSet.union s1 s2)
        s3

    fun set_from_list l =
      List.foldl
        (fn (elem, acc) =>
          SymSet.insert acc elem
        )
        SymSet.empty
        l

    fun union_sets l =
      List.foldl
        (fn (elem, acc) =>
          SymSet.union acc elem
        )
        SymSet.empty
        l

    fun occurs_check r tyval =
      case tyval of
        TVtyvar _ => false
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
      | TVvar (r' as ref (SOME (Rows fields))) =>
          r = r' orelse
          List.foldl
            (fn ({tyval, ...}, b) =>
               b orelse occurs_check r tyval
            )
            false
            fields
      | TVvar (r' as ref NONE) =>
          r = r'
      | TVvar (r' as ref (SOME (Ty tyval))) =>
          r = r' orelse occurs_check r tyval
      | TVarrow (t1, t2) =>
          occurs_check r t1 orelse occurs_check r t2

    fun nexpansive ctx exp =
      case exp of
        ( Enumber _
        | Estring _
        | Echar _
        | Eident _
        ) => true
      | Erecord fields =>
          List.foldl
            (fn ({lab, exp}, acc) =>
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

    (* TODO: expansive quantifier stuff *)

    fun collect_tyvars_ty ty =
      case ty of
        Tident _ => SymSet.empty
      | Ttyvar sym => SymSet.singleton sym
      | ( Tapp (tys, _)
        | Tprod tys ) => List.map collect_tyvars_ty tys |> union_sets
      | Tarrow (t1, t2) =>
          SymSet.union (collect_tyvars_ty t1) (collect_tyvars_ty t2)
      | Trecord fields =>
          List.map (fn {ty, ...} => collect_tyvars_ty ty) fields |> union_sets
      | Tparens ty => collect_tyvars_ty ty

    fun collect_tyvars_tyval tyval =
      case tyval of
        TVtyvar sym => [Proper sym]
      | ( TVapp (tyvals, _)
        | TVabs (tyvals, _) ) =>
          (List.concat o List.map collect_tyvars_tyval) tyvals
      | TVprod tyvals =>
          (List.concat o List.map collect_tyvars_tyval) tyvals
      | TVarrow (tyval1, tyval2) =>
          collect_tyvars_tyval tyval1 @ collect_tyvars_tyval tyval2
      | TVrecord fields =>
          (List.concat o List.map (fn {lab, tyval} => collect_tyvars_tyval
          tyval)) fields
      | TVvar (r as ref NONE) => [Unconstrained r]
      | TVvar (r as ref (SOME (Ty tyval))) =>
          collect_tyvars_tyval tyval
      | TVvar (r as ref (SOME (Rows fields))) =>
          (List.concat o List.map (fn {lab, tyval} => collect_tyvars_tyval tyval)) fields

    fun collect_tyvars_patrow patrow =
      case patrow of
        PRellipsis => SymSet.empty
      | PRlab {pat, ...} => collect_tyvars_pat pat
      | PRas {ty, aspat, ...} =>
          (case (ty, aspat) of
            (SOME ty, NONE) => collect_tyvars_ty ty
          | (NONE, SOME pat) => collect_tyvars_pat pat
          | (NONE, NONE) => SymSet.empty
          | (SOME ty, SOME pat) =>
              SymSet.union (collect_tyvars_ty ty) (collect_tyvars_pat pat)
          )

    and collect_tyvars_pat pat =
      case pat of
        ( Pnumber _
        | Pword _
        | Pstring _
        | Pchar _
        | Pwild
        | Pident _
        | Punit ) => SymSet.empty
      | Precord patrows =>
          List.map collect_tyvars_patrow patrows |> union_sets
      | Pparens pat => collect_tyvars_pat pat
      | Ptuple pats => List.map collect_tyvars_pat pats |> union_sets
      | (Por pats | Plist pats) => List.map collect_tyvars_pat pats |> union_sets
      | Papp {atpat, ...} => collect_tyvars_pat atpat
      | Pinfix {left, right, ...} =>
          SymSet.union
            (collect_tyvars_pat left)
            (collect_tyvars_pat right)
      | Ptyped {pat, ty} =>
          SymSet.union
            (collect_tyvars_pat pat)
            (collect_tyvars_ty ty)
      | Playered {ty, aspat, ...} =>
          SymSet.union
            (collect_tyvars_pat pat)
            (case ty of
              NONE => SymSet.empty
            | SOME ty => collect_tyvars_ty ty
            )

    fun collect_tyvars exp =
      case exp of
        ( Enumber _
        | Estring _
        | Echar _
        | Eunit
        | Eident _
        | Eselect _ ) => SymSet.empty
      | Erecord fields =>
          List.map
            (fn {lab, exp} => collect_tyvars exp)
            fields
          |> union_sets
      | Etuple exps =>
          List.map collect_tyvars exps |> union_sets
      | Elist exps =>
          List.map collect_tyvars exps |> union_sets
      | Eseq exps =>
          List.map collect_tyvars exps |> union_sets
      | Elet {dec, exps} =>
          SymSet.union
            (collect_tyvars_dec dec)
            (union_sets (List.map collect_tyvars exps))
      | Eparens exp => collect_tyvars exp
      | ( Eapp {left, right}
        | Einfix {left, right, ...}
        | Eandalso {left, right}
        | Eorelse {left, right} ) =>
          SymSet.union
            (collect_tyvars left)
            (collect_tyvars right)
      | Etyped {exp, ty} =>
          SymSet.union
            (collect_tyvars exp)
            (collect_tyvars_ty ty)
      | Ehandle {exp, matches} =>
          SymSet.union
            (collect_tyvars exp)
            (collect_tyvars_matches matches)
      | Eraise exp => collect_tyvars exp
      | Eif {exp1, exp2, exp3} =>
          union_three
            (collect_tyvars exp1)
            (collect_tyvars exp2)
            (collect_tyvars exp3)
      | Ewhile {exp1, exp2} =>
          SymSet.union
            (collect_tyvars exp1)
            (collect_tyvars exp2)
      | Ecase {exp, matches} =>
          SymSet.union
            (collect_tyvars exp)
            (collect_tyvars_matches matches)
      | Efn (matches, _) =>
          collect_tyvars_matches matches
      | Ehole => raise Fail "shouldn't happen"

    and collect_tyvars_matches matches =
      List.foldl
        (fn ({pat, exp}, acc) =>
          union_three
            (collect_tyvars exp)
            (collect_tyvars_pat pat)
            acc
        )
        SymSet.empty
        matches

    and collect_tyvars_typbinds typbinds =
      List.map
        (fn {tyvars, ty, ...} =>
          SymSet.difference
            (collect_tyvars_ty ty)
            (set_from_list tyvars)
        )
        typbinds
      |> union_sets

    and collect_tyvars_datbinds datbinds =
      List.map
        (fn {tyvars, conbinds, ...} =>
          SymSet.difference
            (List.map (fn {ty, ...} =>
              case ty of
                NONE => SymSet.empty
              | SOME ty => collect_tyvars_ty ty
              ) conbinds |> union_sets
            )
            (set_from_list tyvars)
        )
        datbinds
      |> union_sets

    and collect_tyvars_fname_args fname_args =
      case fname_args of
        Fprefix {id, args, ...} =>
          union_sets (List.map collect_tyvars_pat args)
      | Finfix {left, right, ...} =>
          SymSet.union
            (collect_tyvars_pat left)
            (collect_tyvars_pat right)
      | Fcurried_infix {left, right, args, ...} =>
          union_three
            (collect_tyvars_pat left)
            (collect_tyvars_pat right)
            (union_sets (List.map collect_tyvars_pat args))

    (* We're looking for unguarded tyvars, meaning that we should not descend
     * into smaller val declarations.
     *
     * We technically want both unguarded and implicitly scoped, meaning not
     * scoped by a farther up val declaration, but we will take care of that
     * with the call stack.
     *)
    and collect_tyvars_dec dec =
      case dec of
        ( Dval _
        | Dfun _
        | Ddatrepl _
        | Dopen _
        | Dinfix _
        | Dinfixr _
        | Dnonfix _ ) => SymSet.empty
      | Dtype typbinds =>
          collect_tyvars_typbinds typbinds
      | Ddatdec {datbinds, withtypee} =>
          SymSet.union
            (collect_tyvars_datbinds datbinds)
            (case withtypee of
              NONE => SymSet.empty
            | SOME typbinds => collect_tyvars_typbinds typbinds
            )
      | Dabstype {datbinds, withtypee, withh} =>
          union_three
            (collect_tyvars_datbinds datbinds)
            (collect_tyvars_dec withh)
            (case withtypee of
              NONE => SymSet.empty
            | SOME typbinds => collect_tyvars_typbinds typbinds
            )
      | Dexception exbinds =>
          (List.map
            (fn Xnew {ty, ...} =>
              (case ty of
                NONE => SymSet.empty
              | SOME ty => collect_tyvars_ty ty
              )
            | Xrepl _ => SymSet.empty
            )
            exbinds
          |> union_sets
          )
      | Dlocal {left_dec, right_dec} =>
          SymSet.union
            (collect_tyvars_dec left_dec)
            (collect_tyvars_dec right_dec)
      | Dseq decs =>
          List.map
            collect_tyvars_dec
            decs
          |> union_sets
      | Dhole => raise Fail "should not happen"

    fun norm_tyval tyval =
      case tyval of
        TVvar (r as ref NONE) => tyval
      | TVvar (r as ref (SOME (Ty tyval))) => tyval
      | TVvar (r as ref (SOME (Rows _))) => tyval
      | TVapp (tyvals, tyid) =>
          TVapp (List.map norm_tyval tyvals, tyid)
      | TVabs (tyvals, absid) =>
          TVabs (List.map norm_tyval tyvals, absid)
      | TVprod tyvals =>
          TVprod (List.map norm_tyval tyvals)
      | TVrecord fields =>
          TVrecord
            (List.map (fn {lab, tyval} => {lab = lab, tyval = norm_tyval tyval}) fields)
      | TVarrow (t1, t2) =>
          TVarrow (norm_tyval t1, norm_tyval t2)
      | TVtyvar sym => TVtyvar sym

    (* This is so we can facilitate a little bit of code reuse between the
     * statics and the debugger.
     *
     * The debugger needs to do pretty much the exact same stuff as the statics
     * for a dec, plus a little bit more. This only happens in three cases
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

    fun quantify_tyval tyvar_fn ctx ty =
      let
        val quantify_tyval = fn ctx => fn ty => quantify_tyval tyvar_fn ctx ty
      in
        case (norm_tyval ty) of
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


    and norm ctx exp = norm_tyval (synth ctx exp)

    and fields_subset fields1 fields2 =
      ( List.foldl
          (fn ({lab, tyval}, ()) =>
            ( case
                List.find (fn {lab = lab', ...} => Symbol.eq (lab, lab')) fields2
              of
                NONE => prog_err "failed to unify record types"
              | SOME {tyval = tyval', ...} => unify tyval tyval'
            ; ()
            )
          )
          ()
          fields1
      ; ()
      )


    and unify t1 t2 =
      case (norm_tyval t1, norm_tyval t2) of
      (* At this point, shoud only be able to be a
       * TVvar NONE, TVtyvar, TVapp, TVprod, TVarrow, TVrecord
       *)
        (TVvar (r as ref NONE), _) =>
          (occurs_check r t2; r := SOME (Ty t2); t2)
      | (_, TVvar (r as ref NONE)) =>
          (occurs_check r t1; r := SOME (Ty t1); t1)
      | (TVvar (r as ref (SOME (Ty t1))), _) =>
          unify t1 t2
      | (_, TVvar (r as ref (SOME (Ty t2)))) =>
          unify t1 t2
      | ( TVvar (r1 as ref (SOME (Rows fields1)))
        , TVvar (r2 as ref (SOME (Rows fields2)))
        ) =>
          let
            val res =
              List.foldl
                (fn ({lab, tyval}, acc) =>
                  SymDict.insertMerge acc lab tyval (fn tyval' => unify tyval tyval')
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
      | (TVvar (r as ref (SOME (Rows rows_fields))), TVrecord record_fields) =>
          ( fields_subset rows_fields record_fields
          ; r := SOME (Ty t2)
          ; t2
          )
      | (TVrecord record_fields, TVvar (r as ref (SOME (Rows rows_fields)))) =>
          ( fields_subset rows_fields record_fields
          ; r := SOME (Ty t1)
          ; t1
          )
      | (TVrecord fields1, TVrecord fields2) =>
          ( fields_subset fields1 fields2
          ; fields_subset fields2 fields1
          ; t1
          )
      | (TVtyvar sym1, TVtyvar sym2) =>
          if Symbol.eq (sym1, sym2) then t2
          else prog_err "failed to unify tyvars"
      | (TVapp (tys1, id1), TVapp (tys2, id2)) =>
          ( List.map (fn (t1, t2) => unify t1 t2) (ListPair.zipEq (tys1, tys2))
          ; if TyId.eq (id1, id2) then () else prog_err "non-matching tyids"
          ; t1
          )
      | (TVprod tys1, TVprod tys2) =>
          ( List.map (fn (t1, t2) => unify t1 t2) (ListPair.zipEq (tys1, tys2))
          ; t1
          )
      | (TVarrow (t1', t2'), TVarrow (t1'', t2'')) =>
          ( unify t1' t1''
          ; unify t2' t2''
          ; t1
          )
      | _ =>
          prog_err ( "Failed to unify different types: "
                   ^ PrettyPrintAst.print_tyval t1
                   ^ " and "
                   ^ PrettyPrintAst.print_tyval t2
                   )

    and unify_row r (sym, tyopt) =
      let
        val new =
          case tyopt of
            NONE => new ()
          | SOME ty => ty
      in
        case r of
          ref NONE =>
            ( r := SOME (Rows [{lab = sym, tyval = new}])
            ; new
            )
        | ref (SOME (Ty (TVrecord fields))) =>
            (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
              NONE => prog_err "Accessing nonexistent field in record type"
            | SOME {lab, tyval} => tyval
            )
        | ref (SOME (Rows fields)) =>
            (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
              NONE =>
                ( r := SOME (Rows ({lab = sym, tyval = new} :: fields))
                ; new
                )
            | SOME {lab, tyval} => tyval
            )
        | ref (SOME (Ty (TVprod tys))) =>
            (case Int.fromString (Symbol.toValue sym) of
              NONE => prog_err "Accessing non-numeral field in product type"
            | SOME i =>
                if i - 1 < List.length tys then
                  ( unify (List.nth (tys, i - 1)) new
                  ; (List.nth (tys, i - 1))
                  )
                else
                  prog_err "Accessing out-of-bounds numeral field in product type"
            )
        | ref (SOME (Ty (TVvar r'))) =>
            unify_row r' (sym, tyopt)
        | _ => prog_err "Accessing field of non-record, non-product type"
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
            NONE => prog_err "cannot find val of identifier"
            (* TODO: if a con, need brand new instances for entry vars *)
          | SOME (_, tyscheme) => instantiate_tyscheme ctx tyscheme
          )
      | Etuple exps =>
          List.map (synth ctx) exps
          |> TVprod
      | Elist exps =>
          List.foldl
            (fn (exp, ty) =>
              unify ty (synth ctx exp)
            )
            (new ())
            exps
          |> (fn ty => TVapp ([ty], Basis.list_tyid))
      | Eseq exps =>
          List.foldl
            (fn (exp, ty) =>
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
              TVvar r => unify_row r (sym, NONE)
            | TVprod tys =>
                (case Int.fromString (Symbol.toValue sym) of
                  NONE => prog_err "Selecting non-numeral label from product type"
                | SOME i =>
                    if i - 1 < List.length tys then
                      List.nth (tys, i - 1)
                    else
                      prog_err "Selecting out-of-bounds label in product type"
                )
            | TVrecord fields =>
                (case List.find (fn {lab, ...} => Symbol.eq (lab, sym)) fields of
                  NONE => prog_err "Selecting nonexistent field in record type"
                | SOME {lab, tyval} => tyval
                )
            | _ => prog_err "Invalid type to select label from"
          )
      | Eapp {left, right} =>
          (case norm ctx left of
            TVvar (r as ref NONE) =>
              let
                val out_ty = new ()
                val in_ty = synth ctx right
              in
                r := SOME (Ty (TVarrow (in_ty, out_ty)));
                out_ty
              end
          | TVarrow (t1, t2) =>
              (unify t1 (synth ctx right); t2)
          | _ => prog_err "Invalid type for left application"
          )
      | Einfix {left, id, right} =>
          (case norm ctx (Eident {opp = false, id = [id]}) of
            TVvar (r as ref NONE) =>
              let
                val out_ty = new ()
                val left_ty = synth ctx left
                val right_ty = synth ctx right
              in
                r := SOME (Ty (TVarrow (TVprod [left_ty, right_ty], out_ty)));
                out_ty
              end
          | TVarrow (t1, t2) =>
              (unify t1 (TVprod [synth ctx left, synth ctx right]); t2)
          | _ => prog_err "Invalid type for infix identifier"
          )
      | Etyped {exp, ty} =>
          unify
            (synth ctx exp) (* TODO: check these functions *)
            (Context.synth_ty (fn _ => NONE) (fn _ => NONE) ctx ty)
      | ( Eandalso {left, right} | Eorelse {left, right} ) =>
          ( unify (synth ctx left) bool_ty
          ; unify (synth ctx right) bool_ty
          ; bool_ty
          )
      | Ehandle {exp, matches} =>
          List.foldl
            (fn ({pat, exp}, body_ty) =>
              let
                val (bindings, pat_ty) = synth_pat ctx pat
                (* These will be unquantified, as they should be, since we
                 * cannot generalize polymorphic bindings at a handle site.
                 *)
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify pat_ty Basis.exn_ty
                ; unify (synth new_ctx exp) body_ty
                )
              end
            )
            (synth ctx exp)
            matches
      | Eraise exp =>
          ( unify (synth ctx exp) Basis.exn_ty
          ; new ()
          )
      | Eif {exp1, exp2, exp3} =>
          ( unify (synth ctx exp1) bool_ty
          ; unify (synth ctx exp2) (synth ctx exp3)
          )
      | Ewhile {exp1, exp2} =>
          ( unify (synth ctx exp1) bool_ty
          ; Basis.unit_ty
          )
      | Ecase {exp, matches} =>
          List.foldl
            (fn ({pat, exp}, (pat_ty, body_ty)) =>
              let
                val (bindings, pat_ty') = synth_pat ctx pat
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify pat_ty' pat_ty
                , unify (synth new_ctx exp) body_ty
                )
              end
            )
            (synth ctx exp, new ())
            matches
          |> (fn (fst, snd) => snd)
      | Efn (matches, _) =>
          List.foldl
            (fn ({pat, exp}, (pat_ty, body_ty)) =>
              let
                val (bindings, pat_ty') = synth_pat ctx pat
                val new_ctx = Context.add_unquantified_val_ty_bindings ctx bindings
              in
                ( unify pat_ty' pat_ty
                , unify (synth new_ctx exp) body_ty
                )
              end
            )
            (new (), new ())
            matches
          |> TVarrow
      | Ehole => raise Fail "shouldn't happen"

    and quantify_binding (id, tyval) ctx =
      let
        val tyval = norm_tyval tyval

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

        val tyvars = collect_tyvars_tyval tyval
        val current_tyvars = Context.get_current_tyvars collect_tyvars_tyval ctx
        val quantified_tyvars =
          tyvars_difference tyvars current_tyvars

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
             * quantified tyvar that replaces it.
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
        ( id
        , ( num_quantifiers
          , type_scheme
          )
        )
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
                    (collect_tyvars_pat pat)
                    (collect_tyvars exp)
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
          get_pat_list_ty_bindings
            ctx
            (List.map (fn {pat, ...} => pat) valbinds)

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
            Context.add_unquantified_val_ty_bindings ctx bindings
          else
            ctx

        (* Check that each purported pat type unifies with the expression's
         * type.
         *)
        val () =
          List.foldr
            (fn (({exp, ...}, pat_ty), ()) =>
              ( unify (synth exp_ctx exp) pat_ty
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
                        (collect_tyvars_fname_args fname_args)
                        (case ty of NONE => SymSet.empty | SOME ty =>
                        collect_tyvars_ty ty)
                        (collect_tyvars exp)
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


        (* This context has all of the mutually recursive function names
         * added into the context.
         * We will use this to evaluate each individual function.
         *)
        val (fn_bindings, fn_data, new_ctx) =
          List.foldr
            (fn (fvalbind, (acc_bindings, fn_data, new_ctx)) =>
              let
                val out_ty = new ()
                fun some_unify tyopt =
                  case tyopt of
                    NONE => ()
                  | SOME ty =>
                      ( unify
                          out_ty
                          (Context.synth_ty (fn _ => NONE) (fn _ => NONE) ctx ty)
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
                      val _ = some_unify ty
                      val id =
                        case acc of
                          NONE => id'
                        | SOME (id, _, _, _) =>
                          if Symbol.eq (id, id') then id
                          else prog_err "Function names non-matching in clauses"
                    in
                      (* For each function clause, we're accumulating that
                       * clause's bindings and body expression.
                       * Overall, we're also keeping the name of the
                       * function (should be the same), and the types of the
                       * patterns of its curried arguments.
                       *)
                      case acc of
                        NONE => SOME (id, [bindings'], pat_tys', [exp])
                      | SOME (_, bindings, pat_tys, exps) =>
                          ( List.map
                              (fn (pat_ty1, pat_ty2) => unify pat_ty1 pat_ty2)
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
            (fn ((id, bindings, pat_tys, exps, out_ty), ()) =>
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

        (* Set all the new tyvars to quantified variables.
         *)
        val quantified_bindings =
          List.map (fn binding => quantify_binding binding orig_ctx) fn_bindings
      in
        ( validate_clauses fn_data
        ; Context.add_val_ty_bindings orig_ctx quantified_bindings
        )
      end

    and get_pat_list_ty_bindings ctx pats =
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

    and instantiate_tyscheme ctx (arity, ty_fn) =
      norm_tyval (ty_fn (List.tabulate (arity, fn _ => new ())))

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
            val row_ref = ref NONE
          in
            List.foldr
              (fn (patrow, (bindings, acc)) =>
                case patrow of
                  PRellipsis => (bindings, true)
                | PRlab {lab, pat} =>
                    let
                      val (bindings', ty) = synth_pat ctx pat
                      val _ = unify_row row_ref (lab, SOME ty)
                    in
                      (bindings @ bindings', acc)
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
                                | SOME ty'' => (unify ty' (Context.synth_ty' ctx ty''); ())
                            in
                              ((id, ty') :: bindings @ bindings', ty')
                            end
                    in
                      ( unify_row row_ref (id, SOME ty)
                      ; (bindings, acc)
                      )
                    end
              )
              ([], false)
              patrows
            |> (fn (bindings, b) =>
                if b then
                  (bindings, TVvar row_ref)
                else
                  ( bindings
                  , case row_ref of
                      ref (SOME (Rows fields)) =>
                        ( row_ref := SOME (Ty (TVrecord fields))
                        ; TVvar row_ref
                        )
                    | _ => raise Fail "impossible"
                  )
               )
          end
      | Pparens pat => synth_pat ctx pat
      | Punit => ([], new ())
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
                (bindings @ bindings', unify ty ty')
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
                    fun assert_one_sided ctx bindings1 bindings2 =
                      List.foldl
                        (fn ((id, ty), ()) =>
                          case List.find (fn (id', _) => Symbol.eq (id, id')) bindings2 of
                            NONE => prog_err "bindings not same"
                          | SOME (_, ty') => ( unify ty ty'; () )
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
                    SOME (bindings, unify ty ty')
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
                prog_err "Invalid type for applied identifier in pattern"
            | SOME ((Esign | Csign), tyscheme) =>
                let
                  val (bindings, ty'') = synth_pat ctx atpat
                in
                  ( case instantiate_tyscheme ctx tyscheme of
                      TVarrow (t1, t2) =>
                        ( unify t1 ty''
                        ; (bindings, ty'')
                        )
                    | _ => raise Fail "prob shouldn't happen"
                  )
                end
            | SOME (Vsign, _) =>
                prog_err "Applied pattern identifier a non-constructor"
          )
      | Pinfix {left, id, right} =>
          ( case Context.get_ident_ty_opt ctx [id] of
              NONE => prog_err "Nonexistent type for infix pattern identifier"
            | SOME ((Csign | Esign), tyscheme) =>
                (case instantiate_tyscheme ctx tyscheme of
                  TVarrow (t1, t2) =>
                    let
                      val (bindings, ty'') = synth_pat ctx left
                      val (bindings', ty''') = synth_pat ctx right
                    in
                      ( unify t1 (TVprod [ty'', ty'''])
                      ; (bindings @ bindings', t2)
                      )
                    end
                | _ => raise Fail "should not happen"
                )
            | SOME (Vsign, _) =>
                prog_err "Infix pattern identifier a non-cosntructor"
          )
      | Ptyped {pat, ty} =>
          let
            val (bindings, ty') = synth_pat ctx pat
            val tyval = Context.synth_ty' ctx ty
          in
            (bindings, unify tyval ty')
          end
      | Playered {id, ty, aspat, ...} =>
          let
            val (bindings, ty') = synth_pat ctx aspat
            val tyval =
              case ty of
                SOME ty => unify ty' (Context.synth_ty' ctx ty)
              | NONE => ty'
          in
            (bindings, tyval)
          end

    and synth_pat' ctx pat = #2 (synth_pat ctx pat)

    and statics_dec_status dec_status =
      case dec_status of
        DEC_VAL (new_ctx, exp_ctx, _) => new_ctx
      | DEC_FUN (ctx, _) => ctx
      | DEC_CTX ctx => ctx
      | DEC_CONT f =>
          let
            fun funarg location dec ctx =
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
                          |> Option.map (fn (tycon, id) => TVapp (tyvals, id))

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
                          prog_err "Invalid number of tyargs to instantiate type scheme"
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
                    , ( 0
                      , case ty of
                          NONE =>
                            ( fn tyvals => Basis.exn_ty )
                        | SOME ty =>
                            ( fn tyvals => TVarrow (Context.synth_ty' ctx ty, Basis.exn_ty) )
                      )
                    )
              | Xrepl {left_id, right_id, ...} =>
                  ( case Context.get_exn_opt ctx right_id of
                      NONE => prog_err "cannot find exn to replicate"
                    | SOME exn_id =>
                      Context.add_exn
                        new_ctx
                        exn_id
                        ( left_id
                        , case Context.get_ident_ty ctx right_id of
                            (Esign, tyscheme) => tyscheme
                          | _ => prog_err "found non-exn to replicate"
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
                ( id
                , LEFT
                , Option.getOpt (precedence, 0)
                )
            )
            ctx
            ids
          |> DEC_CTX
      | Dinfixr {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id
                , RIGHT
                , Option.getOpt (precedence, 0)
                )
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

    fun ascribe (scope as Scope {identdict, valtydict, moddict, infixdict, tydict, tynamedict}) seal =
      case seal of
        NONE => scope
      | SOME {opacity, sigval = Sigval { valspecs, tyspecs, dtyspecs, exnspecs, modspecs }} =>
          let
            (* First, add all the abstract types.
             *)
            val abstydict =
              SymDict.foldl
                (fn (tyname, {equality, status}, abstydict) =>
                  case status of
                    Concrete _ => abstydict
                  | Abstract (n, absid) =>
                      case SymDict.find tynamedict tyname of
                        NONE => prog_err "cannot find abstract type during signature matching"
                      | SOME (ans as Scheme (n', ty_fn)) =>
                          if n <> n' then
                            prog_err "abstract type cannot match type of \
                                     \different arity in signature matching"
                          else
                            AbsIdDict.insert abstydict absid ty_fn
                      | SOME (ans as Datatype tyid) =>
                          if n <> #arity (TyIdDict.lookup tydict tyid) then
                            prog_err "abstract type cannot match type of \
                                     \different arity in signature matching"
                          else
                            AbsIdDict.insert abstydict absid (fn tyvals => TVapp (tyvals, tyid))
                )
                AbsIdDict.empty
                tyspecs

            (* Next, lets establish a map between all the tyids used within the
             * signature and their tyid counterparts in the actual module.
             *)
            val dtydict =
              SymDict.foldl
                (fn (dtyname, {arity, tyid, cons}, dtydict) =>
                  case SymDict.find tynamedict dtyname of
                    NONE =>
                      prog_err "failed to find datatype during signature matching"
                  | SOME (Datatype tyid') =>
                      TyIdDict.insert dtydict tyid tyid'
                  | SOME (Scheme _) =>
                      prog_err "found type instead of datatype during signature matching"
                )
                TyIdDict.empty
                dtyspecs

            fun verify_tyval tyval =
              case tyval of
                TVtyvar sym => tyval
              | TVapp (tyvals, tyid) =>
                  (case TyIdDict.find dtydict tyid of
                    NONE => tyval
                  | SOME tyid => TVapp (tyvals, tyid)
                  )
              | TVabs (tyvals, absid) =>
                  (case AbsIdDict.find abstydict absid of
                    NONE => prog_err "should not happen"
                  | SOME ty_fn => ty_fn tyvals
                  )
              | TVprod tyvals =>
                  TVprod (List.map verify_tyval tyvals)
              | TVrecord fields =>
                  TVrecord
                    (List.map (fn {lab, tyval} => {lab=lab, tyval = verify_tyval tyval}) fields)
              | TVarrow (t1, t2) =>
                  TVarrow (verify_tyval t1, verify_tyval t2)
              | TVvar (ref NONE | ref (SOME (Rows _))) => raise Fail "should not happen"
              | TVvar (r as ref (SOME (Ty ty))) =>
                  ( r := SOME (Ty (verify_tyval ty))
                  ; tyval
                  )

            fun eq_tyschemes (n, ty_fn) (n', ty_fn') =
              if n <> n' then
                prog_err "mismatching arity when comparing types in sig matching"
              else
                let
                  val tyvars = List.tabulate (n, fn _ => TVvar (ref NONE))
                in
                  unify
                    (verify_tyval (ty_fn tyvars))
                    (verify_tyval (ty_fn' tyvars))
                end


            val new_tynamedict =
              SymDict.foldl
                (fn (tyname, {equality, status}, new_tynamedict) =>
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
                        val tyvars = List.tabulate (n, fn _ => TVvar (ref NONE))
                      in
                        ( unify (verify_tyval (ty_fn tyvars)) (TVapp (tyvars, tyid))
                        ; SymDict.insert new_tynamedict tyname old
                        )
                      end
                  | (Concrete (n, ty_fn), SOME (old as Scheme (n', ty_fn'))) =>
                      if n <> n' then
                        prog_err "arity mismatch in types during signature matching"
                      else
                        let
                          val tyvars = List.tabulate (n, fn _ => TVvar (ref NONE))
                        in
                          ( unify (verify_tyval (ty_fn tyvars)) (ty_fn' tyvars)
                          ; SymDict.insert new_tynamedict tyname old
                          )
                        end
                  | (Abstract (n, absid), SOME old) =>
                      SymDict.insert new_tynamedict tyname old
                  | (_, NONE) =>
                      prog_err "cannot find type during signature matching"
                )
                SymDict.empty
                tyspecs

            (* For all the datatypes, check that they have matching
             * constructors, then populate the tydict.
             *
             * We also want all of the cons, so we can later add them to the
             * environment.
             *)
            val (cons, new_tydict) =
              SymDict.foldl
                (fn ( dtyname
                  , {arity = sig_arity, tyid = sig_tyid, cons = sig_cons}
                  , (cons, new_tydict)
                  ) =>
                  let
                    val mod_tyid = TyIdDict.lookup dtydict sig_tyid
                    val old as {arity = mod_arity, cons = mod_cons} =
                      TyIdDict.lookup tydict mod_tyid
                    val tyvars = List.tabulate (mod_arity, fn _ => TVvar (ref NONE))

                    (* Could be more efficient, but the number of constructors
                     * has gotta be real low.
                     *)
                    fun subset cons1 cons2 =
                      List.foldl
                        (fn ({id, tyscheme}, ()) =>
                          case List.find (fn {id = id', ...} => Symbol.eq (id, id')) cons2 of
                            NONE => prog_err "unmatched con in signature matching"
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
                      prog_err "arity does not match in signature matching"
                    else
                      ( subset mod_cons sig_cons
                      ; subset sig_cons mod_cons
                      ; ( cons_to_add @ cons
                        , TyIdDict.insert new_tydict mod_tyid old
                        )
                      )
                  end
                )
                ([], TyIdDict.empty)
                dtyspecs

            (* Populate the valtydict and identdict by iterating over all the
             * specifications for values.
             *)
            val (new_valtydict, new_identdict) =
              SymDict.foldl
                (fn (id, sig_tyscheme, (acc_valtydict, acc_identdict)) =>
                  let
                    val (_, mod_tyscheme) = SymDict.lookup valtydict id
                  in
                    (* Check that the types of the values are the same.
                     * If so, then we want to add this identifier with a value
                     * tag to the identdict and valtydict.
                     *)
                    ( eq_tyschemes mod_tyscheme sig_tyscheme
                    ; ( SymDict.insert
                          acc_valtydict
                          id
                          (Vsign, mod_tyscheme)
                      , case SymDict.find identdict id of
                          SOME (V value) =>
                            SymDict.insert
                              acc_identdict
                              id
                              (V value)
                          (* TODO: check this
                           * if it was originally a constructor or exception, just
                           * add it back as a constr for its val
                           *)
                        | SOME (C _ | E _) =>
                            SymDict.insert
                              acc_identdict
                              id
                              (V (Vconstr {id = [id], arg = NONE}))
                        | _ =>
                          prog_err
                            ("Failed to find value identifier " ^ lightblue (Symbol.toValue id)
                            ^ " during signature ascription"
                            )
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
                        prog_err
                          ("Failed to find exception " ^ lightblue (Symbol.toValue id)
                          ^ " during signature ascription"
                          )
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
              { identdict = new_identdict
              , valtydict = new_valtydict
              , moddict = new_moddict
              , infixdict = SymDict.empty
              , tydict = new_tydict
              , tynamedict = new_tynamedict
              }
          end


    (*
    fun validate_bind pat exp ctx =
      let
        val (bindings, pat_ty) = Statics.get_pat_bindings pat ctx
        val exp_ty = synth ctx exp
      in
        ( unify exp_ty pat_ty
        ; bindings
        )
      end
    *)

    (*
    fun synth_dec ctx dec =
      case dec of
        Dval {tyvars, valbinds} =>
          let
            (* These are all of the tyvars that are scoped at this val
             * declaration.
             *)
            val tyvars =
              SymSet.union
                tyvars
                ( List.map
                    (fn {pat, exp, ...} =>
                      SymSet.union
                        (collect_tyvars_pat pat)
                        (collect_tyvars exp)
                    )
                    valbinds
                  |> union_sets
                )

            val ctx =
              Context.add_scoped_tyvars ctx tyvars

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
              get_pat_list_ty_bindings
                ctx
                (List.map (fn {pat, ...} => pat) valbinds)

            (* If this is a recursive val binding, then we should evaluate the
             * type of the expression in the context of every mutually recursive
             * val declaration.
             * Otherwise, evaluate it in the original context.
             *)
            val exp_ctx =
              if is_rec then
                Context.add_ty_bindings ctx bindings
              else
                ctx

            (* Check that each purported pat type unifies with the expression's
             * type.
             *)
            val () =
              List.foldr
                (fn (({exp, ...}, pat_ty), ()) =>
                  ( unify (synth exp_ctx exp) pat_ty
                  ; ()
                  )
                )
                ()
          in
            Context.add_ty_bindings ctx bindings
          end
      | Dfun {tyvars, fvalbinds} =>

      | Dtype typbinds =>
          (* Because these typbinds happen "simultaneously", first we have to
           * figure out what type that they are each referring to, and then bind
           * them afterwards.
           *)
          List.map
            (fn typbind =>
              Context.get_type_synonym_binding ctx typbind
            )
            typbinds
          |> List.foldl (Context.add_type_synonym ctx) ctx
      | Ddatdec {datbinds, withtypee} =>
          let
            (* Generate new ty IDs for each datatype declaration.
             *)
            val ty_ids =
              List.map (fn {tycon, ...} => (tycon, TyId.new (SOME tycon))) datbinds

            (* For every withtype, evaluate that type to a tyval with the new
             * datatypes.
             *)
            val withtypee_bindings =
              List.map
                (fn {tyvars, tycon, ty} =>
                  let
                    val num = List.length tyvars
                    val enum_tyvars = enum tyvars

                    val datatype_fn =
                      fn (sym, tyvals) =>
                        List.find (fn (tycon, _) => Symbol.eq (tycon, sym)) ty_ids
                        |> Option.map (fn (tycon, id) => TVapp (tyvals, id))

                    fun mk_tyvar_fn tys =
                      let
                        val enum_tys = enum tys
                      in
                        fn sym =>
                          let
                            (* This is the index in the original tyvars list
                             * that the input symbol is.
                             *)
                            val idx = List.find (fn (_, sym') => Symbol.eq (sym, sym'))
                          in
                            List.find (fn (idx', _) => idx = idx') tys
                            |> (fn (_, ty) => ty)
                          end
                      end

                    fun mk_type_scheme tys =
                      if List.length tys <> num then
                        prog_err "Invalid number of tyargs to instantiate type scheme"
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

            (* Add the withtype bindings.
             *)
            val ctx =
              List.foldl
                (fn ((id, tyscheme), ctx) =>
                  Context.add_type_synonym ctx id (Scheme tyscheme)
                )
                ctx
                withtypee_bindings

            (* Add the datbinds.
             *)
            val ctx =
              List.foldl
                (fn (((_, tyid), datbind), ctx) =>
                  Context.add_datbind ctx (tyid, datbind)
                )
                ctx
                (ListPair.zipEq (ty_ids, datbinds,))
          in
            ctx
          end
      | Ddatrepl {left_tycon, right_tycon} =>
          Context.replicate_datatype ctx left_tycon right_tycon
      | Dabstype {datbinds, withtypee, withh} =>
          raise Fail "TODO"
      | Dexception exbinds =>
          List.foldl
            (fn (exbind, new_ctx) =>
              case exbind of
                Xnew {id, ty} =>
                  Context.add_exn new_ctx (id, (ExnId.new (SOME id),ty))
              | Xrepl {left_id, right_id, ...} =>
                  Context.add_exn new_ctx (id, (Context.get_exn_id ctx right_id))
            )
            ctx
            exbinds
      | Dlocal {left_dec, right_dec} =>
          ( case left_dec of
              Dseq decs =>
                List.foldl
                  (fn (dec, ctx) =>
                    synth_dec ctx dec
                  )
                  (Context.enter_scope ctx)
                  decs
                |> Context.enter_scope
                |> (fn ctx' => synth_dec ctx' right_dec)
                |> Context.exit_local
            | _ =>
                synth_dec
                  (Context.enter_scope ctx)
                  left_dec
                |> Context.enter_scope
                |> (fn ctx' => synth_dec ctx' right_dec)
                |> Context.exit_local
          )
      | Dopen ids =>
          List.foldl
            (fn (id, ctx) => Context.open_path ctx id)
            ctx
            ids
      | Dinfix {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id
                , LEFT
                , Option.getOpt (precedence, 0)
                )
            )
            ctx
            ids
      | Dinfixr {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id
                , RIGHT
                , Option.getOpt (precedence, 0)
                )
            )
            ctx
            ids
      | Dnonfix ids =>
          List.foldl
            (fn (id, ctx) =>
              Context.remove_infix ctx id
            )
            ctx
            ids
      | Dhole _ => raise Fail "shouldn't synth dhole"
                     *)

  end
