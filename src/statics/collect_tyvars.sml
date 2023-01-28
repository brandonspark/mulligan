
structure CollectTyvars =
  struct
    open Common
    open SMLSyntax

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
          (List.concat o List.map (fn {lab = _, tyval} => collect_tyvars_tyval tyval)) fields
      | TVvar (i, r as ref NONE) => [Unconstrained (i, r)]
      | TVvar (_, ref (SOME (Ty tyval))) =>
          collect_tyvars_tyval tyval
      | TVvar (_, ref (SOME (Rows fields))) =>
          (List.concat o List.map (fn {lab = _, tyval} => collect_tyvars_tyval tyval)) fields

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
            (collect_tyvars_pat aspat)
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
            (fn {lab = _, exp} => collect_tyvars exp)
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
        Fprefix {args, ...} =>
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
  end
