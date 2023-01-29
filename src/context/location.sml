
structure Location =
  struct
    datatype location =
      (* EXP hole *)
        EHOLE of SMLSyntax.exp
      | CLOSURE of Context.t
          (* When you enter a function application, your entire context changes
           * to the closure of the function.
           * To prevent the pretty printer from applying this new context
           * backwards when reporting the greater context, we need to restore
           * this context once we exit the closrue.
           *)
      | DVALBINDS of
           bool
         * SMLSyntax.symbol list
         * SMLSyntax.pat
         * { recc : bool, pat : SMLSyntax.pat, exp : SMLSyntax.exp } list
      (* DEC hole *)
      | ELET of SMLSyntax.exp list
      | DLOCAL of SMLSyntax.dec list * SMLSyntax.dec
      | DSEQ of SMLSyntax.dec list
      (* STRDEC hole *)
      | DMLOCAL of SMLSyntax.strdec list * SMLSyntax.strdec
      | DMSEQ of SMLSyntax.strdec list
      | MLET of SMLSyntax.module
      (* MODULE hole *)
      | MSTRUCT
      | MSEAL of { opacity : SMLSyntax.opacity, signat : SMLSyntax.signat }
      | MAPP of SMLSyntax.symbol
      | STRUCTS of
           SMLSyntax.symbol
         * { opacity : SMLSyntax.opacity, signat : SMLSyntax.signat } option
         * { id : SMLSyntax.symbol
           , seal : {opacity : SMLSyntax.opacity, signat : SMLSyntax.signat } option
           , module : SMLSyntax.module
           } list
        (* special: just meant to say wtf is going on *)
      | FBODY of SMLSyntax.symbol
      (* TOPDEC hole *)
      | PROG of SMLSyntax.topdec list

    local
      open SMLSyntax
    in
      fun plug_dec_hole v dec_hole =
        case dec_hole of
          Dval {tyvars, valbinds} =>
            Dval { tyvars = tyvars
                 , valbinds =
                    List.map (fn {recc, pat, exp} => { recc = recc
                                                   , pat = pat
                                                   , exp = plug_exp_hole v exp
                                                   }
                             ) valbinds
                 }
        | Dlocal {left_dec, right_dec} =>
             Dlocal { left_dec = plug_dec_hole v left_dec
                    , right_dec = plug_dec_hole v right_dec
                    }
        | Dseq decs =>
            Dseq (List.map (plug_dec_hole v) decs)
        | ( Dfun _
          | Dtype _
          | Ddatdec _
          | Ddatrepl _
          | Dabstype _
          | Dexception _
          | Dopen _
          | Dinfix _
          | Dinfixr _
          | Dnonfix _
          | Dhole ) => dec_hole

      and plug_hole v location =
        case location of
          (EHOLE exp_hole :: rest) => (plug_exp_hole v exp_hole, rest)
          (* THINK: I no longer understand what this does. *)
        | (DVALBINDS _ :: _) => (Value.value_to_exp v, location)
        | (CLOSURE _ :: rest) => plug_hole v rest
        | ( ELET _
          | DLOCAL _
          | DSEQ _
          | DMLOCAL _
          | DMSEQ _
          | MLET _
          | MSTRUCT
          | MSEAL _
          | MAPP _
          | STRUCTS _
          | FBODY _
          | PROG _ ) :: _ => raise Fail "invalid value hole"
        | [] => raise Fail "invalid value hole"

      and plug_exp_hole v exp_hole =
        ( case exp_hole of
          ( Enumber _
          | Estring _
          | Echar _
          | Eselect _
          | Eunit
          | Eident _
          ) => exp_hole
        | Erecord fields =>
            Erecord
              (List.map (fn {lab, exp} => {lab = lab, exp = plug_exp_hole v exp}) fields)
        | Etuple exps =>
            Etuple (List.map (plug_exp_hole v) exps)
        | Elist exps =>
            Elist (List.map (plug_exp_hole v) exps)
        | Eseq exps =>
            Eseq (List.map (plug_exp_hole v) exps)
        | Elet {dec, exps} =>
            (* Shouldn't be any holes in exps. *)
            Elet { dec = plug_dec_hole v dec
                 , exps = exps
                 }
        | Eparens exp => Eparens (plug_exp_hole v exp)
        | Eapp {left, right} =>
            Eapp { left = plug_exp_hole v left
                 , right = plug_exp_hole v right
                 }
        | Einfix {left, id, right} =>
            Einfix { left = plug_exp_hole v left
                   , id = id
                   , right = plug_exp_hole v right
                   }
        | Etyped {exp, ty} =>
            Etyped { exp = plug_exp_hole v exp, ty = ty }
        | Eandalso {left, right} =>
            Eandalso { left = plug_exp_hole v left
                     , right = plug_exp_hole v right
                     }
        | Eorelse {left, right} =>
            Eorelse { left = plug_exp_hole v left
                     , right = plug_exp_hole v right
                     }
        | Ehandle {exp, matches} =>
            (* Shouldn't be any holes in matches. *)
            Ehandle { exp = plug_exp_hole v exp
                    , matches = matches
                    }
        | Eraise exp => Eraise (plug_exp_hole v exp)
        | Eif {exp1, exp2, exp3} =>
            (* Shouldn't be any holes in exp2 or exp3. *)
            Eif { exp1 = plug_exp_hole v exp1, exp2 = exp2, exp3 = exp3 }
        | Ewhile {exp1, exp2} =>
            (* Shouldn't be any holes in exp2. *)
            Ewhile { exp1 = plug_exp_hole v exp1, exp2 = exp2}
        | Ecase {exp, matches} =>
            (* Shouldn't be any holes in matches. *)
            Ecase { exp = plug_exp_hole v exp, matches = matches }
        | Efn _ => (* Shouldn't be any in a function. *) exp_hole
        | Ehole => Value.value_to_exp v
        )
    end

    fun is_val_dec location =
      case location of
        (DVALBINDS _ :: _) => true
      | (CLOSURE _ :: rest) => is_val_dec rest
      | _ => false
  end


