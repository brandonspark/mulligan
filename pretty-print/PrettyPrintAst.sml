(** Adapted from code by Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst :
sig
  val pretty: Context.t -> SMLSyntax.ast -> bool -> string
  val report :
    Context.t -> SMLSyntax.exp -> int -> Location.location list -> string
  val report_doc :
    Context.t -> PrettySimpleDoc.t -> int -> Location.location list -> string
  val show_value : Context.t -> Context.value -> PrettySimpleDoc.t
  val show_exp : Context.t -> SMLSyntax.exp -> PrettySimpleDoc.t
  val ctx_toString : Context.t -> string
  val location_toString : Context.t -> Location.location list -> string
end =
struct
  open PrettyPrintContext

  open SMLSyntax
  structure PD = PrettySimpleDoc
  open PD
  open Location

  structure TC = TerminalColors

  infix |>
  fun x |> f = f x

  fun inject y = fn x => (x, y)
  val empty_set = MarkerSet.empty
  fun union_sets l =
    List.foldl
      (fn (set, acc) =>
        MarkerSet.union set acc
      )
      MarkerSet.empty
      l
  val union = MarkerSet.union

  val white = TC.hsv {h=38.0, s=0.0, v=0.75}
  val orange = TC.hsv {h=35.0, s=0.79, v=0.9}
  val green = TC.hsv {h=128.0, s=0.43, v=0.9}
  val heavygreen = TC.hsv {h=117.0, s=1.0, v=0.26}
  val blue = TC.hsv {h=220.0, s=0.75, v=0.85}
  val heavyblue = TC.hsv {h=222.0, s=0.95, v=0.85}
  val lightblue = TC.hsv {h=200.0, s=0.45, v=0.83}
  val yellow = TC.hsv {h=55.0, s=0.81, v=0.90}
  val purple = TC.hsv {h=269.0, s=0.52, v=1.0}
  val pink = TC.hsv {h=300.0, s=0.61, v=0.9}
  val red = TC.hsv {h=0.0, s=0.72, v=0.8}
  val heavyred = TC.hsv {h=0.0, s=0.83, v=0.85}
  val cyan = TC.hsv {h=186.0, s=0.73, v=1.0}
  val brown = TC.hsv {h=21.0, s=0.82, v=0.70}


  infix 2 +++ ++ +-+ $$ // $$< //< ^^ \\
  fun x ++ y = beside (x, y)
  fun x +-+ y = x ++ space ++ y
  fun x +++ y = x ++ softspace ++ y
  fun x $$ y = aboveOrSpace (x, y)
  fun x $$< y = y $$ x
  fun x // y = aboveOrBeside (x, y)
  fun x //< y = y // x

  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))

  fun x \\ y =
    group (x $$ (spaces 2 ++ y))

  val text_syntax = text lightblue
  val text_literal = text brown
  val text_lab = text blue


  fun parensAround (x: doc) =
    group (text_syntax "(" +++ x // text_syntax ")")

  fun separateWithSpaces (items: doc option list) : doc =
    let
      val items: doc list = List.mapPartial (fn x => x) items
    in
      case items of
        [] => empty
      | first :: rest =>
          List.foldl (fn (next, prev) => prev +-+ next) first rest
    end

  fun separateWithNewlines items =
    List.foldr
      (fn (x, acc) => x $$ acc)
      empty
      items

  (* if all is true: apply delim to all elements, including first.
   * delim is prepended to any mapped elements. *)
  fun apply_list_base f delim all l =
    case l of
      [] => []
    | first::rest =>
        (if all then text_syntax delim ++ f first else f first)
        ::
        List.map (fn elem => text_syntax delim ++ f elem) rest

  fun apply_list_after_base f delim l =
    List.foldr
      (fn (elem, []) => [f elem]
      | (elem, acc) => (f elem ++ text_syntax delim) :: acc
      )
      []
      l

  (* Put the list of documents together in the right order. *)
  fun combine_list space smush [] = text_syntax ""
    | combine_list space smush (first::rest) =
    let
      val result = List.foldl (if space then op$$< else op//<) first rest
    in
      if smush then group result else result
    end

  fun show_list_base smush f delim all space l =
    combine_list space smush (apply_list_base f delim all l)

  (* show_list by default calls show_list_base with these settings:
   * - Group the combined list
   * - Do not newline-separate
   * - Delimiter is applied to not all elements *)
  fun show_list f delim l = show_list_base true f delim false false l

  (* Delim is put after all but the last element, instead of before all but the
   * first. *)
  fun show_list_after f delim l =
    case l of
      [] => text_syntax ""
    | [elem] => f elem
    | _ =>
      let
        val prelude_mapped =
          List.map (fn elem => f elem ++ text_syntax delim) (ListUtils.up_to_last l)
      in
        group (List.foldr op// (f (ListUtils.last l)) prelude_mapped)
      end

  (* Suppose we have a nonempty list L : 'a list.
   * Suppose we also have a function mk : 'a -> bool -> doc.
   * Then we want to $$ all of the elements in the list together, where
   * `mk true` is applied to the first element, and `mk false` is applied to the
   * rest.
   * That's what this function does. *)
  fun show_list_mk mk l =
    List.foldl op$$<
      (mk true (List.nth (l, 0)))
      (List.map (mk false) (List.drop (l, 1)))

  fun show_seq mk first delim last l =
    let
      val res =
        List.foldl
          (fn (elem, NONE) =>
              SOME (text_syntax first +++ mk elem)
          | (elem, SOME acc) =>
              SOME (acc // (text_syntax delim +-+ mk elem))
          )
          NONE
          l
    in
      case res of
        NONE => text_syntax first ++ text_syntax last
      | SOME ans => group (ans // text_syntax last)
    end

  fun curry f = fn x => fn y => f (x, y)

  (* Suppose we would like to show all the elements in a list with some
   * delimiter, in such a way that the first element is prepended with one
   * (spaced) string, and then all after are prepended with a different string.
   * That's what this does. *)
  fun show_list_prepend color smush first after f delim l =
    combine_list true smush (
      (ListUtils.map_cons
        (curry (op +-+) (text color first))
        (curry (op +-+) (text color after))
        (apply_list_base f delim false l))
    )

  fun show_symbol color = text color o Symbol.toValue
  fun show_symbol_node color = text color o Symbol.toValue

  fun show_char c = text_literal ("#\"" ^ Char.toString c ^ "\"")

  fun show_id color identifier = (show_symbol_node color) identifier

  fun show_longid color longid =
    let
      val mapped =
        List.tabulate
          ( List.length longid
          , fn i =>
              if i = List.length longid - 1 then
                show_symbol_node color (List.nth (longid, i))
              else
                show_symbol_node orange (List.nth (longid, i))
          )
    in
      case mapped of
        [] => raise Fail "empty longid"
      | hd::tl =>
          List.foldl
            (fn (elem, acc) => acc ++ text_syntax "." ++ elem)
            hd
            tl
    end

  val show_tyvar = show_id yellow

  fun show_tyvars tyvars =
    parensAround (
      show_list show_tyvar ", " tyvars
    )
  fun show_tyvars_option [] = NONE
    | show_tyvars_option [tyvar] = SOME (show_tyvar tyvar)
    | show_tyvars_option other =  SOME (show_tyvars other)

  fun bool_to_option b default =
    if b then SOME default
    else NONE

  fun show_setting (name, value) =
    group (
      show_id blue name ++ text_syntax "="
      $$
      show_id white value
    )
  fun show_settings settings =
    text_syntax "{" ++ show_list show_setting ", " settings ++ text_syntax "}"

  fun show_plugin (name, settings) =
    case settings of
      [] => show_id white name
    | _ => group (show_id white name $$ show_settings settings)
  fun show_plugins plugins =
    show_list show_plugin "," plugins

  local
    open SMLSyntax
    val color = green
    val show_tyvar = show_id yellow
    val show_id = show_id color
    val show_longid = show_longid color
  in
    fun show_ty ty = show_ty_ ty
    and show_ty_ ty_ =
      case ty_ of
        Tident longid =>
        show_longid longid
      | Ttyvar id =>
        show_tyvar id
      | Tapp (typarams, longid) =>
        (case typarams of
          [elem] => show_ty elem
        | _ =>
          text_syntax "(" ++ show_list show_ty ", " typarams ++ text_syntax ")")
        +-+
        show_longid longid
      | Tprod tys =>
        text_syntax "(" ++ show_list show_ty " * " tys ++ text_syntax ")"
      | Tarrow (t1, t2) =>
        group (
          parensAround (
            show_ty t1 +-+ text_syntax "->"
            $$
            show_ty t2
          )
        )
      | Trecord fields =>
        let
          val fields_doc =
            List.map
              (fn {lab, ty} =>
                group (
                  show_symbol_node blue lab +-+ text_syntax ":"
                  $$
                  spaces 2 ++ show_ty ty
                ))
              fields
        in
          group (
            text_syntax "{"
            ++
            show_list_after (fn x => x) ", " fields_doc
            ++
            text_syntax "}"
          )
        end
      | Tparens ty => parensAround (show_ty ty)
  end

  local
    open SMLSyntax
    val color = red
    val show_id = show_id color
    val show_constr = show_longid heavyred
    val show_longid = show_longid color
    val text = text color
  in
    fun show_pat ctx pat = show_pat_ ctx pat
    and show_pat_ ctx pat_ =
      case pat_ of
        Pnumber n => text_literal (Int.toString n)
      | Pword s => text_literal ("0w" ^ Symbol.toValue s)
      | Pstring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Pchar c => show_char c
      | Pwild => text "_"
      | Pident {opp, id} =>
          if opp then
            text_syntax "op"
            +-+
            show_longid id
          else
            show_longid id
      | Precord patrows =>
          text_syntax "{" ++ show_list_after (show_patrow ctx) ", " patrows ++ text_syntax "}"
      | Pparens pat => show_atpat ctx pat
      | Punit => text "()"
      | Ptuple pats =>
          parensAround (show_list_after (show_pat ctx) ", " pats)
      | Plist pats =>
          text_syntax "[" ++ show_list (show_pat ctx) ", " pats ++ text_syntax "]"
      | Por pats =>
          text_syntax "(" ++ show_list (show_pat ctx) " | " pats ++ text_syntax ")"
      | Papp {opp, id, atpat} =>
          if opp then
            text_syntax "op"
            +-+
            show_constr id
            +-+
            show_atpat ctx atpat
          else
            show_constr id
            +-+
            show_atpat ctx atpat
      | Pinfix {left, id, right} =>
          show_atpat ctx left +-+ show_id id +-+ show_atpat ctx right
      | Ptyped {pat, ty} =>
          show_pat ctx pat
          +-+
          text_syntax ":"
          +-+
          show_ty ty
      | Playered {opp, id, ty, aspat} =>
          separateWithSpaces
            [ bool_to_option opp (text_syntax "op")
            , SOME (show_id id)
            , Option.map show_ty ty
            , SOME (text_syntax "as")
            , SOME (show_pat ctx aspat) ]

    and show_atpat ctx pat =
      case pat of
        ( Pnumber _
        | Pword _
        | Pstring _
        | Pchar _
        | Pwild
        | Pident { opp = false, ...}
        | Precord _
        | Punit
        | Ptuple _
        | Plist _
        | Por _
        ) => show_pat ctx pat
      | Pparens pat => show_atpat ctx pat

      | ( Pident {opp = true, ...}
        | Papp _
        | Pinfix _
        | Ptyped _
        | Playered _
        ) => parensAround (show_pat ctx pat)

    and show_patrow ctx patrow =
      case patrow of
        PRellipsis => text "..."
      | PRlab {lab, pat} =>
          group (
            show_symbol_node blue lab +-+ text_syntax "="
            $$
            show_pat ctx pat
          )
      | PRas {id, ty, aspat} =>
          separateWithSpaces
            [ SOME (show_id id)
            , Option.map (fn ty => text_syntax ":" +-+ show_ty ty) ty
            , Option.map (fn pat => text_syntax "as" +-+ show_pat ctx pat) aspat ]

  end

  fun show_conbind {opp, id, ty} =
    separateWithSpaces
      [ bool_to_option opp (text_syntax "op")
      , SOME (show_id heavyblue id)
      , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]

  fun marker_set_of_list l =
    List.foldl
      (fn (x, acc) =>
        MarkerSet.insert acc x
      )
      MarkerSet.empty
      l

  local
    open SMLSyntax
  in
    val show_number = fn
      Int i => text_literal (Int.toString i)
    | Word s => text_literal ("0w" ^ s)
    | Real r => text_literal (Real.toString r)

    fun show_fname_args ctx fname_args =
      case fname_args of
        Fprefix { opp, id, args } =>
          group (
            ( if opp then
                text_syntax "op" +-+ show_id white id
              else
                show_id white id
            )
            +-+ show_list_base true (show_atpat ctx) "" false true args
          )
      | Finfix {left, id, right} =>
          separateWithSpaces
            [ SOME (show_atpat ctx left)
            , SOME (show_id white id)
            , SOME (show_atpat ctx right)
            ]
      | Fcurried_infix {left, id, right, args} =>
          parensAround (
            separateWithSpaces
              [ SOME (show_pat ctx left)
              , SOME (show_id white id)
              , SOME (show_pat ctx right)
              ]
          )
          +-+ show_list_base true (show_pat ctx) "" false true args

    fun show_exp ctx exp = show_exp_ ctx exp
    and show_exp_ ctx exp_ =
      let
        val show_exp = show_exp_ ctx
        val show_atexp = show_atexp ctx
        val color = white
        val show_id = show_id color
        val show_constr = show_longid heavyblue
        val show_longid = show_longid color
        val text = text color
      in
      case exp_ of
        Enumber n => show_number n
      | Eparens exp => show_atexp exp
      | Estring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Echar c => show_char c
      | Erecord fields =>
          show_seq
            (fn {lab, exp} =>
              separateWithSpaces
                [ SOME (show_symbol_node blue lab)
                , SOME (text_syntax "=")
                , SOME (show_exp exp) ] )
            "{"
            ","
            "{"
            fields
      | Eselect lab =>
          text_syntax "#" ++ show_symbol_node blue lab
      | Eunit => text "()"
      | Eident {opp, id} =>
          if not (Context.is_substitute ctx) then
            show_longid id
          else if Context.is_con ctx id then
            show_constr id
          else
            let
              fun show_val_ident id =
                (case Context.get_val_opt ctx id of
                  NONE => show_longid id
                  (* If it's a recursive function, don't substitute its definition
                   * in.
                   *)
                | SOME (Vfn {rec_env = SOME _, ...}) =>
                    show_longid id
                | SOME value => show_value ctx value
                )
            in
              if opp then
                text_syntax "op" +-+ show_val_ident id
              else
                show_val_ident id
            end
      | Etuple exps =>
          show_seq show_exp "(" "," ")" exps
      | Elist exps =>
          show_seq show_exp "[" "," "]" exps
      | Eseq exps =>
          show_seq show_exp "(" ";" ")" exps
      | Elet {dec, exps} =>
          let
            val (dec_doc, bound_ids) = show_dec' ctx dec
            val new_ctx = Binding.remove_bound_ids ctx bound_ids
          in
            group (
              separateWithNewlines
                [ text_syntax "let"
                , spaces 2 ++ dec_doc
                , text_syntax "in"
                , spaces 2 ++ show_list (show_exp_ new_ctx) "; " exps
                , text_syntax "end"
                ]
            )
          end
      | Eapp {left = left as Eapp _, right} =>
          show_exp left \\ show_atexp right
      | Eapp {left, right} =>
          show_atexp left \\ show_atexp right
      | Einfix {left, id, right} =>
          let
            fun show_infix l i r =
              case (l, left) of
                (Einfix {left = l', id = id', right = r'}, _) =>
                  (show_infix l' id' r')
                  $$ (show_id id +-+ show_exp r)
              | (_, Einfix _) =>
                  show_exp l
                  $$ (show_id i +-+ show_exp r)
              | _ => show_exp l +-+ show_id i +-+ show_exp r
          in
            group (
              show_infix left id right
            )
          end
      | Etyped {exp, ty} =>
          show_exp exp +-+ text_syntax ":" +-+ show_ty ty
      | Eandalso {left, right} =>
          show_atexp left +-+ text_syntax "andalso" +-+ show_atexp right
      | Eorelse {left, right} =>
          show_atexp left +-+ text_syntax "orelse" +-+ show_atexp right
      | Ehandle {exp, matches} =>
          show_atexp exp +-+ text_syntax "handle"
          $$
          show_list (show_match ctx) "| " matches
      | Eraise exp =>
          text_syntax "raise" +-+ show_exp exp
      | Eif {exp1, exp2, exp3} =>
          group (
              group (
                separateWithNewlines
                  [ text_syntax "if"
                  , show_exp exp1
                  , text_syntax "then"
                  , show_exp exp2
                  ]
              )
              $$
              group (
                text_syntax "else"
                $$
                show_exp exp3
              )
          )
      | Ewhile {exp1, exp2} =>
          group (
            group (
              text_syntax "while"
              $$
              show_exp exp1
            )
            $$
            group (
              text_syntax "do"
              $$
              show_exp exp2
            )
          )
      | Ecase {exp, matches} =>
          (case matches of
            [] => raise Fail "empty matches in Ecase"
          | hd::tl =>
              group (
                  group(
                    text_syntax "case"
                    $$
                    (spaces 2 ++ show_exp exp)
                    $$
                    text_syntax "of"
                  )
                  $$
                  ( case
                      List.foldl
                        (fn ({pat, exp}, NONE) =>
                          SOME (
                            spaces 2 ++
                              group (
                                show_pat ctx pat
                                +-+ text_syntax "=>"
                                $$ spaces 2 ++ show_exp exp
                              )
                          )
                        | ({pat, exp}, SOME acc) =>
                          SOME (
                            acc
                            $$ ( text_syntax "|"
                               +-+ show_pat ctx pat
                               +-+ text_syntax "=>"
                               $$ spaces 2 ++ show_exp exp)
                          )
                        )
                        NONE
                        matches
                    of
                      NONE => raise Fail "shouldn't happen"
                    | SOME ans => ans
                  )
              )
          )
      | Efn (matches, ctx_opt) =>
          let
            val ctx = Option.getOpt (ctx_opt, ctx)
            val {pat, exp} = List.nth (matches, 0)
            val inner =
              (text_syntax "fn" +-+ show_pat ctx pat +-+ text_syntax "=>")
              \\ show_exp exp
          in
            group (
                  ( List.foldl
                      (fn ({pat, exp}, acc) =>
                        acc $$
                        ( space ++
                            ( (text_syntax "|" +-+ show_pat ctx pat +-+ text_syntax "=>")
                              \\ show_exp exp
                            )
                        )
                      )
                      inner
                      (List.drop (matches, 1))
                  )
            )
            (*List.foldl
              (fn ((match, i), acc) =>
                if i = 0 then
                  text_syntax "fn" +-+ show_match ctx match
                else
                  acc $$
                  space ++ (text_syntax "| "

                show_match ctx match
                   *)
            (*case matches of
              [] => raise Fail "empty matches in fn"
            | hd::tl =>
                group
                  (text_syntax "(" ++
                    ( if List.length matches = 1 then
                      inner
                      else
                      ( space ++
                        ( inner $$
                        space ++ show_list_base false (show_match ctx) "| " true true tl
                        )
                      )
                    )
                  //
                  text_syntax ")"
                )
              *)
          end
      | Ehole => Context.get_hole_print_fn ctx ()
      end

    and show_atexp ctx exp =
      let
        val show_exp = show_exp_ ctx
        val show_atexp = show_atexp ctx
        val color = white
        val show_id = show_id color
        val show_constr = show_longid heavyblue
        val show_longid = show_longid color
        val gold = text yellow
        val text = text color
      in
      case exp of
        ( Enumber _
        | Estring _
        | Echar _
        | Erecord _
        | Eselect _
        | Eunit
        | Etuple _
        | Elist _
        | Eseq _
        | Eparens _
        ) => show_exp exp
      | Eident {opp = false, id} =>
          if not (Context.is_substitute ctx) then
            show_longid id
          else if Context.is_con ctx id then
            show_constr id
          else
            let
              fun show_val_ident id =
                (case Context.get_val_opt ctx id of
                  NONE => show_longid id
                  (* If it's a recursive function, don't substitute its definition
                   * in.
                   *)
                | SOME (Vfn {rec_env = SOME _, ...}) =>
                    show_longid id
                | SOME value => show_atvalue ctx value
                )
            in
              show_val_ident id
            end
      | ( Eident {opp = true, ...}
        | Elet _
        | Eapp _
        | Einfix _
        | Etyped _
        | Eandalso _
        | Efn _
        | Eorelse _
        | Ehandle _
        | Eraise _
        | Eif _
        | Ewhile _
        | Ecase _
        | Ehole ) => parensAround (show_exp exp)
      end

    and show_value ctx value =
      let
        open Context
        val show_value = show_value ctx
        val color = white
        val show_id = show_id color
        val show_constr = show_longid heavyblue
        val show_longid = show_longid color
        val text = text color
        val show_atvalue = show_atvalue ctx
      in
      case value of
        Vnumber n => show_number n
      | Vstring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Vchar c => show_char c
      | Vrecord fields =>
          show_seq
            (fn {lab, value} =>
              separateWithSpaces
                [ SOME (show_symbol_node blue lab)
                , SOME (text_syntax "=")
                , SOME (show_value value) ] )
            "{" "," "}"
            fields
      | Vselect lab =>
          text_syntax "#" ++ show_symbol_node blue lab
      | Vunit => text "()"
      | Vconstr {id, arg} =>
          ( case arg of
              NONE => show_constr id
            | SOME value => show_constr id +-+ show_atvalue value
          )
      | Vtuple values =>
          show_seq show_value "(" "," ")" values
      | Vlist values =>
          show_seq show_value "[" "," "]" values
      | Vinfix {left, id, right} =>
          group (
            show_value left $$ (show_id id +-+ show_value right)
          )
      | Vfn {matches, env, ...} =>
          let
            val inner = group (text_syntax "fn" +-+ (show_match env) (List.nth (matches, 0)))
          in
            case matches of
              [] => raise Fail "empty matches in fn"
            | hd::tl =>
                group (
                  if List.length matches = 1 then
                    inner
                  else
                  inner $$
                  space ++ show_list_base false (show_match env) "| " true true tl
                )
          end
      | Vbasis {name, ...} => show_id name
      end

    and show_atvalue ctx value =
      case value of
        ( Vnumber _
        | Vstring _
        | Vchar _
        | Vrecord _
        | Vunit
        | Vconstr {arg = NONE, ...}
        | Vselect _
        | Vtuple _
        | Vlist _
        | Vbasis _
        ) => show_value ctx value
      | ( Vconstr {arg = SOME _, ...}
        | Vinfix _
        | Vfn _
        ) => parensAround (show_value ctx value)


    and show_match ctx {pat, exp} =
      let
        val bound_ids = Binding.get_pat_bindings ctx pat
        val new_ctx = Binding.remove_bound_ids ctx bound_ids
      in
        group (
          show_pat ctx pat +-+ text_syntax "=>"
          $$
          show_exp new_ctx exp
        )
      end
    and show_rec_match_equal ctx {recc, pat, exp} =
      let
        val ctx =
          if recc then
            let
              val binded_ids = Binding.get_pat_bindings ctx pat
              val ctx = Binding.remove_bound_ids ctx binded_ids
            in
              ctx
            end
          else
            ctx
      in
        if recc then
          group (
            text_syntax "rec" +-+ show_pat ctx pat +-+ text_syntax "="
            $$
            show_exp ctx exp
          )
        else
          group (
            show_pat ctx pat +-+ text_syntax "="
            $$
            show_exp ctx exp
          )
      end
    and show_dec ctx dec = #1 (show_dec' ctx dec)
    and show_dec' ctx dec_ =
      let
        val rec_color = yellow
        val text_rec = text rec_color

        val color = purple
        val show_exn = show_id pink
        val show_long_exn = show_longid pink
        val show_id = show_id white
        val show_open = show_longid orange
        val show_longid = show_longid white
        val text = text color

        fun show_exbind exbind =
          case exbind of
            Xnew {opp, id, ty} =>
              if opp then
                separateWithSpaces
                  [ SOME (text_syntax "op")
                  , SOME (show_exn id)
                  , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
              else
                (case ty of
                  NONE => show_exn id
                | SOME ty =>
                    show_exn id +-+ text_syntax "of" +-+ show_ty ty)
          | Xrepl {opp, left_id, right_id} =>
              separateWithSpaces
                [ bool_to_option opp (text_syntax "op")
                , SOME (show_exn left_id)
                , SOME (text_syntax "=")
                , SOME (show_long_exn right_id) ]

        fun show_typbind {tyvars, tycon, ty} =
          separateWithSpaces
            [ show_tyvars_option tyvars
            , SOME (show_id tycon)
            , SOME (text_syntax "=")
            , SOME (show_ty ty)
            ]

        fun show_datbind str mark {tyvars, tycon, conbinds} =
          case conbinds of
            [] => raise Fail "empty conbinds in datbind"
          | hd::tl =>
              group (
                separateWithSpaces
                  [ SOME (text (if mark then str else "and"))
                  , show_tyvars_option tyvars
                  , SOME (show_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                (spaces 2 ++ (show_conbind hd))
                $$
                show_list_prepend color false "|" "|" show_conbind "" tl
              )
      in
      case dec_ of
        Dval {tyvars, valbinds} =>
          (case valbinds of
            [] => raise Fail "empty valbinds"
          | hd::tl =>
              let
                val bound_ids =
                  List.map (Binding.get_pat_bindings ctx o #pat) valbinds
                  |> union_sets

                (* If any of them are `rec`, then the matches need to be
                 * evaluated in the context of all of their bindings. *)
                val new_ctx =
                  if List.foldl
                       (fn (x, y) => x orelse y)
                       false
                       (List.map #recc valbinds)
                  then
                    Binding.remove_bound_ids ctx bound_ids
                  else
                    ctx
              in
                ( List.foldl op$$<
                    (group (
                      separateWithSpaces
                        [ SOME (text "val")
                        , show_tyvars_option tyvars
                        , SOME (show_rec_match_equal new_ctx hd) ]
                    ))
                    (List.map
                      (fn match => text "and" +-+ show_rec_match_equal new_ctx match)
                      tl
                    )
                , bound_ids
                )
              end
          )
      | Dfun {tyvars, fvalbinds} =>
          let
            fun get_fname_args_id fname_args =
              case fname_args of
                Fprefix {id, ...} => VAL id
              | Finfix {id, ...} => VAL id
              | Fcurried_infix {id, ...} => VAL id

            val func_names =
              List.map
                (fn fvalbind =>
                  get_fname_args_id (#fname_args (List.nth (fvalbind, 0)))
                )
                fvalbinds
              |> marker_set_of_list

            val mutual_ctx = Binding.remove_bound_ids ctx func_names

            fun mk { fname_args, ty, exp } =
              let
                val binded_ids = Binding.get_fname_args_bindings ctx fname_args
                val new_ctx = Binding.remove_bound_ids mutual_ctx binded_ids
              in
                group (
                  separateWithSpaces
                    [ SOME (show_fname_args mutual_ctx fname_args)
                    , Option.map (fn ty => text_syntax ":"  +-+ show_ty ty) ty
                    , SOME (text_syntax "=") ]
                  $$
                  show_exp new_ctx exp
                )
              end

            fun mk_fun mark clauses =
              case clauses of
                [] => raise Fail "empty clauses in fvalbind"
              | hd::tl =>
                  List.foldl op$$<
                    (group (
                      text (if mark then "fun" else "and")
                      +-+
                      mk hd
                    ))
                    (List.map
                      (fn elem => spaces 2 ++ text_syntax "|" +-+ mk elem)
                      tl
                    )
            val fun_docs =
              show_list_mk mk_fun fvalbinds

          in
            (group (fun_docs), func_names)
          end
      | Dtype typbinds =>
          show_list_prepend color false "type" "and" show_typbind "" typbinds
          |> inject empty_set
      | Ddatdec {datbinds, withtypee} =>
          let
            val datdecs =
              show_list_mk (show_datbind "datatype") datbinds
          in
            ( case withtypee of
                NONE => datdecs
              | SOME typbinds =>
                  group (
                    datdecs
                    $$
                    group (
                      show_list_prepend color false "withtype" "and" show_typbind "" typbinds
                    )
                  )
            , empty_set
            )
          end
      | Ddatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (show_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (show_longid right_tycon) ]
          |> inject empty_set
      | Dabstype {datbinds, withtypee, withh} =>
          let
            val typdecs =
              show_list_mk (show_datbind "abstype") datbinds

            val (with_doc, with_bound_ids) =
              show_dec' ctx withh

            val withh =
              group (
                text_syntax "with"
                $$
                with_doc
                $$
                text_syntax "end"
              )
          in
            ( case withtypee of
                NONE =>
                  group (typdecs $$ withh)
              | SOME typbinds =>
                  group (
                    typdecs
                    $$
                    show_list_prepend color false "withtype" "and" show_typbind "" typbinds
                    $$
                    withh
                  )
            )
            |> inject with_bound_ids
          end
      | Dexception exbinds =>
          group (
            show_list_prepend color false "exception" "and" show_exbind "" exbinds
          )
          |> inject empty_set
      | Dlocal {left_dec, right_dec} =>
          let
            val (left_dec_doc, left_bound_ids) = show_dec' ctx left_dec
            val ctx = Binding.remove_bound_ids ctx left_bound_ids
            val (right_dec_doc, right_bound_ids) = show_dec' ctx right_dec
          in
            group (
              text_syntax "local"
              $$
              spaces 2 ++ left_dec_doc
              $$
              text_syntax "in"
              $$
              spaces 2 ++ right_dec_doc
              $$
              text_syntax "end"
            )
            |> inject right_bound_ids
          end
      | Dopen longids =>
          let
            val bound_ids =
              Binding.open_bound_ids ctx longids
          in
            text_syntax "open" +-+ show_list show_open " " longids
            |> inject bound_ids
          end
      | Dseq [] => raise Fail "empty dseq"
      | Dseq [x] => show_dec' ctx x
      | Dseq decs =>
          List.foldl
            (fn (dec, (acc, ctx, acc_boundids)) =>
              let
                val (dec_doc, bound_ids) = show_dec' ctx dec
              in
                ( (case acc of NONE => SOME dec_doc | SOME acc => SOME (acc $$ dec_doc))
                , Binding.remove_bound_ids ctx bound_ids
                , MarkerSet.union acc_boundids bound_ids
                )
              end
            )
            (NONE, ctx, empty_set)
            decs
          |> (fn (doc, _, boundids) => (text_syntax "dseq" ++ Option.getOpt (doc, text_syntax ""), boundids))
      | Dinfix {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infix")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (show_list show_id " " ids) ]
          |> inject empty_set
      | Dinfixr {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infixr")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (show_list show_id " " ids) ]
          |> inject empty_set
      | Dnonfix ids =>
          (text_syntax "nonfix"
          ++
          show_list show_id " " ids
          )
          |> inject empty_set
      | Dhole => ( Context.get_hole_print_fn ctx (), empty_set )
      end
  end

  local
    open SMLSyntax
  in
    fun show_strdec ctx strdec = #1 (show_strdec' ctx strdec)
    and show_strdec' ctx strdec_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case strdec_ of
        DMdec dec =>
          show_dec' ctx dec
      | DMstruct body =>
          let
            fun mk mark {id, seal, module} =
              group (
                group (
                  separateWithSpaces
                    [ SOME (if mark then text_syntax "structure" else text_syntax "and")
                    , SOME (show_id id)
                    , Option.map (fn {opacity, signat} =>
                        case opacity of
                          Transparent => text_syntax ":" +-+ show_signat signat
                        | Opaque => text_syntax ":>" +-+ show_signat signat) seal
                    , SOME (text_syntax "=") ]
                )
                $$
                spaces 2 ++ (show_module ctx) module
              )
          in
            show_list_mk mk body
            |> inject (marker_set_of_list (List.map (PrettyPrintContext.MOD o #id) body))
          end
      | DMlocal {left_dec, right_dec} =>
          let
            val (left_dec_doc, left_bound_ids) = show_strdec' ctx left_dec
            val ctx = Binding.remove_bound_ids ctx left_bound_ids
            val (right_dec_doc, right_bound_ids) = show_strdec' ctx right_dec
          in
            group (
              separateWithNewlines
                [ text_syntax "local"
                , spaces 2 ++ left_dec_doc
                , text_syntax "in"
                , spaces 2 ++ right_dec_doc
                , text_syntax "end"
                ]
            )
            |> inject right_bound_ids
          end
      | DMseq [] => raise Fail "empty dmseq"
      | DMseq [strdec] => show_strdec' ctx strdec
      | DMseq strdecs =>
          List.foldl
            (fn (strdec, (acc, ctx, acc_boundids)) =>
              let
                val (strdec_doc, bound_ids) = show_strdec' ctx strdec
              in
                ( case acc of NONE => SOME strdec_doc | SOME acc => SOME (acc $$ strdec_doc)
                , Binding.remove_bound_ids ctx bound_ids
                , MarkerSet.union acc_boundids bound_ids
                )
              end
            )
            ( NONE, ctx, empty_set )
            strdecs
          |> (fn (doc, _, bound_ids) => (Option.getOpt (doc, text_syntax ""), bound_ids))
      | DMhole => ( Context.get_hole_print_fn ctx (), empty_set )
      end
    and show_module ctx module = show_module_ ctx module
    and show_module_ ctx module_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case module_ of
        Mident longid => show_longid longid
      | Mstruct strdec =>
          group (
            text_syntax "struct"
            $$
            spaces 2 ++ (show_strdec ctx) strdec
            $$
            text_syntax "end"
          )
      | Mseal {module, opacity, signat} =>
          separateWithSpaces
            [ SOME (show_module ctx module)
            , SOME (case opacity of Transparent => text_syntax ":" | _ => text_syntax ":>")
            , SOME (show_signat signat) ]
      | Mapp {functorr, arg} =>
          group (
            show_id functorr +-+ text_syntax "("
            ++
            (case arg of
              Normal_app module => (show_module ctx) module
            | Sugar_app strdec => (show_strdec ctx) strdec
            )
            ++
            text_syntax ")"
          )
      | Mlet {dec, module} =>
          let
            val (strdec_doc, bound_ids) = show_strdec' ctx dec
            val ctx = Binding.remove_bound_ids ctx bound_ids
          in
            group (
              separateWithNewlines
                [ text_syntax "let"
                , spaces 2 ++ strdec_doc
                , text_syntax "in"
                , spaces 2 ++ show_module ctx module
                , text_syntax "end"
                ]
            )
          end
      | Mhole => Context.get_hole_print_fn ctx ()
      end
    and show_signat signat = show_signat_ signat
    and show_signat_ signat_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case signat_ of
        Sspec spec =>
          group (
            text_syntax "sig"
            $$
            spaces 2 ++ show_spec spec
            $$
            text_syntax "end"
          )
      | Sident id =>
          show_id id
      | Swhere {signat, wheretypee} =>
          let
            fun show_wheretypee mark {tyvars, id, ty} =
              group (
                separateWithSpaces
                  [ SOME (if mark then text_syntax "where type"
                          else text_syntax "and type")
                  , show_tyvars_option tyvars
                  , SOME (show_longid id)
                  , SOME (text_syntax "=") ]
                  $$
                  show_ty ty
              )
          in
            group (
              show_signat signat
              $$
              text_syntax "where type"
              $$
              show_list_mk show_wheretypee wheretypee
            )
          end
        end
    and show_spec spec = show_spec_ spec
    and show_spec_ spec_ =
      let
        val color = purple
        val text = text color
        val show_id = show_id white
        val show_longid = show_longid white
        fun show_condesc {id, ty} =
          separateWithSpaces
            [ SOME (show_id id)
            , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
        fun show_typdesc {tyvars, tycon, ty} =
          separateWithSpaces
            [ show_tyvars_option tyvars
            , SOME (show_id tycon)
            , Option.map (fn ty => text_syntax "=" +-+ show_ty ty) ty
            ]

        fun show_datbind str mark {tyvars, tycon, conbinds} =
          case conbinds of
            [] => raise Fail "empty conbinds"
          | hd::tl =>
              group (
                separateWithSpaces
                  [ SOME (text (if mark then str else "and"))
                  , show_tyvars_option tyvars
                  , SOME (show_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                (spaces 2 ++ (show_conbind (hd)))
                $$
                show_list_prepend color false "|" "|" show_conbind "" tl
              )
      in
      case spec_ of
        SPval valbinds =>
          let
            fun mk_valbind mark {id, ty} =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "val" else text "and")
                  , SOME (show_id id)
                  , SOME (text_syntax ":") ]
                $$
                spaces 2 ++ show_ty ty
              )
          in
            show_list_mk mk_valbind valbinds
          end
      | SPtype typdescs =>
          let
            fun mk_typdesc mark typdesc =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "type" else text "and")
                  , SOME (show_typdesc typdesc)
                  ]
              )
          in
            show_list_mk mk_typdesc typdescs
          end
      | SPeqtype typdescs =>
          let
            fun mk_typdesc mark typdesc =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "eqtype" else text "and")
                  , SOME (show_typdesc typdesc)
                  ]
              )
          in
            show_list_mk mk_typdesc typdescs
          end
      | SPdatdec datdescs =>
          let
            fun mk {tyvars, tycon, condescs} =
              group (
                separateWithSpaces
                  [ SOME (text "datatype")
                  , show_tyvars_option tyvars
                  , SOME (show_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                show_list show_condesc "| " condescs
              )
          in
            (* Conbinds look like condescs, but with an extra "opp".
             * Elaborate the condescs to degenerate conbinds to make things
             * easier.
             *)
            show_list_mk (show_datbind "datatype")
              (List.map
                (fn {tyvars, tycon, condescs} =>
                  { tyvars = tyvars
                  , tycon = tycon
                  , conbinds =
                      List.map (fn {id, ty} => {opp=false, id=id, ty=ty}) condescs
                  }
                ) datdescs
              )
          end
      | SPdatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (show_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (show_longid right_tycon) ]
      | SPexception exndescs =>
          let
            fun mk_exndesc mark {id, ty} =
              separateWithSpaces
                [ SOME (if mark then text "exception" else text "and")
                , SOME (show_id id)
                , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
          in
            show_list_mk mk_exndesc exndescs
          end
      | SPmodule moddescs =>
          let
            fun mk_moddesc mark {id, signat} =
              separateWithSpaces
                [ SOME (if mark then text "structure" else text "and")
                , SOME (show_id id)
                , SOME (text_syntax ":")
                , SOME (show_signat signat) ]
          in
            show_list_mk mk_moddesc moddescs
          end
      | SPinclude signat =>
          text_syntax "include" +-+ show_signat signat
      | SPinclude_ids ids =>
          text_syntax "include" +-+ show_list show_id " " ids
      | ( SPsharing {spec, tycons}
        | SPsharing_type {spec, tycons }
        ) =>
          group (
            show_spec spec
            $$
            spaces 2 ++ text_syntax "sharing type"
            $$
            group (spaces 2 ++ show_list show_longid " = " tycons)
          )
      | SPseq specs =>
          show_list show_spec " " specs
      end
    and show_sigbinds sigbinds =
      let
        val color = orange
        val text = text color
        val show_id = show_id color
        val show_longid = show_longid color
        fun mk mark {id, signat} =
          group (
            separateWithSpaces
              [ SOME (if mark then text_syntax "signature" else text "and")
              , SOME (show_id id)
              , SOME (text_syntax "=") ]
            $$
            spaces 2 ++ show_signat signat
          )
      in
        show_list_mk mk sigbinds
      end

    and show_sigdec sigdec = show_sigbinds sigdec
  end

  fun show_funbinds ctx binds =
    let
      val color = orange
      val show_id = show_id color
      val show_longid = show_longid color
      val text = text white

      fun mk mark {id, funarg, seal, body} =
        let
          val bound_ids = Binding.get_funarg_bound_ids ctx funarg
          val new_ctx = Binding.remove_bound_ids ctx bound_ids
        in
          group (
            separateWithSpaces
              [ SOME (if mark then text "functor" else text "and")
              , SOME (show_id id)
              , SOME
                  ( parensAround
                    (case funarg of
                      Normal {id, signat} =>
                        show_id id +-+ text_syntax ":" +-+ show_signat signat
                    | Sugar spec =>
                        show_spec spec
                    )
                  )
              , Option.map (fn {signat, opacity} =>
                  case opacity of
                    Transparent => text_syntax ":" +-+ show_signat signat
                  | Opaque => text_syntax ":>" +-+ show_signat signat)
                seal
              , SOME (text_syntax "=") ]
            $$
            show_module new_ctx body
          )
        end
    in
      show_list_mk mk binds
    end

  fun show_fundec ctx node = show_funbinds ctx node

  local
    open SMLSyntax
  in
    fun show_topdec ctx topdec = #1 (show_topdec' ctx topdec)

    and show_topdec' ctx topdec =
      case topdec of
        Strdec strdec => show_strdec' ctx strdec
      | Sigdec sigdec => (show_sigdec sigdec, empty_set)
      | Fundec fundec => (show_fundec ctx fundec, empty_set)
      | Thole => ( Context.get_hole_print_fn ctx (), empty_set )

    fun show_ast ctx ast =
      List.foldl
        (fn (topdec, (acc, ctx, acc_boundids)) =>
          let
            val (topdec_doc, bound_ids) = show_topdec' ctx topdec
          in
            ( acc $$ topdec_doc
            , Binding.remove_bound_ids ctx bound_ids
            , MarkerSet.union acc_boundids bound_ids
            )
          end
        )
        (text_syntax "", ctx, empty_set)
        ast
      |> (fn (doc, _, boundids) => (doc, boundids))
  end

  local
    open Context
  in

    fun report ctx acc_ids doc n location =
      let
        (* This hole may contain declarations, which may in turn affect where we
         * currently are. Consider the following example:
         *
         * <assume there was a binding of x up here somewhere>
         * let
         *   val x = <hole>
         * in
         *   x + 2
         * end
         *
         * This may have a location which looks like the following:
         * [EHOLE <exp>, DVALBINDS ([], "x", []), ELET <x + 2>]
         *
         * Since the pretty printer is contextual, it wants to use the bindings
         * that it currently knows about to substitute for known variables.
         *
         * However, it can't use whatever value of `x` is currently in the
         * context at hole-time, since that value of `x` will be shadowed with
         * the current declaration.
         *
         * So as we traverse out of the locations, we have to know what bindings
         * will be killed when printing the surroundings of the hole.
         *)

        (* This new ctx contains the print function for the current doc.
         *)
        val ctx = Context.add_hole_print_fn ctx (fn () => doc)
        val new_ctx = Binding.remove_bound_ids ctx acc_ids
      in
         case (n, location) of
          (_, CLOSURE ctx :: rest) =>
            report
              (Context.add_hole_print_fn ctx (fn () => doc))
              empty_set
              doc
              n
              rest
         | (_, EHOLE exp :: rest) =>
              ( case (!(#print_dec (Context.get_settings ctx)), n) of
                  (true, _) =>
                    report
                      ctx
                      empty_set
                      (show_exp ctx exp)
                      n (* eagerly go until you find a non-ehole *)
                      rest
                | (false, 0) => doc
                | (false, n) =>
                    report
                      ctx
                      empty_set
                      (show_exp ctx exp)
                      (n - 1)
                      rest
              )
         | (_, DVALBINDS (recc, tyvars, pat, valbinds) :: rest) =>
            let
              val valbinds =
                { recc = recc, pat = pat, exp = Ehole }
                :: valbinds

              val (doc', bound_ids) =
                show_dec' ctx (Dval { tyvars = tyvars, valbinds = valbinds })
            in
              case (!(#print_dec (Context.get_settings ctx)), n) of
                (true, _) => doc'
              | (false, 0) => doc
              | _ =>
                report
                  ctx
                  bound_ids
                  doc'
                  (n - 1)
                  rest
            end
         | (0, _) => doc
         | (n, []) => doc
         | (n, ELET exps :: rest) =>
            let
              val exp = Elet {dec = Dhole, exps = exps}
            in
              report
                ctx
                empty_set
                (show_exp new_ctx exp)
                (n - 1)
                rest
            end
        | (n, DLOCAL (decs, dec) :: rest) =>
            let
              val dec = Dlocal {left_dec = Dseq (Dhole :: decs), right_dec = dec}

              val (doc, bound_ids) =
                show_dec' new_ctx dec
            in
              report
                ctx
                bound_ids
                doc
                (n - 1)
                rest
            end
        | (n, DSEQ decs :: rest) =>
            let
              val (doc, bound_ids) = show_dec' new_ctx (Dseq (Dhole :: decs))
            in
              report
                ctx
                (union acc_ids bound_ids)
                doc
                (n - 1)
                rest
            end
        | (n, DMLOCAL (strdecs, strdec) :: rest) =>
            let
              val strdec = DMlocal {left_dec = DMseq (DMhole :: strdecs), right_dec = strdec}

              val (doc, bound_ids) =
                show_strdec' new_ctx strdec
            in
              report
                ctx
                bound_ids
                doc
                (n - 1)
                rest
            end
        | (n, DMSEQ strdecs :: rest) =>
            let
              val (doc, bound_ids) =
                show_strdec' new_ctx (DMseq (DMhole :: strdecs))
            in
              report
                ctx
                (union acc_ids bound_ids)
                doc
                (n - 1)
                rest
            end
        | (n, MLET module :: rest) =>
            let
              val doc =
                show_module new_ctx (Mlet {dec = DMhole, module = module})
            in
              report
                ctx
                empty_set
                doc
                (n - 1)
                rest
            end
        | (n, MSTRUCT :: rest) =>
            let
              val doc =
                show_module ctx (Mstruct DMhole)
            in
              report ctx empty_set doc n rest
            end
        | (n, MSEAL {opacity, signat} :: rest) =>
            let
              val doc =
                show_module
                  ctx
                  (Mseal {module = Mhole, opacity = opacity, signat = signat})
            in
              report ctx empty_set doc n rest
            end
        | (n, MAPP sym :: rest) =>
            let
              val doc =
                group (
                  show_id orange sym +-+ text_syntax "("
                  ++
                  doc
                  ++
                  text_syntax ")"
                )
            in
              report ctx empty_set doc (n - 1) rest
            end
        | (n, STRUCTS (id, seal, modules) :: rest) =>
            let
              val strdec =
                DMstruct
                  ( { id = id
                    , seal = seal
                    , module = Mhole
                    } :: modules
                  )
              val (doc, bound_ids) =
                show_strdec' ctx strdec
            in
              report new_ctx bound_ids doc (n - 1) rest
            end
        | (n, FBODY sym :: rest) => (* TODO? *) report ctx empty_set doc n rest
        | (n, [PROG topdecs]) =>
          let
            val (doc, bound_ids) = show_ast ctx (Thole :: topdecs)
          in
            report
              ctx
              bound_ids
              doc
              (n - 1)
              []
          end
        | (n, PROG topdecs :: _) => raise Fail "PROG not outermost"
      end

    val report_doc = fn ctx => fn doc => fn n => fn location =>
      PrettySimpleDoc.toString true (report ctx empty_set doc n location)
    val report = fn ctx => fn exp => fn n => fn location =>
      PrettySimpleDoc.toString
        true
        (report ctx empty_set (PrettySimpleDoc.bold (show_exp ctx exp)) n location)
  end

  fun pretty ctx ast b = PrettySimpleDoc.toString b (#1 (show_ast ctx ast))

  fun ctx_toString (ctx as {scope, outer_scopes, ...}) =
    let
      fun dict_toString f d =
        "{" ^
        ( SymDict.toList d
          |> List.map (fn (id, x) => Symbol.toValue id ^ ": " ^ f x)
          |> String.concatWith ", "
        )
        ^ "}"

      fun set_toString f s =
        "{" ^ (String.concatWith ", " (List.map f (SymSet.toList s))) ^ "}"

      val show_doc = PrettySimpleDoc.toString true

      fun scope_toString (Scope {valdict, condict, exndict, moddict, infixdict, tydict}) =
        "< valdict: " ^ dict_toString (show_doc o show_value ctx) valdict ^ ">"
      (* ^ "  condict: " ^ set_toString Symbol.toValue condict ^ "\n"
      ^ "  exndict: " ^ set_toString Symbol.toValue exndict ^ "\n"
      ^ "  moddict: " ^ dict_toString scope_toString moddict ^ "\n"
      ^ "  infixdict:
       *)
    in
      String.concatWith "\n" (List.map scope_toString (scope :: outer_scopes))
    end

  fun loc_toString ctx location =
    case location of
    (* EXP hole *)
      EHOLE exp => PrettySimpleDoc.toString true (show_exp ctx exp)
    | CLOSURE ctx => "<CLOSURE>"
        (* When you enter a function application, your entire context changes
         * to the closure of the function.
         * To prevent the pretty printer from applying this new context
         * backwards when reporting the greater context, we need to restore
         * this context once we exit the closrue.
         *)
    | DVALBINDS _ => "DVALBINDS"

    (* DEC hole *)
    | ELET _ => "ELET"
    | DLOCAL _ => "DLOCAL"
    | DSEQ _ => "DSEQ"
    (* STRDEC hole *)
    | DMLOCAL _ => "DMLOCAL"
    | DMSEQ _ => "DMSEQ"
    | MLET _ => "MLET"
    (* MODULE hole *)
    | MSTRUCT => "MSTRUCT"
    | MSEAL _ => "MSEAL"
    | MAPP _ => "MAPP"
    | STRUCTS _ => "STRUCTS"
      (* special: just meant to say wtf is going on *)
    | FBODY _ => "FBODY"
    (* TOPDEC hole *)
    | PROG _ => "PROG"

  fun location_toString ctx location =
    "[" ^ String.concatWith ", " (List.map (loc_toString ctx) location) ^ "]"
end
