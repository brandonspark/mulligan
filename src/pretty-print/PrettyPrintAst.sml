(** Adapted from code by Sam Westrick
  *
  * See the file LICENSE for details.
  *)

open PrettyPrintContext
open SMLSyntax
open PrettySimpleDoc
open Location
open Printf
structure PD = PrettySimpleDoc
structure TC = TerminalColors

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is for everything related to pretty printing the SML AST.
 * This is crucially used for the debugger's trace output, since we need to
 * be able to show the current state of the program.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

fun curry f = fn x => fn y => f (x, y)

fun bool_to_option b default =
  if b then SOME default
  else NONE

fun map_nonfirst f l =
  case l of
    x::xs => x :: List.map f xs
  | [] => []

fun map_nonlast f l =
  List.rev (map_nonfirst f (List.rev l))

fun combine f default l =
  case l of
    [] => default
  | [x] => x
  | x::xs =>
      f (x, combine f default xs)

(*****************************************************************************)
(* Colors! *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Pretty-printing helpers *)
(*****************************************************************************)

infix 2 +++ ++ +-+ $$ // $$< //< ^^ \\

(* Juxtaposition *)
fun x ++ y = beside (x, y)

(* Space-separated juxtaposition *)
fun x +-+ y = x ++ space ++ y

(* Optionally space-separated juxtaposition *)
fun x +++ y = x ++ softspace ++ y

(* Newline or space-separated *)
fun x $$ y = aboveOrSpace (x, y)
fun x $$< y = y $$ x

(* Newline or juxtaposed *)
fun x // y = aboveOrBeside (x, y)
fun x //< y = y // x

fun spaces n =
  List.foldl op++ empty (List.tabulate (n, fn _ => space))

(* +-+ or indented newline *)
fun x \\ y =
  group (x $$ (spaces 2 ++ y))

val text_syntax = text lightblue
val text_literal = text brown
val text_lab = text blue

fun parensAround (x: doc) =
  group (text_syntax "(" +++ x // text_syntax ")")

fun surround left middle right =
  text_syntax left ++ middle ++ text_syntax right

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

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)

signature PRETTYPRINTAST =
  sig
    (* Pretty-printing an entire AST! *)
    val pretty: Context.t -> SMLSyntax.ast -> bool -> string

    (* Contextually pretty-printing an expression, which may be within
     * some larger context.
     *)
    val report :
      Context.t -> SMLSyntax.exp -> int -> Location.location list -> string

    val show_ctx : Context.t -> string
    val show_location : Context.t -> Location.location list -> string

    val show_value : Context.t -> Context.value -> string
    val show_pat : Context.t -> SMLSyntax.pat -> string
    val show_exp : Context.t -> SMLSyntax.exp -> string
    val show_tyval : SMLSyntax.tyval -> string
    val show_tyscheme : SMLSyntax.type_scheme -> string
    val show_longid : SMLSyntax.longid -> string

    (* Some format flags for printf.
    *)
    val ftv : (SMLSyntax.tyval -> 'a, 'b, 'state) Printf.t * string -> ('a, 'b, 'state) Printf.t
    val fp : (SMLSyntax.pat -> 'a, 'b, Context.t) Printf.t * string -> ('a, 'b, Context.t) Printf.t
    val fe : (SMLSyntax.exp -> 'a, 'b, Context.t) Printf.t * string -> ('a, 'b, Context.t) Printf.t
    val fv : (SMLSyntax.value -> 'a, 'b, Context.t) Printf.t * string -> ('a, 'b, Context.t) Printf.t
    val fl : (SMLSyntax.longid -> 'a, 'b, 'state) Printf.t * string -> ('a, 'b, 'state) Printf.t
  end

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

structure PrettyPrintAst : PRETTYPRINTAST =
struct
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
  fun combine_list _ _ [] = text_syntax ""
    | combine_list space smush (first::rest) =
    let
      val result = List.foldl (if space then op$$< else op//<) first rest
    in
      if smush then group result else result
    end

  fun p_list_base smush f delim all space l =
    combine_list space smush (apply_list_base f delim all l)

  (* p_list by default calls p_list_base with these settings:
   * - Group the combined list
   * - Do not newline-separate
   * - Delimiter is applied to not all elements *)
  fun p_list f delim l = p_list_base true f delim false false l

  (* Delim is put after all but the last element, instead of before all but the
   * first. *)
  fun p_list_after (f : 'a -> PD.doc) delim (l : 'a list) =
    case l of
      [] => text_syntax ""
    | [elem] => f elem
    | _ =>
      l
      |> List.map f
      |> map_nonlast (fn elem => elem ++ text_syntax delim)
      |> combine op// (text_syntax "")

  (* Suppose we have a nonempty list L : 'a list.
   * Suppose we also have a function mk : 'a -> bool -> doc.
   * Then we want to $$ all of the elements in the list together, where
   * `mk true` is applied to the first element, and `mk false` is applied to the
   * rest.
   * That's what this function does. *)
  fun p_list_mk mk l =
    List.foldl op$$<
      (mk true (List.nth (l, 0)))
      (List.map (mk false) (List.drop (l, 1)))

  fun p_seq mk first delim last l =
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

  (* Suppose we would like to show all the elements in a list with some
   * delimiter, in such a way that the first element is prepended with one
   * (spaced) string, and then all after are prepended with a different string.
   * That's what this does. *)
  fun p_list_prepend color smush first after f delim l =
    combine_list true smush
      (ListUtils.map_cons
        (curry (op +-+) (text color first))
        (curry (op +-+) (text color after))
        (apply_list_base f delim false l))

  fun p_symbol color = text color o Symbol.toValue

  fun p_char c = text_literal ("#\"" ^ Char.toString c ^ "\"")

  fun p_id color identifier = (p_symbol color) identifier

  fun p_longid color longid =
    let
      (* we want to display all but the last id in the longid as orange
       *)
      val mapped =
        List.tabulate
          ( List.length longid
          , fn i =>
              if i = List.length longid - 1 then
                p_symbol color (List.nth (longid, i))
              else
                p_symbol orange (List.nth (longid, i))
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

  val p_tyvar = p_id yellow

  fun p_tyvars tyvars =
    parensAround (
      p_list p_tyvar ", " tyvars
    )
  fun p_tyvars_option [] = NONE
    | p_tyvars_option [tyvar] = SOME (p_tyvar tyvar)
    | p_tyvars_option other =  SOME (p_tyvars other)

  fun p_setting (name, value) =
    group (
      p_id blue name ++ text_syntax "="
      $$
      p_id white value
    )
  fun p_settings settings =
    surround "{" (p_list p_setting ", " settings) "}"

  fun p_plugin (name, settings) =
    case settings of
      [] => p_id white name
    | _ => group (p_id white name $$ p_settings settings)
  fun p_plugins plugins =
    p_list p_plugin "," plugins

  local
    open SMLSyntax
    val color = green
    val p_tyvar = p_id yellow
    val p_id = p_id color
    val p_longid = p_longid color
  in
    fun p_ty ty = p_ty_ ty
    and p_ty_ ty_ =
      case ty_ of
        Tident longid =>
          p_longid longid
      | Ttyvar id =>
          p_tyvar id
      | Tapp (typarams, longid) =>
          (case typarams of
            [] => p_longid longid
          | [elem] => p_atty elem +-+ p_longid longid
          | _ =>
            surround "(" (p_list p_ty ", " typarams) ")"
            +-+ p_longid longid)
      | Tprod tys =>
          p_list p_ty " * " tys
      | Tarrow (t1, t2) =>
          group (
              p_atty t1 +-+ text_syntax "->"
              $$
              p_atty t2
          )
      | Trecord fields =>
          let
            val fields_doc =
              List.map
                (fn {lab, ty} =>
                  group (
                    p_symbol blue lab +-+ text_syntax ":"
                    $$
                    spaces 2 ++ p_ty ty
                  ))
                fields
          in
            group (
              surround "{" (p_list_after (fn x => x) ", " fields_doc) "}"
            )
          end
      | Tparens ty => p_atty ty
    and p_atty ty =
      case ty of
        ( Tident _
        | Ttyvar _
        | Tapp (([] | [_]), _)
        | Trecord _ ) =>
            p_ty ty
      | ( Tprod _ (* int * int *)
        | Tarrow _ (* int -> int *)
        | Tparens _ (* handled by Tparens case above *)
        | Tapp _ (* THINK: (int, int) either *)
        ) =>
            parensAround (p_ty ty)

    and p_tyval tyval =
      case tyval of
        TVtyvar id =>
          p_tyvar id
      | TVapp (tyvals, tyid) =>
          (case tyvals of
            [] => text color (TyId.show tyid)
          | [elem] => p_attyval elem +-+ text color (TyId.show tyid)
          | _ =>
            surround "(" (p_list p_tyval ", " tyvals) ")"
            +-+ text color (TyId.show tyid)
          )
      | TVprod tys =>
          p_list p_tyval " * " tys
      | TVarrow (left, right) =>
          let
            (* i no longer remember what this does
             *)

            fun p_side tyval =
              case tyval of
                TVprod _ => p_tyval tyval
              | _ => p_attyval tyval

            fun p_arrow l mark r =
              case (r, right) of
                (TVarrow (l', r'), _) =>
                  (if mark then
                    text_syntax "->" +-+ p_side l
                  else
                    spaces 3 ++ p_side l
                  )
                  $$ p_arrow l' true r'
              | (_, TVarrow _) =>
                  (if mark then
                    text_syntax "->" +-+ p_side l
                   else
                     spaces 3 ++ p_side l
                  )
                  $$ (text_syntax "->"  +-+ p_side r)
              | _ => p_side l +-+ text_syntax "->" +-+ p_side r
          in
            group (p_arrow left false right)
          end
      | TVrecord fields =>
        let
          val fields_doc =
            List.map
              (fn {lab, tyval} =>
                group (
                  p_symbol blue lab +-+ text_syntax ":"
                  $$
                  spaces 2 ++ p_tyval tyval
                ))
              fields
        in
          group (
            surround "{" (p_list_after (fn x => x) ", " fields_doc) "}"
          )
        end
      | TVvar (r as (_, ref NONE)) =>
          text color (Ref.show r)
      | TVvar (_, ref (SOME (Ty tyval))) =>
          p_tyval tyval
      | TVvar (_, ref (SOME (Rows fields))) =>
        let
          val fields_doc =
            List.map
              (fn {lab, tyval} =>
                group (
                  p_symbol blue lab +-+ text_syntax ":"
                  $$
                  spaces 2 ++ p_tyval tyval
                ))
              fields
        in
          group (
            surround
              "{"
              (p_list_after (fn x => x) ", " fields_doc
              ++
              text_syntax ", " +-+ text_syntax "..."
              )
              "}"
          )
        end
      | TVabs (tyvals, absid) =>
          (case tyvals of
            [] => text color (AbsId.show absid)
          | [elem] => p_attyval elem +-+ text color (AbsId.show absid)
          | _ =>
            surround "(" (p_list p_tyval ", " tyvals) ")"
            +-+ text color (AbsId.show absid)
          )

    and p_attyval tyval =
      case tyval of
        ( TVtyvar _
        | TVapp _
        | TVabs _
        | TVrecord _
        | TVvar (_, ref NONE)
        | TVvar (_, ref (SOME (Rows _)))) => p_tyval tyval
      | TVvar (_, ref (SOME (Ty tyval))) => p_tyval tyval
      | ( TVprod _
        | TVarrow _ ) => parensAround (p_tyval tyval)
  end

  local
    open SMLSyntax
    val color = red
    val p_id = p_id color
    val p_constr = p_longid heavyred
    val p_longid = p_longid color
    val text = text color
  in
    fun p_pat ctx pat = p_pat_ ctx pat
    and p_pat_ ctx pat_ =
      case pat_ of
        Pnumber n => text_literal (Int.toString n)
      | Pword s => text_literal ("0w" ^ Symbol.toValue s)
      | Pstring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Pchar c => p_char c
      | Pwild => text "_"
      | Pident {opp, id} =>
          if opp then
            text_syntax "op"
            +-+
            p_longid id
          else
            p_longid id
      | Precord patrows =>
          surround "{" (p_list_after (p_patrow ctx) ", " patrows) "}"
      | Pparens pat => p_atpat ctx pat
      | Punit => text "()"
      | Ptuple pats =>
          parensAround (p_list_after (p_pat ctx) ", " pats)
      | Plist pats =>
          surround "[" (p_list (p_pat ctx) ", " pats) "]"
      | Por pats =>
          surround "(" (p_list (p_pat ctx) " | " pats) ")"
      | Papp {opp, id, atpat} =>
          if opp then
            text_syntax "op"
            +-+
            p_constr id
            +-+
            p_atpat ctx atpat
          else
            p_constr id
            +-+
            p_atpat ctx atpat
      | Pinfix {left, id, right} =>
          p_atpat ctx left +-+ p_id id +-+ p_atpat ctx right
      | Ptyped {pat, ty} =>
          p_pat ctx pat
          +-+
          text_syntax ":"
          +-+
          p_ty ty
      | Playered {opp, id, ty, aspat} =>
          separateWithSpaces
            [ bool_to_option opp (text_syntax "op")
            , SOME (p_id id)
            , Option.map p_ty ty
            , SOME (text_syntax "as")
            , SOME (p_pat ctx aspat) ]

    and p_atpat ctx pat =
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
        ) => p_pat ctx pat
      | Pparens pat => p_atpat ctx pat

      | ( Pident {opp = true, ...}
        | Papp _
        | Pinfix _
        | Ptyped _
        | Playered _
        ) => parensAround (p_pat ctx pat)

    and p_patrow ctx patrow =
      case patrow of
        PRellipsis => text "..."
      | PRlab {lab, pat} =>
          group (
            p_symbol blue lab +-+ text_syntax "="
            $$
            p_pat ctx pat
          )
      | PRas {id, ty, aspat} =>
          separateWithSpaces
            [ SOME (p_id id)
            , Option.map (fn ty => text_syntax ":" +-+ p_ty ty) ty
            , Option.map (fn pat => text_syntax "as" +-+ p_pat ctx pat) aspat ]
  end

  fun p_conbind {opp, id, ty} =
    separateWithSpaces
      [ bool_to_option opp (text_syntax "op")
      , SOME (p_id heavyblue id)
      , Option.map (fn ty => text_syntax "of" +-+ p_ty ty) ty ]

  fun marker_set_of_list l =
    List.foldl
      (fn (x, acc) =>
        MarkerSet.insert acc x
      )
      MarkerSet.empty
      l

  datatype xdoc = DOC of PD.doc | DUPLE of PD.doc * PD.doc

  local
    open SMLSyntax
  in
    val p_number = fn
      Int i => text_literal (Int.toString i)
    | Word s => text_literal ("0w" ^ s)
    | Real r => text_literal (Real.toString r)

    fun p_fname_args ctx fname_args =
      case fname_args of
        Fprefix { opp, id, args } =>
          group (
            ( if opp then
                text_syntax "op" +-+ p_id white id
              else
                p_id white id
            )
            +-+ p_list_base true (p_atpat ctx) "" false true args
          )
      | Finfix {left, id, right} =>
          separateWithSpaces
            [ SOME (p_atpat ctx left)
            , SOME (p_id white id)
            , SOME (p_atpat ctx right)
            ]
      | Fcurried_infix {left, id, right, args} =>
          parensAround (
            separateWithSpaces
              [ SOME (p_pat ctx left)
              , SOME (p_id white id)
              , SOME (p_pat ctx right)
              ]
          )
          +-+ p_list_base true (p_pat ctx) "" false true args

    fun p_exp ctx exp = p_exp_ ctx exp
    and p_exp_ ctx exp_ =
      let
        val p_exp = p_exp_ ctx
        val p_atexp = p_atexp ctx
        val color = white
        val p_id = p_id color
        val p_longid = p_longid color
        val text = text color
      in
      case exp_ of
        Enumber n => p_number n
      | Eparens exp => p_atexp exp
      | Estring s => text_syntax "\"" ++ text_literal (Symbol.toValue s) ++ text_syntax "\""
      | Echar c => p_char c
      | Erecord fields =>
          p_seq
            (fn {lab, exp} =>
              separateWithSpaces
                [ SOME (p_symbol blue lab)
                , SOME (text_syntax "=")
                , SOME (p_exp exp) ] )
            "{"
            ","
            "}"
            fields
      | Eselect lab =>
          text_syntax "#" ++ p_symbol blue lab
      | Eunit => text "()"
      | Eident {opp, id} =>
          if not (Context.is_substitute ctx) then
            p_longid id
          else
            let
              fun p_val_ident id =
                (case Context.get_val_opt ctx id of
                  NONE => p_longid id
                  (* If it's a recursive function, don't substitute its definition
                   * in.
                   *)
                | SOME (Vfn {rec_env = SOME _, ...}) =>
                    p_longid id
                | SOME value => p_atvalue ctx value
                )
            in
              if opp then
                text_syntax "op" +-+ p_val_ident id
              else
                p_val_ident id
            end
      | Etuple exps =>
          p_seq p_exp "(" "," ")" exps
      | Elist exps =>
          p_seq p_exp "[" "," "]" exps
      | Eseq exps =>
          p_seq p_exp "(" ";" ")" exps
      | Elet {dec, exps} =>
          let
            val (dec_doc, bindings) = p_dec' ctx dec
            val new_ctx = Binding.remove_bindings ctx bindings
          in
            group (
              separateWithNewlines
                [ text_syntax "let"
                , spaces 2 ++ dec_doc
                , text_syntax "in"
                , spaces 2 ++ p_list (p_exp_ new_ctx) "; " exps
                , text_syntax "end"
                ]
            )
          end
      | Eapp {left = left as Eapp _, right} =>
          p_exp left \\ p_atexp right
      | Eapp {left = left as Eident {id, ...}, right = right as Etuple [e1, e2]} =>
          (case Context.get_val_opt ctx id of
            SOME (Vbasis {is_infix = true, name, function = _}) =>
              p_exp (Einfix {left = e1, id = name, right = e2})
          | _ =>
            p_exp left \\ p_atexp right
          )
      | Eapp {left, right} =>
          p_atexp left \\ p_atexp right
      | Einfix {left, id, right} =>
          let
            fun p_infix l i r =
              case (l, left) of
                (Einfix {left = l', id = id', right = r'}, _) =>
                  (p_infix l' id' r')
                  $$ (p_id id +-+ p_exp r)
              | (_, Einfix _) =>
                  p_exp l
                  $$ (p_id i +-+ p_exp r)
              | _ => p_exp l +-+ p_id i +-+ p_exp r
          in
            group (
              p_infix left id right
            )
          end
      | Etyped {exp, ty} =>
          p_exp exp +-+ text_syntax ":" +-+ p_ty ty
      | Eandalso {left, right} =>
          p_atexp left +-+ text_syntax "andalso" +-+ p_atexp right
      | Eorelse {left, right} =>
          p_atexp left +-+ text_syntax "orelse" +-+ p_atexp right
      | Ehandle {exp, matches} =>
          p_atexp exp +-+ text_syntax "handle"
          $$
          p_list (p_match ctx NONE) "| " matches
      | Eraise exp =>
          text_syntax "raise" +-+ p_exp exp
      | Eif {exp1, exp2, exp3} =>
          group (
              group (
                separateWithNewlines
                  [ text_syntax "if"
                  , p_exp exp1
                  , text_syntax "then"
                  , p_exp exp2
                  ]
              )
              $$
              group (
                text_syntax "else"
                $$
                p_exp exp3
              )
          )
      | Ewhile {exp1, exp2} =>
          group (
            group (
              text_syntax "while"
              $$
              p_exp exp1
            )
            $$
            group (
              text_syntax "do"
              $$
              p_exp exp2
            )
          )
      | Ecase {exp, matches} =>
          (case matches of
            [] => raise Fail "empty matches in Ecase"
          | _ =>
              group (
                  group(
                    text_syntax "case"
                    $$
                    (spaces 2 ++ p_exp exp)
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
                                p_pat ctx pat
                                +-+ text_syntax "=>"
                                $$ spaces 2 ++ p_exp exp
                              )
                          )
                        | ({pat, exp}, SOME acc) =>
                          SOME (
                            acc
                            $$ ( text_syntax "|"
                               +-+ p_pat ctx pat
                               +-+ text_syntax "=>"
                               $$ spaces 2 ++ p_exp exp)
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
            val first_match = List.nth (matches, 0)

            val inner =
              p_match ctx (SOME "fn") first_match
          in
            group (
              List.foldl
                  (fn (match, acc) =>
                    acc $$
                    ( space ++ p_match ctx (SOME "|") match )
                  )
                  inner
                  (List.drop (matches, 1))
            )
          end
      | Ehole => Context.get_hole_print_fn ctx ()
      end

    and p_atexp ctx exp =
      let
        val p_exp = p_exp_ ctx
        val color = white
        val p_longid = p_longid color
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
        ) => p_exp exp
      | Eident {opp = false, id} =>
          if not (Context.is_substitute ctx) then
            p_longid id
          else
            let
              fun p_val_ident id =
                (case Context.get_val_opt ctx id of
                  NONE => p_longid id
                  (* If it's a recursive function, don't substitute its definition
                   * in.
                   * TODO: we probably do want to be able to print this,
                   * but only the "one-level unrolling"
                   *)
                | SOME (Vfn {rec_env = SOME _, ...}) =>
                    p_longid id
                | SOME value => p_atvalue ctx value
                )
            in
              p_val_ident id
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
        | Ehole ) => parensAround (p_exp exp)
      end

    and p_exp_xdoc ctx exp =
      case exp of
        Etuple [e1, e2] => DUPLE (p_exp ctx e1, p_exp ctx e2)
      | _ => DOC (p_exp ctx exp)

    and p_value ctx value =
      let
        open Context
        val p_value = p_value ctx
        val color = white
        val p_id = p_id color
        val p_constr = p_longid heavyblue
        val text = text color
        val p_atvalue = p_atvalue ctx
      in
      case value of
        Vnumber n => p_number n
      | Vstring s => text_syntax "\"" ++ text_literal (Symbol.toValue s) ++ text_syntax "\""
      | Vchar c => p_char c
      | Vrecord fields =>
          p_seq
            (fn {lab, value} =>
              separateWithSpaces
                [ SOME (p_symbol blue lab)
                , SOME (text_syntax "=")
                , SOME (p_value value) ] )
            "{" "," "}"
            fields
      | Vselect lab =>
          text_syntax "#" ++ p_symbol blue lab
      | Vunit => text "()"
      | Vconstr {id, arg} =>
          ( case arg of
              NONE => p_constr id
            | SOME value => p_constr id +-+ p_atvalue value
          )
      | Vtuple values =>
          p_seq p_value "(" "," ")" values
      | Vlist values =>
          p_seq p_value "[" "," "]" values
      | Vinfix {left, id, right} =>
          group (
            p_value left $$ (p_id id +-+ p_value right)
          )
      | Vexn {name, arg, ...} =>
          (case arg of
            NONE => p_constr name
          | SOME value => p_constr name +-+ p_atvalue value
          )
      | Vfn {matches, env, ...} =>
          let
            val inner = group (p_match env (SOME "fn") (List.nth (matches, 0)))
          in
            case matches of
              [] => raise Fail "empty matches in fn"
            | _::tl =>
                group (
                  if List.length matches = 1 then
                    inner
                  else
                  inner $$
                  space ++ p_list_base false (p_match env NONE) "| " true true tl
                )
          end
      | Vbasis {name, ...} => p_id name
      end

    and p_atvalue ctx value =
      case value of
        ( Vnumber _
        | Vstring _
        | Vchar _
        | Vrecord _
        | Vunit
        | Vconstr {arg = NONE, ...}
        | Vexn {arg = NONE, ...}
        | Vselect _
        | Vtuple _
        | Vlist _
        | Vbasis _
        ) => p_value ctx value
      | ( Vconstr {arg = SOME _, ...}
        | Vexn {arg = SOME _, ...}
        | Vinfix _
        | Vfn _
        ) => parensAround (p_value ctx value)


    and p_match ctx prefix {pat, exp} =
      let
        val bindings = Binding.of_pat ctx pat
        val new_ctx = Binding.remove_bindings ctx bindings
      in
        group (
          ( case prefix of
              NONE => text_syntax ""
            | SOME thing => text_syntax thing ++ spaces 1
          )
          ++
            p_pat ctx pat +-+ text_syntax "=>"
            \\ p_exp new_ctx exp
        )
      end
    and p_rec_match_equal ctx {recc, pat, exp} =
      let
        val ctx =
          if recc then
            let
              val binded_ids = Binding.of_pat ctx pat
              val ctx = Binding.remove_bindings ctx binded_ids
            in
              ctx
            end
          else
            ctx
      in
        if recc then
          group (
            text_syntax "rec" +-+ p_pat ctx pat +-+ text_syntax "="
            $$
            p_exp ctx exp
          )
        else
          group (
            p_pat ctx pat +-+ text_syntax "="
            $$
            p_exp ctx exp
          )
      end

    (* Pretty-printing declarations might introduce bound identifiers,
     * in which case we should know what identifiers after this declaration
     * are no longer valid to pretty-print.
     *)
    and p_dec ctx dec = #1 (p_dec' ctx dec)
    and p_dec' ctx dec_ =
      let
        val color = purple
        val p_exn = p_id pink
        val p_long_exn = p_longid pink
        val p_id = p_id white
        val p_open = p_longid orange
        val p_longid = p_longid white
        val text = text color

        fun p_exbind exbind =
          case exbind of
            Xnew {opp, id, ty} =>
              if opp then
                separateWithSpaces
                  [ SOME (text_syntax "op")
                  , SOME (p_exn id)
                  , Option.map (fn ty => text_syntax "of" +-+ p_ty ty) ty ]
              else
                (case ty of
                  NONE => p_exn id
                | SOME ty =>
                    p_exn id +-+ text_syntax "of" +-+ p_ty ty)
          | Xrepl {opp, left_id, right_id} =>
              separateWithSpaces
                [ bool_to_option opp (text_syntax "op")
                , SOME (p_exn left_id)
                , SOME (text_syntax "=")
                , SOME (p_long_exn right_id) ]

        fun p_typbind {tyvars, tycon, ty} =
          separateWithSpaces
            [ p_tyvars_option tyvars
            , SOME (p_id tycon)
            , SOME (text_syntax "=")
            , SOME (p_ty ty)
            ]

        fun p_datbind str mark {tyvars, tycon, conbinds} =
          case conbinds of
            [] => raise Fail "empty conbinds in datbind"
          | hd::tl =>
              group (
                separateWithSpaces
                  [ SOME (text (if mark then str else "and"))
                  , p_tyvars_option tyvars
                  , SOME (p_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                (spaces 2 ++ (p_conbind hd))
                $$
                p_list_prepend color false "|" "|" p_conbind "" tl
              )
      in
      case dec_ of
        (* TODO: we should probably still have the `rec` around here... *)
        Dval {tyvars, valbinds} =>
          (case valbinds of
            [] => raise Fail "empty valbinds"
          | hd::tl =>
              let
                val bindings =
                  List.map (Binding.of_pat ctx o #pat) valbinds
                  |> union_sets

                (* If any of them are `rec`, then the matches need to be
                 * evaluated in the context of all of their bindings. *)
                val new_ctx =
                  if List.foldl
                       (fn (x, y) => x orelse y)
                       false
                       (List.map #recc valbinds)
                  then
                    Binding.remove_bindings ctx bindings
                  else
                    ctx
              in
                ( List.foldl op$$<
                    (group (
                      separateWithSpaces
                        [ SOME (text "val")
                        , p_tyvars_option tyvars
                        , SOME (p_rec_match_equal new_ctx hd) ]
                    ))
                    (List.map
                      (fn match => text "and" +-+ p_rec_match_equal new_ctx match)
                      tl
                    )
                , bindings
                )
              end
          )
      | Dfun {tyvars = _, fvalbinds} =>
          (* TODO: pretty print tyvars *)
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

            val mutual_ctx = Binding.remove_bindings ctx func_names

            fun mk { fname_args, ty, exp } =
              let
                val binded_ids = Binding.of_fname_args ctx fname_args
                val new_ctx = Binding.remove_bindings mutual_ctx binded_ids
              in
                group (
                  separateWithSpaces
                    [ SOME (p_fname_args mutual_ctx fname_args)
                    , Option.map (fn ty => text_syntax ":"  +-+ p_ty ty) ty
                    , SOME (text_syntax "=") ]
                  $$
                  p_exp new_ctx exp
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
              p_list_mk mk_fun fvalbinds

          in
            (group fun_docs, func_names)
          end
      | Dtype typbinds =>
          p_list_prepend color false "type" "and" p_typbind "" typbinds
          |> inject empty_set
      | Ddatdec {datbinds, withtypee} =>
          let
            val datdecs =
              p_list_mk (p_datbind "datatype") datbinds
          in
            ( case withtypee of
                NONE => datdecs
              | SOME typbinds =>
                  group (
                    datdecs
                    $$
                    group (
                      p_list_prepend color false "withtype" "and" p_typbind "" typbinds
                    )
                  )
            , empty_set
            )
          end
      | Ddatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (p_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (p_longid right_tycon) ]
          |> inject empty_set
      | Dabstype {datbinds, withtypee, withh} =>
          let
            val typdecs =
              p_list_mk (p_datbind "abstype") datbinds

            val (with_doc, with_bindings) =
              p_dec' ctx withh

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
                    p_list_prepend color false "withtype" "and" p_typbind "" typbinds
                    $$
                    withh
                  )
            )
            |> inject with_bindings
          end
      | Dexception exbinds =>
          group (
            p_list_prepend color false "exception" "and" p_exbind "" exbinds
          )
          |> inject empty_set
      | Dlocal {left_dec, right_dec} =>
          let
            val (left_dec_doc, left_bindings) = p_dec' ctx left_dec
            val ctx = Binding.remove_bindings ctx left_bindings
            val (right_dec_doc, right_bindings) = p_dec' ctx right_dec
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
            |> inject right_bindings
          end
      | Dopen longids =>
          let
            val bindings =
              union_sets
                (List.map (Binding.of_modname ctx) longids)
          in
            text_syntax "open" +-+ p_list p_open " " longids
            |> inject bindings
          end
      | Dseq [] => raise Fail "empty dseq"
      | Dseq [x] => p_dec' ctx x
      | Dseq decs =>
          List.foldl
            (fn (dec, (acc, ctx, acc_boundids)) =>
              let
                val (dec_doc, bindings) = p_dec' ctx dec
              in
                ( (case acc of NONE => SOME dec_doc | SOME acc => SOME (acc $$ dec_doc))
                , Binding.remove_bindings ctx bindings
                , MarkerSet.union acc_boundids bindings
                )
              end
            )
            (NONE, ctx, empty_set)
            decs
          |> (fn (doc, _, boundids) => (Option.getOpt (doc, text_syntax ""), boundids))
      | Dinfix {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infix")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (p_list p_id " " ids) ]
          |> inject empty_set
      | Dinfixr {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infixr")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (p_list p_id " " ids) ]
          |> inject empty_set
      | Dnonfix ids =>
          (text_syntax "nonfix"
          ++
          p_list p_id " " ids
          )
          |> inject empty_set
      | Dhole => ( Context.get_hole_print_fn ctx (), empty_set )
      end
  end

  local
    open SMLSyntax
  in
    fun p_strdec ctx strdec = #1 (p_strdec' ctx strdec)
    and p_strdec' ctx strdec_ =
      let
        val color = orange
        val p_id = p_id color
      in
      case strdec_ of
        DMdec dec =>
          p_dec' ctx dec
      | DMstruct body =>
          let
            fun mk mark {id, seal, module} =
              group (
                group (
                  separateWithSpaces
                    [ SOME (if mark then text_syntax "structure" else text_syntax "and")
                    , SOME (p_id id)
                    , Option.map (fn {opacity, signat} =>
                        case opacity of
                          Transparent => text_syntax ":" +-+ p_signat signat
                        | Opaque => text_syntax ":>" +-+ p_signat signat) seal
                    , SOME (text_syntax "=") ]
                )
                $$
                spaces 2 ++ (p_module ctx) module
              )
          in
            p_list_mk mk body
            |> inject (marker_set_of_list (List.map (PrettyPrintContext.MOD o #id) body))
          end
      | DMlocal {left_dec, right_dec} =>
          let
            val (left_dec_doc, left_bindings) = p_strdec' ctx left_dec
            val ctx = Binding.remove_bindings ctx left_bindings
            val (right_dec_doc, right_bindings) = p_strdec' ctx right_dec
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
            |> inject right_bindings
          end
      | DMseq strdecs =>
          List.foldl
            (fn (strdec, (acc, ctx, acc_boundids)) =>
              let
                val (strdec_doc, bindings) = p_strdec' ctx strdec
              in
                ( case acc of NONE => SOME strdec_doc | SOME acc => SOME (acc $$ strdec_doc)
                , Binding.remove_bindings ctx bindings
                , MarkerSet.union acc_boundids bindings
                )
              end
            )
            ( NONE, ctx, empty_set )
            strdecs
          |> (fn (doc, _, bindings) => (Option.getOpt (doc, text_syntax ""), bindings))
      | DMhole => ( Context.get_hole_print_fn ctx (), empty_set )
      end

    and p_module ctx module = p_module_ ctx module
    and p_module_ ctx module_ =
      let
        val color = orange
        val p_id = p_id color
        val p_longid = p_longid color
      in
      case module_ of
        Mident longid => p_longid longid
      | Mstruct strdec =>
          group (
            text_syntax "struct"
            $$
            spaces 2 ++ (p_strdec ctx) strdec
            $$
            text_syntax "end"
          )
      | Mseal {module, opacity, signat} =>
          separateWithSpaces
            [ SOME (p_module ctx module)
            , SOME (case opacity of Transparent => text_syntax ":" | _ => text_syntax ":>")
            , SOME (p_signat signat) ]
      | Mapp {functorr, arg} =>
          group (
            p_id functorr +-+ text_syntax "("
            ++
            (case arg of
              Normal_app module => (p_module ctx) module
            | Sugar_app strdec => (p_strdec ctx) strdec
            )
            ++
            text_syntax ")"
          )
      | Mlet {dec, module} =>
          let
            val (strdec_doc, bindings) = p_strdec' ctx dec
            val ctx = Binding.remove_bindings ctx bindings
          in
            group (
              separateWithNewlines
                [ text_syntax "let"
                , spaces 2 ++ strdec_doc
                , text_syntax "in"
                , spaces 2 ++ p_module ctx module
                , text_syntax "end"
                ]
            )
          end
      | Mhole => Context.get_hole_print_fn ctx ()
      end

    and p_signat signat = p_signat_ signat
    and p_signat_ signat_ =
      let
        val color = orange
        val p_id = p_id color
        val p_longid = p_longid color
      in
      case signat_ of
        Sspec spec =>
          group (
            text_syntax "sig"
            $$
            spaces 2 ++ p_spec spec
            $$
            text_syntax "end"
          )
      | Sident id =>
          p_id id
      | Swhere {signat, wheretypee} =>
          let
            fun p_wheretypee mark {tyvars, id, ty} =
              group (
                separateWithSpaces
                  [ SOME (if mark then text_syntax "where type"
                          else text_syntax "and type")
                  , p_tyvars_option tyvars
                  , SOME (p_longid id)
                  , SOME (text_syntax "=") ]
                  $$
                  p_ty ty
              )
          in
            group (
              p_signat signat
              $$
              text_syntax "where type"
              $$
              p_list_mk p_wheretypee wheretypee
            )
          end
        end

    and p_spec spec = p_spec_ spec
    and p_spec_ spec_ =
      let
        val color = purple
        val text = text color
        val p_id = p_id white
        val p_longid = p_longid white
        fun p_typdesc {tyvars, tycon, ty} =
          separateWithSpaces
            [ p_tyvars_option tyvars
            , SOME (p_id tycon)
            , Option.map (fn ty => text_syntax "=" +-+ p_ty ty) ty
            ]

        fun p_datbind str mark {tyvars, tycon, conbinds} =
          case conbinds of
            [] => raise Fail "empty conbinds"
          | hd::tl =>
              group (
                separateWithSpaces
                  [ SOME (text (if mark then str else "and"))
                  , p_tyvars_option tyvars
                  , SOME (p_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                (spaces 2 ++ (p_conbind hd))
                $$
                p_list_prepend color false "|" "|" p_conbind "" tl
              )
      in
      case spec_ of
        SPval valbinds =>
          let
            fun mk_valbind mark {id, ty} =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "val" else text "and")
                  , SOME (p_id id)
                  , SOME (text_syntax ":") ]
                $$
                spaces 2 ++ p_ty ty
              )
          in
            p_list_mk mk_valbind valbinds
          end
      | SPtype typdescs =>
          let
            fun mk_typdesc mark typdesc =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "type" else text "and")
                  , SOME (p_typdesc typdesc)
                  ]
              )
          in
            p_list_mk mk_typdesc typdescs
          end
      | SPeqtype typdescs =>
          let
            fun mk_typdesc mark {tyvars, tycon} =
              group (
                separateWithSpaces
                  [ SOME (if mark then text "eqtype" else text "and")
                  , p_tyvars_option tyvars
                  , SOME (p_id tycon)
                  ]
              )
          in
            p_list_mk mk_typdesc typdescs
          end
      | SPdatdec datdescs =>
        (* Conbinds look like condescs, but with an extra "opp".
          * Elaborate the condescs to degenerate conbinds to make things
          * easier.
          *)
        p_list_mk (p_datbind "datatype")
          (List.map
            (fn {tyvars, tycon, condescs} =>
              { tyvars = tyvars
              , tycon = tycon
              , conbinds =
                  List.map (fn {id, ty} => {opp=false, id=id, ty=ty}) condescs
              }
            ) datdescs
          )
      | SPdatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (p_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (p_longid right_tycon) ]
      | SPexception exndescs =>
          let
            fun mk_exndesc mark {id, ty} =
              separateWithSpaces
                [ SOME (if mark then text "exception" else text "and")
                , SOME (p_id id)
                , Option.map (fn ty => text_syntax "of" +-+ p_ty ty) ty ]
          in
            p_list_mk mk_exndesc exndescs
          end
      | SPmodule moddescs =>
          let
            fun mk_moddesc mark {id, signat} =
              separateWithSpaces
                [ SOME (if mark then text "structure" else text "and")
                , SOME (p_id id)
                , SOME (text_syntax ":")
                , SOME (p_signat signat) ]
          in
            p_list_mk mk_moddesc moddescs
          end
      | SPinclude signat =>
          text_syntax "include" +-+ p_signat signat
      | SPinclude_ids ids =>
          text_syntax "include" +-+ p_list p_id " " ids
      | ( SPsharing {spec, tycons}
        | SPsharing_type {spec, tycons }
        ) =>
          group (
            p_spec spec
            $$
            spaces 2 ++ text_syntax "sharing type"
            $$
            group (spaces 2 ++ p_list p_longid " = " tycons)
          )
      | SPseq specs =>
          p_list p_spec " " specs
      end

    and p_sigbinds sigbinds =
      let
        val color = orange
        val text = text color
        val p_id = p_id color
        fun mk mark {id, signat} =
          group (
            separateWithSpaces
              [ SOME (if mark then text_syntax "signature" else text "and")
              , SOME (p_id id)
              , SOME (text_syntax "=") ]
            $$
            spaces 2 ++ p_signat signat
          )
      in
        p_list_mk mk sigbinds
      end

    and p_sigdec sigdec = p_sigbinds sigdec
  end

  fun p_funbinds ctx binds =
    let
      val color = orange
      val p_id = p_id color
      val text = text white

      fun mk mark {id, funarg, seal, body} =
        let
          val bindings = Binding.of_funarg ctx funarg
          val new_ctx = Binding.remove_bindings ctx bindings
        in
          group (
            separateWithSpaces
              [ SOME (if mark then text "functor" else text "and")
              , SOME (p_id id)
              , SOME
                  ( parensAround
                    (case funarg of
                      Normal {id, signat} =>
                        p_id id +-+ text_syntax ":" +-+ p_signat signat
                    | Sugar spec =>
                        p_spec spec
                    )
                  )
              , Option.map (fn {signat, opacity} =>
                  case opacity of
                    Transparent => text_syntax ":" +-+ p_signat signat
                  | Opaque => text_syntax ":>" +-+ p_signat signat)
                seal
              , SOME (text_syntax "=") ]
            $$
            p_module new_ctx body
          )
        end
    in
      p_list_mk mk binds
    end

  fun p_fundec ctx node = p_funbinds ctx node

  local
    open SMLSyntax
  in
    fun p_topdec ctx topdec = #1 (p_topdec' ctx topdec)

    and p_topdec' ctx topdec =
      case topdec of
        Strdec strdec => p_strdec' ctx strdec
      | Sigdec sigdec => (p_sigdec sigdec, empty_set)
      | Fundec fundec => (p_fundec ctx fundec, empty_set)
      | Thole => ( Context.get_hole_print_fn ctx (), empty_set )

    fun p_ast ctx ast =
      List.foldl
        (fn (topdec, (acc, ctx, acc_boundids)) =>
          let
            val (topdec_doc, bindings) = p_topdec' ctx topdec
          in
            ( case acc of
                NONE => SOME (topdec_doc)
              | SOME acc => SOME (acc $$ topdec_doc)
            , Binding.remove_bindings ctx bindings
            , MarkerSet.union acc_boundids bindings
            )
          end
        )
        (NONE, ctx, empty_set)
        ast
      |> (fn (doc, _, boundids) =>
            case doc of
              NONE => (text_syntax "", boundids)
            | SOME doc => (doc, boundids))
  end

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

  local
    open Context
  in

    fun flatten_doc doc =
      case doc of
        DOC doc => doc
      | DUPLE (doc1, doc2) =>
          p_seq (fn x => x) "(" "," ")" [doc1, doc2]

    fun report ctx acc_ids (doc : xdoc) n location : PD.doc =
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

         (* THINK: This is probably unnecessarily complicated, and would be
          * better if we did a proper naming set-up.
          *)

        (* This new ctx contains the print function for the current doc.
         *)
        val ctx = Context.add_hole_print_fn ctx (fn () => flatten_doc doc)
        val new_ctx = Binding.remove_bindings ctx acc_ids

        fun handle_exp n ctx exp_doc rest =
          ( case ((!(#print_dec (Context.get_settings ctx)))
            orelse (!(#print_all (Context.get_settings ctx))), n) of
            (true, _) =>
              report
                ctx
                empty_set
                exp_doc
                n (* eagerly go until you find a non-ehole *)
                rest
          | (false, 0) => flatten_doc doc
          | (false, n) =>
              report
                ctx
                empty_set
                exp_doc
                (n - 1)
                rest
          )
      in
         case (n, location) of
          (_, CLOSURE ctx :: rest) =>
            report
              ctx
              empty_set
              doc
              n
              rest
         | (_, EHOLE (e_app as Eapp {left = Eident {opp = false, id = [sym]}, right = Ehole}) :: rest) =>
            (case doc of
              DUPLE (d1, d2) =>
                (* This logic exists so that the pretty printer can know whether
                 * it should print out an infix operator as an infix expression.
                 *
                 * This is hard to deal with, because our hole structure usually
                 * assumes that you can just "fill in the blank".

                 * If one of our locations was, for instance,
                 * ^ <*>
                 * then we have an issue, because we really want to rewrite this
                 * in a way where we have finer grained control over what the
                 * hole prints as.
                 *
                 * So we introduce this DUPLE notion of an xdoc, and we consume
                 * that information here. But we only print if we are printing
                 * out that far, and if the operator is actually infix.
                 *
                 * This won't work properly for functions which are not infix
                 * in the basis, but that's OK.
                 *)
                if (n >= 1 orelse !(#print_dec (Context.get_settings ctx))) andalso
                  ( case Context.get_val_opt ctx [sym] of
                      SOME (Vbasis {is_infix, ...}) => is_infix
                    | _ => false
                  )
                then
                  handle_exp n ctx (DOC (group (d1 +-+ p_id white sym +-+ d2))) rest
                else
                  handle_exp n ctx (p_exp_xdoc ctx e_app) rest
            | _ =>
                handle_exp n ctx (p_exp_xdoc ctx e_app) rest
            )
         | (_, EHOLE exp :: rest) =>
            handle_exp n ctx (p_exp_xdoc ctx exp) rest
         | (_, DVALBINDS (recc, tyvars, pat, valbinds) :: rest) =>
            let
              val valbinds =
                { recc = recc, pat = pat, exp = Ehole }
                :: valbinds

              val (doc', bindings) =
                p_dec' ctx (Dval { tyvars = tyvars, valbinds = valbinds })
            in
              case (Context.get_settings ctx, n) of
                ({ print_all = ref true, ... }, _ ) =>
                  report
                    ctx
                    bindings
                    (DOC doc')
                    n
                    rest
              | ({ print_dec = ref true, ... }, _) => doc'
              | ({ print_dec = ref false, ... }, 0) => flatten_doc doc
              | _ =>
                report
                  ctx
                  bindings
                  (DOC doc')
                  (n - 1)
                  rest
            end
         | (0, _) =>
            if !(#print_all (Context.get_settings ctx)) then
              (* terrible, horrible hack here which simulates
                 an endless printing.

                 in reality, it should be a sum type...
               *)
              report ctx acc_ids doc 1 location
            else
              flatten_doc doc
         | (_, []) => flatten_doc doc
         | (n, ELET exps :: rest) =>
            let
              val exp = Elet {dec = Dhole, exps = exps}
            in
              report
                ctx
                empty_set
                (p_exp_xdoc new_ctx exp)
                (n - 1)
                rest
            end
        | (n, DLOCAL (decs, dec) :: rest) =>
            let
              val dec = Dlocal {left_dec = Dseq (Dhole :: decs), right_dec = dec}

              val (doc, bindings) =
                p_dec' new_ctx dec
            in
              report
                ctx
                bindings
                (DOC doc)
                (n - 1)
                rest
            end
        | (n, DSEQ decs :: rest) =>
            let
              val (doc, bindings) = p_dec' new_ctx (Dseq (Dhole :: decs))
            in
              report
                ctx
                (union acc_ids bindings)
                (DOC doc)
                (n - 1)
                rest
            end
        | (n, DMLOCAL (strdecs, strdec) :: rest) =>
            let
              val strdec = DMlocal {left_dec = DMseq (DMhole :: strdecs), right_dec = strdec}

              val (doc, bindings) =
                p_strdec' new_ctx strdec
            in
              report
                ctx
                bindings
                (DOC doc)
                (n - 1)
                rest
            end
        | (n, DMSEQ strdecs :: rest) =>
            let
              val (doc, bindings) =
                p_strdec' new_ctx (DMseq (DMhole :: strdecs))
            in
              report
                ctx
                (union acc_ids bindings)
                (DOC doc)
                (n - 1)
                rest
            end
        | (n, MLET module :: rest) =>
            let
              val doc =
                p_module new_ctx (Mlet {dec = DMhole, module = module})
            in
              report
                ctx
                empty_set
                (DOC doc)
                (n - 1)
                rest
            end
        | (n, MSTRUCT :: rest) =>
            let
              val doc =
                p_module ctx (Mstruct DMhole)
            in
              report ctx empty_set (DOC doc) n rest
            end
        | (n, MSEAL {opacity, signat} :: rest) =>
            let
              val doc =
                p_module
                  ctx
                  (Mseal {module = Mhole, opacity = opacity, signat = signat})
            in
              report ctx empty_set (DOC doc) n rest
            end
        | (n, MAPP sym :: rest) =>
            let
              val doc =
                group (
                  p_id orange sym +-+ text_syntax "("
                  ++
                  flatten_doc doc
                  ++
                  text_syntax ")"
                )
            in
              report ctx empty_set (DOC doc) (n - 1) rest
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
              val (doc, bindings) =
                p_strdec' ctx strdec
            in
              report new_ctx bindings (DOC doc) (n - 1) rest
            end
        | (n, FBODY _ :: rest) => (* TODO? *) report ctx empty_set doc n rest
        | (n, [PROG topdecs]) =>
          let
            val (doc, bindings) = p_ast ctx (Thole :: topdecs)
          in
            report
              ctx
              bindings
              (DOC doc)
              (n - 1)
              []
          end
        | (_, PROG _ :: _) => raise Fail "PROG not outermost"
      end

    val report = fn ctx => fn exp => fn n => fn location =>
      let
        val xdoc =
          case (p_exp_xdoc ctx exp) of
            DOC doc => DOC (PD.bold doc)
          | DUPLE (d1, d2) => DUPLE (PD.bold d1, PD.bold d2)
      in
        PD.toString
          true
          (report ctx empty_set xdoc n location)
      end
  end

  fun p_longid' longid =
    case longid of
      [] => raise Fail "empty longid"
    | [sing] => text white (Symbol.toValue sing)
    | hd::tl =>
        text orange (Symbol.toValue hd) ++ text_syntax "." ++ p_longid' tl

  fun pretty ctx ast b = PD.toString b (#1 (p_ast ctx ast))

  fun show_loc_atom ctx location =
    case location of
    (* EXP hole *)
      EHOLE exp => PD.toString true (p_exp ctx exp)
    | CLOSURE _ => "<CLOSURE>"
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

  fun show_location ctx location =
    "[" ^ String.concatWith ", " (List.map (show_loc_atom ctx) location) ^ "]"

  fun show_value ctx value = PD.toString true (p_value ctx value)
  fun show_exp ctx exp = PD.toString true (p_exp ctx exp)
  fun show_pat ctx pat = PD.toString true (p_pat ctx pat)

  fun show_id_info ctx = fn
    V value => show_value ctx value
  | C tyid => TyId.show tyid
  | E exnid => ExnId.show exnid

  fun show_ctx (ctx as {scope, outer_scopes, ...} : SMLSyntax.context) =
    let
      fun show_dict f d =
        "{" ^
        ( SymDict.toList d
          |> List.map (fn (id, x) => Symbol.toValue id ^ ": " ^ f x)
          |> String.concatWith ", "
        )
        ^ "}"

      fun show_scope (Scope {identdict, moddict, ...}) =
        "< identdict: " ^ show_dict (show_id_info ctx) identdict ^ ">"
        ^ "  moddict: " ^ show_dict (show_scope) moddict ^ "\n"
        ^ "  infixdict: "
    in
      String.concatWith "\n" (List.map show_scope (scope :: outer_scopes))
    end

  fun show_tyval tyval =
    PD.toString true (p_tyval (Context.norm_tyval (Basis.initial ()) tyval))

  fun show_tyscheme (arity, ty_fn) =
    (* We have to generate some type variables for the arity of
       this type scheme. It needs to be in alphabetical order,
       though.

       We just assume that practically there won't be a type scheme
       of more than 26 type variables. If so, tough luck.
      *)
    if arity = 0 then
      show_tyval (ty_fn [])
    else if arity > 26 then
      raise Fail "TODO"
    else
      List.tabulate (arity, fn i => i)
      |> List.map (fn num => Char.toString (Char.chr (97 + num)))
      |> List.map (fn s => "'" ^ s)
      |> List.map Symbol.fromValue
      |> List.map TVtyvar
      |> ty_fn
      |> Context.norm_tyval (Basis.initial ())
      |> show_tyval

  fun show_longid longid = PD.toString true (p_longid' longid)

  fun promote' f =
    fn ctx => fn x => f ctx x

  val op ftv = fn z => newFormat (fn _ => fn x => show_tyval (Context.norm_tyval (Basis.initial ()) x)) z
  val op fe = fn acc => newFormat (promote' show_exp) acc
  val op fv = fn acc => newFormat (promote' show_value) acc
  val op fp = fn acc => newFormat (promote' show_pat) acc
  val op fl = fn acc => newFormat (fn _ => fn x => show_longid x) acc

end
