
structure Ast =
  struct
    (* This is meant to be a stripped-down AST as compared to parse-sml. We need
     * just enough information to step the expression, in a way which is
     * relatively syntactically faithful to the original.
     *)

    datatype exp =
        Econst of constant
      | Eident of { opp: bool, id: symbol list }
      | Erecord of { lab: symbol, exp: exp } list
      | Eselect of symbol
      | Eunit
      | Etuple of exp list
      | Elist of exp list
      | Eseq of exp list
      | Elet of { dec: dec, exps: exp list }
      | Eparen of exp
      | Eapp of exp * exp
      | Einfix of { left: exp, id: symbol, right: exp }
      | Etyped of { exp: exp, ty: ty }
      | Eandalso of exp * exp
      | Eorelse of exp * exp
      | Ewhile of exp * exp
      | Ehandle of { exp: exp, matches: (pat * exp) list }
      | Eraise of exp
      | Eif of exp * exp * exp
      | Ecase of { exp: exp, elems : (pat * exp) list }
      | Efn of (pat * exp) list

    and constant of
        Cint of int
      | Cword of string
      | Creal of string
      | Cchar of char
      | Cstring of string

    and dec =
        Dempty
      | Dval of { tyvars: symbol list
                , valbinds: { recc: bool, pat: pat, exp: exp} list
                }
      | Dfun of { tyvars: symbol list
                , fvalbind: fvalbind
                }
      | Dtype of typbind
      | Ddatdec of { datbind: datbind, withtypee: typbind option }
      | Ddatrepl of { left_id: symbol, right_id: symbol list }
      | Dabstype of { datbind: datbind, withtypee: typbind option, dec: dec }
      | Dexception of exbind list
      | Dlocal of { left_idec : dec, right_dec : dec }
      | Dopen of symbol list
      | Dseq of { elems: dec list, delims: symbol option list }
      | Dinfix of { precedence: symbol option, elems: symbol list }
      | Dinfixr of { precedence: symbol option, elems: symbol list }
      | Dnonfix of symbol list

    and ty =
        Ttyvar of symbol
      | Trecord of (symbol * ty) list
      | Ttuple of ty list
      | Tapp of ty list * symbol list
      | Tarrow of ty * ty
      | Tparens of ty

    and pat =
        Pwild
      | Pconst of symbol
      | Punit
      | Pident of { opp: bool, id: sym }
      | Pconstr of { opp: bool, id: sym list }
      | Plist of pat list
      | Ptuple of pat list
      | Precord of

    and exbind =
        Xnew of { id: symbol, arg: ty option }
      | Xrepl of { left_id: symbol, right_id : symbol list }

    withtype fvalbind =
      { fname_args: fname_args
      , ty: ty option
      , exp: exp
      } list list

    and typbind = { tyvars: symbol list, tycon: symbol, ty: ty } list

    and datbind = { tyvars: symbol list, elems: {opp: bool, arg: ty option} list } list

    open AstType

    local
      open Exp
      fun translate_constant tok =
        case Token.getClass tok of
          Token.IntegerConstant =>
            Token.toString tok
            |> Int.fromString
            |> Option.valOf
            |> Cint
        | Token.WordConstant =>
            Cword (Token.toString tok)
        | Token.RealConstant =>
            Token.toString tok
            |> Real.fromString
            |> Option.valOf
            |> Creal
        | Token.CharConstant =>
            Token.toString tok
            |> Char.fromString
            |> Option.valOf
            |> Cchar
        | Token.StringConstant =>
            Cstring (Token.toString tok)
        | ( Token.Whitespace
          | Token.Comment
          | Token.MLtonReserved
          | Token.Reserved
          | Token.Identifier =>
          | Token.LongIdentifier =>
          ) => raise Fail "not a constant"

      fun tok_to_sym tok =
        case Token.getClass tok of
          Token.Identifier => Symbol.fromValue (Token.toString tok)
        | _ => raise Fail "not an identifier"

      fun long_tok_to_sym tok =
        case Token.getClass tok of
          Token.LongIdentifier =>
            Token.toString tok
            |> String.tokens (fn c => c = #".")
            |> List.map Symbol.fromValue
        | _ => raise Fail "not a long identifier"

      fun opt_to_bool opt =
        case opt of
          SOME _ => true
        | _ => false

      fun seq_map f s =
        List.map f (Seq.toList s)

      fun sseq_map f s =
        case s of
          SyntaxSeq.Empty
        | SyntaxSeq.One x => [f x]
        | SyntaxSeq.Many {elems, ...} => seq_map f elems
    in
      fun translate_exp exp =
        case exp of
          Exp.Const tok => Econst (translate_constant tok)
        | Exp.Ident { opp, id } =>
            Eident {opp = opt_to_bool opp, id = tok_to_sym id}
        | Exp.Record {elems, ...} =>
            Erecord (
              seq_map
                (fn {lab, exp, ...} =>
                  {lab = tok_to_sym lab, exp = translate_exp exp}
                )
              elems
            )
        | Exp.Select {label, ...} =>
            Eselect (tok_to_sym label)
        | Exp.Unit _ => Eunit
        | Exp.Tuple {elems, ...} =>
            Etuple (seq_map translate_exp elems)
        | Exp.List {elems, ...} =>
            Elist (seq_map translate_exp elems)
        | Exp.Sequence {elems, ...} =>
            Eseq (seq_map translate_exp elems)
        | Exp.LetInEnd {dec, exps, ...} =>
            Elet { dec = translate_dec dec, exps = seq_map translate_exp exps }
        | Exp.Parens {exp, ...} =>
            Eparen (translate_exp exp)
        | Exp.App {left, right} =>
            Eapp (translate_exp left, translate_exp right)
        | Exp.Infix {left, id, right} =>
            Einfix { left = translate_exp left
                   , id = tok_to_sym id
                   , right = translate_exp right
                   }
        | Exp.Typed {exp, ty, ...} =>
            Etyped { exp = translate_exp exp, ty = translate_ty ty }
        | Exp.Andalso {left, right, ...} =>
            Eandalso (translate_exp left, translate_exp right)
        | Exp.Orelse {left, right, ...} =>
            Eorelse (translate_exp left, translate_exp right)
        | Exp.Handle {exp, elems, ...} =>
            Ehandle { exp = translate_exp exp
                    , elems =
                        seq_map
                          (fn {pat, exp, ...} => (translate_pat pat, translate_exp exp))
                          elems
                    }
        | Exp.Raise {exp, ...} =>
            Eraise (translate_exp exp)
        | Exp.IfThenElse {exp1, exp2, exp3, ...} =>
            Eif (translate_exp exp1, translate_exp exp2, translate_exp exp3)
        | Exp.While {exp1, exp2, ...} =>
            Ewhile (translate_exp exp1, translate_exp exp2)
        | Exp.Case of {exp, elems, ...} =>
            Ecase { exp = translate_exp exp
                  , elems =
                      seq_map
                        (fn {pat, exp, ...} => (translate_pat pat, translate_exp exp))
                        elems
                  }
        | Exp.Fn of {elems, ...} =>
            Efn
              ( seq_map
                  (fn {pat, exp, ...} => (translate_pat pat, translate_exp exp))
                  elems
              )
        | Exp.MltonSpecific _ => raise Fail "not supported rn"

      and translate_ty ty =
        case ty of
          Ty.Var tok => Ttyvar (tok_to_sym tok)
        | Ty.Record {elems, ...} =>
            seq_map (fn {lab, ty, ...} => (tok_to_sym lab, translate_ty ty)) elems
           |> Trecord
        | Ty.Tuple {elems, ...} =>
            Ttuple (seq_map translate_ty elems)
        | Ty.Con {args, id} =>
            Tapp (sseq_map translate_ty args, long_tok_to_sym id)
        | Ty.Arrow {from, to, ...} =>
            Tarrow (translate_ty from, translate_ty to)
        | Ty.Parens {ty, ...} =>
            Tparens (translate_ty ty)

      and translate_pat pat =
        case pat of
          Wild _ => Pwild
        | Const tok =>
            (case
        |
    end

  end

(* Essentially a one-step zipper for an expression.
 *)
datatype flag =
    Econst of Token.t
  | Eident of {opp: Token.t option, id: MaybeLongToken.t}
  | Erecord of
      { left: Token.t
      , lab: Token.t
      , eq: Token.t
      , elems: (Token.t * Token.t * exp) list * (Token.t * Token.t * exp) list
      , delims: Token.t Seq.t
      , right: Token.t
      }
  | Eselect
  | Eunit
  | Etuple of exp list * exp list
  | Elist of exp list * exp list
  | Eseq of exp list * exp list
  | Elet
  | Eparen
  | Eapp
  | Einfix
  | Etyped
  | Eandalso
  | Eorelse
  | Ewhile
  | Ehandle
  | Eraise
  | Eif
  | Ecase
  | Efn

structure Cont = SMLofNJ.Cont

infix |>
fun x |> f = f x

datatype 'acc status =
    VAL of exp
  | STEP of 'acc * (unit -> exp status)

exception Perform of
  { context : ctx
  , exp : exp
  , cont : exp Cont.cont
  , continue : bool
  }

(* So for instance, path_part may look like
 * ANDALSO LEFT
 * old_exp would be
 * Andalso (e1, e2)
 * and exp would be
 * Const true
 *
 * meaning that we are trying to checkpoint a step on Const true, in the context
 * of it being the left-exp e1 of the expression Andalso (e1, e2)
 *)
fun checkpoint zipper exp ctx =
  let
    (* This first callcc is a `true` continue, meaning that it will simply
     * recurse stepping.
     *
     * We essentially change focus to this sub-expression, until it returns a
     * value.
     *)
    val new_exp =
      Cont.callcc
        (fn cont =>
          raise
            Perform { context = Context.add_path ctx zipper (* Add this outer expr *)
                    , exp = exp (* Add the focused expression *)
                    , cont = cont (* Add the continuation to get back here *)
                    , continue = true (* We want to continue to focus *)
                    }
        )
  in
    (* This second callcc is _just_ to trigger the outer handler, and to show
     * that we have returned to the original outer context.
     *)
    ( Cont.callcc
      (fn cont =>
        raise
          Perform { context = ctx (* Our context is the outer context *)
                  , exp = exp_from_zipper zipper new_exp
                    (* Use the zipper to compute the new total expression *)
                  , cont = cont (* Continuation to get back here *)
                  , continue = false (* We don't want to continue *)
                  }
      )
    ; new_exp
    )
  end

(* Does a left-to-right application of the function  to each element.
 *)

(* This continuation will return the currently evaluated expression to the outer
 * context.
 *)
fun step exp ctx cont =
  let
    (* Needs to be given a function which can construct the desired zipper from
     * the left and right halves of the list.
     *)
    fun step_list mk_zipper l =
      let
        fun step_list' left right =
          case right of
            [] => left
          | x::right' =>
              let
                val new_exp = checkpoint (mk_zipper (left, right')) x ctx
              in
                step_list' (new_exp::left) right'
              end
      in
        step_list' [] l
      end
  in
    case exp of
      Exp.Const _ => Cont.throw cont exp
    | Exp.Ident _ => (* special stuff here *)
    | Exp.Record {left, elems, delims, right} =>
        List.map (fn {lab, eq, exp} => exp) elems
        |> step_list (fn (left, right) =>
             let
               val len = List.length left
             in
               Seq.drop elems len
               |> Seq.tabulate (fn 0 =>
                   let
                     val {lab, eq, ...} = Seq.nth elems 0
                   in
                     {lab=lab, eq=eq, exp=
             end
        step_list
          (fn {lab, eq, exp} => exp)
          elems
        |> (fn elems => Record {left=left, elems=elems, delims=delims, right=right})
    | Exp.Select _ => NONE
    | Exp.Unit => NONE
    | Exp.Tuple {left, elems, delims, right} =>
        map_until
          step
          elems
        |> (fn elems => Tuple {left=left, elems=elems, delims=delims, right=right})
    | Exp.List {left, elems, delims, right} =>
        map_until
          step
          elems
        |> (fn elems => List {left=left, elems=elems, deims=delims, right=right})
    | Exp.Sequence {left, elems, delims, right} =>
        map_until
          step
          elems
        |> (fn elems => Sequence {left=left, elems=elems, delims=delims, right=right})
    | Exp.LetInEnd {lett, dec, inn, exps, delims, endd} =>
        map_until
          step
          elems
  end



(* fun f x =
 *    (x, fn x => x)
 *
 * f 2
 * ==> (2, fn x => x)
 *
 * two approaches:
 * can either operate directly on the austere AST
 * or can translate to a "less verbose" ast
 *
 * advantages of the former is that it's faster, and less meaningless code
 *
 * advantages of the latter is that we can have better behavior for var
 * bindings, which necessitate special things
 *)
