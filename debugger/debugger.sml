
structure Debugger :
  sig
    val eval_program : SMLSyntax.ast -> Context.t -> Context.t

    val eval :
         Location.location list
      -> SMLSyntax.exp
      -> Context.t
      -> Context.value Cont.t
      -> 'a

    datatype focus =
       VAL of SMLSyntax.exp * Context.value
     | EXP of SMLSyntax.exp

    datatype perform =
        Step of
          { context : Context.t
          , location : Location.location list
          , focus : focus
          , cont : Context.value Cont.t
          , stop : bool
          }
      | Break of bool * unit Cont.t

    exception Perform of perform
  end =
  struct
    open SMLSyntax
    open Context
    open Location

    val sym_true = Symbol.fromValue "true"
    val sym_false = Symbol.fromValue "false"

    fun is_true exp =
      case exp of
        Vconstr{id = [x], arg = NONE} => Symbol.eq (x, sym_true)
      | _ => false

    fun is_false exp =
      case exp of
        Vconstr {id = [x], arg = NONE} => Symbol.eq (x, sym_false)
      | _ => false

    fun is_bool sym =
      Symbol.eq (sym, sym_false) orelse
      Symbol.eq (sym, sym_true)

    infix |>
    fun x |> f = f x

    (* We want to be able to reconstruct the location of the evaler, once we
     * focus in on a particular area.
     *
     * If we're in an expression, we may have a lot of expressions to back out
     * of.
     *
     * If we're in a declaration, we must be in a sequence or local.
     *
     * If we're in a strdec, we must be in a DMseq (in a structure).
     *
     * If we're in a topdec, we must be at the top-level program.
     *)

   datatype focus =
      VAL of SMLSyntax.exp * Context.value
    | EXP of SMLSyntax.exp

    datatype perform =
        Step of
          { context : Context.t
          , location : Location.location list
          , focus : focus
          , cont : Context.value Cont.t
          , stop : bool
          }
      | Break of bool * unit Cont.t

    exception Perform of perform

    fun iter_list f z l =
      case l of
        [] => z
      | x::xs =>
          iter_list f (f (x, xs, z)) xs

    fun break_check_ids ctx bindings =
      let
        val break_assigns = !(Context.get_break_assigns ctx)
      in
        List.foldl
          (fn ((id, value), acc) =>
            if SymSet.member break_assigns id then
              ( (*print ("Breaking at binding of value " ^
              PrettySimpleDoc.toString true (PrettyPrintAst.show_value ctx
              value) ^ " to " ^ Symbol.toValue id ^ "\n")
              ; *)true
              )
            else
              acc
          )
          false
          bindings
        |> (fn b =>
            if b then
              Cont.callcc (fn cont => raise Perform (Break (false, cont)))
            else
              ()
           )
      end

    (* Given an expression with a hole (Ehole) in it, checkpoint will signal to
     * the handler that we have focused on this expression, within that hole
     * context. We will evaluate that expression to a value, and then signal
     * that we are done.
     *)
    fun checkpoint location exp ctx break =
      let
        val _ = print
          ("checkpointing an " ^
            (PrettyPrintAst.report_doc ctx (PrettyPrintAst.show_exp ctx
            exp) 0 location) ^ "\n"
          )

        (* This first callcc is a `true` EXP, meaning that it will simply
         * recurse evaling.
         *
         * We essentially change focus to this sub-expression, until it returns a
         * value.
         *)
        val new_value =
          Cont.callcc
            (fn cont =>
              (print ("checkpoint spawned cont " ^ Int.toString
              (Cont.get_id cont) ^ "\n");
              raise
                Perform
                  ( Step { context = ctx
                         , location = location
                         , focus = EXP exp (* Add the focused expression *)
                         , cont = cont (* Add the continuation to get back here *)
                         , stop = break
                         }
                  )
              )
            )

        val _ = print "Returnign!\n"

        val _ = print
          ("we are here now with val " ^
            (PrettyPrintAst.report_doc ctx (PrettyPrintAst.show_value ctx
            new_value) 0 location) ^ "\n")

        (*
        val (exp, new_location) =
          case plug_hole new_value location of
            (exp, true) => (location, VAL (exp, new_value))
          | (exp, false) => (List.drop (location, 1), VAL (exp, new_value))
        *)
      in
        new_value
      end

    (* Does a left-to-right application of the function to each element.
     *
     * This continuation will throw the currently evaluated expression, as a
     * value, to the continuation.
     *)
    fun eval location exp ctx cont =
      let
        val _ =
          print ("  sup ya knobs, im here to evaluate " ^ PrettyPrintAst.report
          ctx exp 0 location ^ " with continuation " ^ Int.toString (Cont.get_id
          cont) ^ "\n")

        fun eval' ehole exp ctx =
          eval (ehole :: location) exp ctx cont

        fun checkpoint' ehole exp ctx =
          (* checkpoint (EHOLE ehole :: location) exp ctx false *)
          Cont.callcc (fn cont =>
            ( print (" checkpoint' spawning cont " ^ Int.toString (Cont.get_id
            cont) ^ " for exp " ^ PrettyPrintAst.report ctx exp 0 (EHOLE ehole
            :: location) ^ "\n"); eval (EHOLE ehole :: location) exp ctx cont
            ; raise Fail "can't get here"
            )
          )
          (*
            Changing to this makes it so that all expressions are evaluated at
            once, to values.
          *)
        (* Needs to be given a function which can construct the desired current
         * exp from the left and right halves of the list.
         *)
        fun eval_list mk_exp l =
          let
            fun eval_list' left right =
              case right of
                [] => List.rev left
              | exp::right' =>
                  let
                    val left_exps =
                      List.map Value.value_to_exp left
                    val new_value =
                      checkpoint'
                        ((mk_exp (List.rev left_exps, right')))
                        exp
                        ctx
                  in
                    eval_list' (new_value::left) right'
                  end
          in
            eval_list' [] l
          end

        val _ = print "about to case\n"

        fun throw value =
          if Location.is_val_dec location then
            redex_value location value ctx cont
          else
            Cont.throw cont value
      in
        case exp of
          Enumber num => throw (Vnumber num)
        | Estring s => throw (Vstring s)
        | Echar c => throw (Vchar c)
        | Eunit => throw Vunit
        | Eselect sym => throw (Vselect sym)
        | Eident {opp, id} => (* special stuff here *)
            if Context.is_con ctx id then
              throw (Vconstr {id = id, arg = NONE})
            else
              throw (Context.get_val ctx id)
        | Erecord fields =>
            let
              val labs = List.map #lab fields
              val exps = List.map #exp fields
            in
              exps
              |> eval_list (fn (left, right) =>
                   let
                     val len = List.length left
                   in
                     ( labs
                     , left @ [Ehole] @ right
                     )
                     |> ListPair.mapEq (fn (lab, exp) => {lab = lab, exp = exp})
                     |> Erecord
                   end
                )
              |> (fn values =>
                   ListPair.mapEq
                     (fn (lab, value) => {lab = lab, value = value})
                     (labs, values)
                 )
              |> Vrecord
              |> throw
            end
        | Etuple exps =>
            eval_list
              (fn (left, right) => Etuple (left @ [Ehole] @ right))
              exps
            |> Vtuple
            |> throw
        | Elist exps =>
            eval_list
              (fn (left, right) => Elist (left @ [Ehole] @ right))
              exps
            |> Vlist
            |> throw
        | Eseq [] => raise Fail "empty eseq"
        | Eseq [exp] => eval location exp ctx cont
        | Eseq (exp::rest) =>
            ( checkpoint' (Eseq (Ehole :: rest)) exp ctx
            ; redex location (Eseq rest) ctx cont
            )
        | Elet {dec, exps} =>
            let
              val new_ctx = eval_dec (ELET (exps) :: location) dec ctx
            in
              case exps of
                [] => raise Fail "impossible, empty exps in elet"
              | [exp] => eval (CLOSURE ctx :: location) exp new_ctx cont
              | exps => eval (CLOSURE ctx :: location) (Eseq exps) new_ctx cont
            end
        | Eparens exp =>
            eval
              location
              exp
              ctx
              cont
        | Eapp {left, right} =>
            let
              val _ = print "before left in eapp\n"
              val left =
                checkpoint' (Eapp {left = Ehole, right = right}) left ctx
              val _ = print "after left in eapp\n"
              val right =
                checkpoint' (Eapp {left = Value.value_to_exp left, right = Ehole}) right ctx
              val _ = print "after right in eapp\n"

              (*
              fun wrap location (exp, ctx) =
                case exp of
                  Efn (matches, _) =>
                    INR (matches, ctx, NONE, NONE)
                | _ => INL (fn () => Cont.callcc (fn cont => eval location exp ctx cont)

              fun eval_value location exp ctx =
                Cont.callcc (fn cont => eval location exp ctx cont)

              fun handle_cases location left right ctx =
                case (left, right) of
                  (constr as Vconstr {id, arg = NONE}, v) =>
                    INL (fn () => Vconstr { id = id, arg = SOME v})
                | (Vfn {matches, env, rec_env, break}, v) =>
                    let
                      val (bindings, new_ctx, new_exp) =
                        Value.apply_fn (matches, env, rec_env) v

                      val _ =
                        break_check_ids ctx bindings

                      val _ =
                        if Option.isSome (!break) then
                          Cont.callcc (fn cont =>
                            raise Perform (Break (true, cont))
                          )
                        else
                          ()
                    in
                      redex (CLOSURE ctx :: location) (new_exp, new_ctx)
                    end
                | (Vselect sym, Vrecord fields) =>
                     List.find
                       (fn {lab, ...} => Symbol.eq (lab, sym))
                       fields
                     |> (fn NONE => raise Fail "no matching field in record"
                        | SOME ans => ans
                        )
                     |> #value
                     |> (fn value => redex_value location value ctx )
                | (Vbasis {function = f, ...}, v) => (fn () => f v)
                | _ => raise Fail "impossible app redex"

              fun eval_curried location left right =
                case left of
                  Eapp {left = left', right = right'} =>
                    (case eval_curried (Eapp { left = Ehole, right = right } :: location) left' right' of
                      INR (matches, env, rec_env, break) =>
                        let
                          val right =
                            eval_value
                              (EHOLE (Eapp {left = Value.value_to_exp (Vfn { matches = matches
                                                                    , env = env
                                                                    , rec_env = rec_env
                                                                    , break = NONE
                                                                    }
                                                               )
                                    , right = right
                                    }) :: location
                              ) right ctx

                          val (bindings, new_ctx, new_exp) =
                            Value.apply_fn (matches, env, rec_env) right

                          val _ =
                            break_check_ids ctx bindings

                          val _ =
                            if Option.isSome (!break) then
                              Cont.callcc (fn cont =>
                                raise Perform (Break (true, cont))
                              )
                            else
                              ()
                        in
                          wrap location (new_exp, new_ctx)
                        end
                    | INL f =>
                        INL (fn () =>
                          let
                            val left = f ()
                            val right =
                              eval_value
                                (EHOLE (Eapp { left = Value.value_to_exp left
                                      , right = right
                                      }) :: location
                                ) right ctx
                          in
                            handle_cases location left right ctx
                          end
                        )
                | Efn matches =>
                    let
                      val left =
                        eval_value (EHOLE (Eapp {left = Ehole, right = right})
                        :: location) left ctx
                      val right =
                        eval_value (EHOLE (Eapp {left = Value.value_to_exp left,
                        right = Ehole}) :: location) right ctx
                    in
                      handle_cases location left right ctx
                    end
                | _ =>
                    let
                *)
            in
              case (left, right) of
                (constr as Vconstr {id, arg = NONE}, v) =>
                  throw
                    ( Vconstr
                        { id = id
                        , arg = SOME v
                        }
                    )
              | (Vfn {matches, env, rec_env, break}, v) =>
                  let
                    val (bindings, new_ctx, new_exp) =
                      Value.apply_fn (matches, env, rec_env) v

                    val _ =
                      break_check_ids ctx bindings

                    val _ =
                      if Option.isSome (!break) then
                        Cont.callcc (fn cont =>
                          raise Perform (Break (true, cont))
                        )
                      else
                        ()

                  in
                    if !(#step_app (Context.get_settings ctx)) then
                      redex (CLOSURE ctx :: location) new_exp new_ctx cont
                    else
                      eval (CLOSURE ctx :: location) new_exp new_ctx cont
                  end
              | (Vselect sym, Vrecord fields) =>
                   List.find
                     (fn {lab, ...} => Symbol.eq (lab, sym))
                     fields
                   |> (fn NONE => raise Fail "no matching field in record"
                      | SOME ans => ans
                      )
                   |> #value
                   |> (fn value => redex_value location value ctx cont)
              | (Vbasis {function = f, ...}, v) => redex_value location (f v) ctx cont
              | _ => raise Fail "impossible app redex"
            end
        | Einfix {left, id, right} =>
            let
              val _ = print "before left in einfix\n"
              val left =
                checkpoint'
                  (Einfix {left = Ehole, id = id, right = right})
                  left
                  ctx
              val _ = print "after left in einfix\n"
              val right =
                checkpoint'
                  (Einfix {left = Value.value_to_exp left, id = id, right = Ehole})
                  right
                  ctx

              val value = Context.get_val ctx [id]
            in
              case value of
                (Vfn {matches, env, rec_env, break}) =>
                  let
                    val (bindings, new_ctx, new_exp) =
                      Value.apply_fn (matches, env, rec_env) (Vtuple [left, right])

                    val _ =
                      break_check_ids ctx bindings

                    val _ =
                      if Option.isSome (!break) then
                        Cont.callcc (fn cont =>
                          raise Perform (Break (true, cont))
                        )
                      else
                        ()
                  in
                    redex (CLOSURE ctx :: location) new_exp new_ctx cont
                  end
              | Vconstr {id = [sym], arg = NONE} =>
                  redex_value
                    location
                    (Vinfix {left = left, id = sym, right = right})
                    ctx
                    cont
              | Vbasis {function = f, name} =>
                  let
                    val result = f (Vtuple [left, right])
                  in
                    if !(#step_arithmetic (Context.get_settings ctx)) then
                      redex_value location (f (Vtuple [left, right])) ctx cont
                    else
                      (case Symbol.toValue name of
                        ( "+"
                        | "-"
                        | "*"
                        | "div" ) =>
                          throw (f (Vtuple [left, right]))
                      | _ =>
                        redex_value location (f (Vtuple [left, right])) ctx cont
                      )
                  end
              | _ => raise Fail "applied value is not a function or constr"
            end
        | Etyped {exp, ...} => eval location exp ctx cont
        | Eandalso {left, right} =>
            let
              val left =
                checkpoint'
                  (Eandalso {left = Ehole, right = right})
                  left
                  ctx
              val right =
                checkpoint'
                  (Eandalso {left = Value.value_to_exp left, right = Ehole})
                  right
                  ctx
            in
              case (left, right) of
                (Vconstr {id = [x], arg = NONE}, Vconstr {id = [y], arg = NONE}) =>
                  if is_bool x andalso is_bool y then
                    if Symbol.eq (x, sym_true) andalso Symbol.eq (y, sym_true) then
                      redex_value
                        location
                        (Vconstr { id = [sym_true], arg = NONE })
                        ctx
                        cont
                    else
                      redex_value
                        location
                        (Vconstr { id = [sym_false], arg = NONE })
                        ctx
                        cont
                  else
                    raise Fail "invalid inputs to andalso"
              | _ => raise Fail "invalid inputs to andalso"
            end
        | Eorelse {left, right} =>
            let
              val left =
                checkpoint'
                  (Eorelse {left = Ehole, right = right})
                  left
                  ctx
              val right =
                checkpoint'
                  (Eorelse {left = Value.value_to_exp left, right = Ehole})
                  right
                  ctx
            in
              case (left, right) of
                (Vconstr {id = [x], arg = NONE}, Vconstr {id = [y], arg = NONE}) =>
                  if is_bool x andalso is_bool y then
                    if Symbol.eq (x, sym_false) andalso Symbol.eq (y, sym_false) then
                      redex_value
                        location
                        (Vconstr { id = [sym_false], arg = NONE })
                        ctx
                        cont
                    else
                      redex_value
                        location
                        (Vconstr { id = [sym_true], arg = NONE })
                        ctx
                        cont
                  else
                    raise Fail "invalid inputs to orelse"
              | _ => raise Fail "invalid inputs to orelse"
            end
        | Ehandle {exp = exp', matches} =>
            ( (*print "evalling handle\n"
            ; *)throw
              (checkpoint' (Ehandle {exp = Ehole, matches = matches}) exp' ctx)
              handle Context.Raise value =>
                ( let
                    (*val _ = print "have now handled\n"
                     * *)
                    val (bindings, new_ctx, exp) = Value.match_against ctx matches value
                    val _ =
                      break_check_ids ctx bindings
                  in
                    redex (CLOSURE ctx :: location) exp new_ctx cont
                  end
                  handle Value.Mismatch _ => raise Context.Raise value
                )

            )
        | Eraise exp =>
            (case checkpoint' (Eraise Ehole) exp ctx of
              (constr as Vconstr _) => (raise Context.Raise constr)
            | (constr as Vinfix _) => raise Context.Raise constr
            | _ => raise Fail "raise on non-constr"
            )
        | Eif {exp1, exp2, exp3} =>
            (case
              checkpoint'
                (Eif {exp1 = Ehole, exp2 = exp2, exp3 = exp3})
                exp1
                ctx
             of
              Vconstr {id = [x], arg = NONE} =>
                if Symbol.eq (x, sym_true) then
                  redex location exp2 ctx cont
                else if Symbol.eq (x, sym_false) then
                  redex location exp3 ctx cont
                else
                  raise Fail "if given non-bools"
            | _ => raise Fail "if given non-bools"
            )
        | Ewhile {exp1, exp2} =>
            let
              (* TODO later when adding mutable state *)
              val value =
                (* This won't be right, because ctx will never change. *)
                checkpoint' (Ewhile {exp1 = Ehole, exp2 = exp2}) exp1 ctx
            in
              ( while
                  (case value of
                    Vconstr {id = [x], arg = NONE} =>
                      if Symbol.eq (x, sym_true) then
                        true
                      else if Symbol.eq (x, sym_false) then
                        false
                      else
                        raise Fail "non-bool given to while"
                  | _ => raise Fail "nonb-ool given to while"
                  )
                do
                  redex location exp2 ctx cont
              ; throw Vunit
              )
            end
        | Ecase {exp, matches} =>
            ( let
                val value =
                  checkpoint' (Ecase {exp = Ehole, matches = matches}) exp ctx
                val (bindings, new_ctx, new_exp) =
                  Value.match_against ctx matches value

                val _ =
                  break_check_ids ctx bindings
              in
                redex (CLOSURE ctx :: location) new_exp new_ctx cont
              end
              handle Value.Mismatch _ =>
                raise Context.Raise (Vconstr {id = [Symbol.fromValue "Match"], arg = NONE})
            )
          (* If we are evaluating an `Efn`, it must have already existed.
           * As in, it was an anonymous function value, and its closure is just
           * the current closure.
           *
           * This breaks down if we ever call eval twice.
           *)
        | Efn (matches, NONE) =>
            throw (Vfn {matches = matches, env = ctx, rec_env = NONE, break = ref NONE})
        | Efn (matches, SOME _) =>
            raise Fail "shouldn't eval an Efn with a closure"
        | Ehole => raise Fail "shouldn't happen, evaling Ehole"
      end

    and redex_break location exp ctx cont =
      let
        val new_value = checkpoint location exp ctx true
      in
        Cont.throw cont new_value
      end

    (* This function is used on the expression resulting immediately from
     * evaluating a redex.
     * This will cause a checkpoint, which will trigger a pause.
     *)
    and redex location exp ctx cont =
      let
        val _ = print ("entered redex with cont " ^ Int.toString (Cont.get_id
        cont) ^ "\n")
        val _ = print ("redexing normal!! on " ^ (PrettySimpleDoc.toString true
        (PrettyPrintAst.show_exp ctx exp) ^ " with cont " ^ Int.toString
        (Cont.get_id cont) ^ "\n"))
        val new_value = checkpoint location exp ctx false
      in
        Cont.throw cont new_value
      end

    (* This function is used on the expression resulting from evaluating a redex
     * into a value.
     * We use this so that we report the value that was evaluated to, rather
     * than just immediately throwing.
     *)
    and redex_value location value ctx cont =
      let
        val _ = print ("redexing value!! on " ^ (PrettySimpleDoc.toString true
        (PrettyPrintAst.show_value ctx value) ^ " with cont " ^ Int.toString
        (Cont.get_id cont) ^ "\n"))
        val (exp, new_location) = plug_hole value location
      in
        ( Cont.callcc
            (fn cont =>
              (print ("redex value spawned cont " ^ Int.toString (Cont.get_id
              cont) ^ "\n");
              raise
                Perform
                  ( Step { context = ctx (* Our context is the outer context *)
                         , location = new_location
                         , focus = VAL (exp, value)
                          (* Use the exp wiht a hole  to compute the new total expression *)
                         , cont = cont (* Continuation to get back here *)
                         , stop = false
                         }
                  )
              )
            )
        ; print ("redex value throwing to cont " ^ Int.toString (Cont.get_id cont) ^
        "\n")
        ; Cont.throw cont value
        )
      end

    and eval_dec location dec ctx =
      case dec of
        Dseq decs =>
          iter_list
            (fn (dec, decs, ctx) =>
              eval_dec (DSEQ decs :: location) dec ctx
            )
            ctx
            decs
      | Dval {valbinds, tyvars} =>
          (* Iterate on them and evaluate each valbinding
           * sequentially, in the original context.
           *)
          let
            val recc_flag =
              List.foldl
                (fn ({recc, ...}, acc) => recc orelse acc)
                false
                valbinds
          in
            iter_list
              (fn ({recc, pat, exp}, rest, pairs) =>
                let
                  val ids = Binding.get_pat_ids ctx pat

                  val break_assigns = !(Context.get_break_assigns ctx)

                  val _ =
                    List.foldl
                      (fn (id, acc) =>
                        SymSet.member break_assigns id orelse acc
                      )
                      false
                      ids
                    |> (fn b => if b then Cont.callcc (fn cont => raise Perform
                    (Break (false, cont))) else ())

                  (* val _ = print ("iter for valdecs on exp " ^
                (PrettyPrintAst.report_doc ctx (PrettyPrintAst.show_exp ctx
                exp) 0 location) ^ "\n")
                  *)

                  val new_value =
                    checkpoint
                      ( DVALBINDS (recc_flag, tyvars, pat, rest) :: location)
                      exp
                      ctx
                      false
                  (* This flag needs to be true, or else val bindings of values
                   * won't trigger a step at all.
                   * For instance, `val _ = fn x => x`
                   *)
                in
                  Value.match_pat ctx pat new_value @ pairs
                end
              )
              []
              valbinds
            |> (fn bindings =>
                (* If any say rec, then we must recursively make all the lambda
                 * expressions contain the others in their closure.
                 *)
                if List.foldl (fn (x, y) => x orelse y) false (List.map #recc valbinds) then
                  Context.add_rec_bindings ctx bindings
                else
                  Context.add_bindings ctx bindings
               )
          end
      | Dfun {fvalbinds, ...} =>
          let
            fun get_id_pats fname_args =
              case fname_args of
                Fprefix {opp, id, args} => (id, args)
              | Finfix {left, id, right} => (id, [Ptuple [left, right]])
              | Fcurried_infix {left, id, right, args} =>
                  (id, Ptuple [left, right] :: args)

            fun process_fvalbind fvalbind =
              List.foldr
                (fn ({fname_args, ty, exp}, acc) =>
                  let
                    val (id, pats) = get_id_pats fname_args
                    val num_args = List.length pats
                  in
                    case acc of
                      NONE => SOME (id, num_args, [(pats, exp)])
                    | SOME (id, num_args, acc) => SOME (id, num_args, (pats, exp) :: acc)
                  end
                )
                NONE
                fvalbind

            fun get_fvalbinding fvalbind ctx =
              case process_fvalbind fvalbind of
                NONE => raise Fail "empty fvalbind"
              | SOME (id, num_args, matches) =>
                ( if SymSet.member (!(Context.get_break_assigns ctx)) id then
                    Cont.callcc (fn cont => raise Perform (Break (false, cont)))
                  else ()
                ; case matches of
                   [(pats, exp)] =>
                     ( id
                     , List.foldr
                         (fn (pat, exp) => Efn ([{pat = pat, exp = exp}], NONE))
                         exp
                         pats
                       |> (fn Efn ([{pat, exp}], _) =>
                            Vfn { matches = [{pat = pat, exp = exp}]
                                , env = ctx
                                , rec_env = NONE
                                , break = ref NONE
                                }
                          | _ => raise Fail "empty args for fvalbind, shouldn't happen"
                          )
                     )
                 | _ =>
                   let
                     val new_vars = List.tabulate (num_args, fn _ => TempId.new ())

                   in
                     ( id
                     , List.foldr
                         (fn (id, exp) =>
                           Efn ( [ { pat = Pident {opp = false, id = [id]}
                                   , exp = exp
                                   }
                                 ]
                               , NONE
                               )
                         )
                         ( Ecase
                             { exp =
                                case new_vars of
                                  [var] => Eident {opp = false, id = [var]}
                                | _ =>
                                    Etuple
                                      ( List.map
                                          (fn id => Eident {opp = false, id = [id]})
                                          new_vars
                                      )
                             , matches =
                                List.map
                                  (fn (pats, exp) => { pat =
                                                       case pats of
                                                         [pat] => pat
                                                       | _ => Ptuple pats
                                                   , exp = exp
                                                   }
                                  )
                                  matches
                             }
                         )
                         new_vars
                        |> (fn exp =>
                             case exp of
                               Efn ([{pat, exp}], _) => Vfn { matches = [{pat = pat, exp = exp}]
                                                       , env = ctx
                                                       , rec_env = NONE
                                                       , break = ref NONE
                                                       }
                            | _ => raise Fail "empty args for fvalbind, shouldn't happen"
                          )
                     )
                   end
                )

            val fvalbindings =
              List.map
                (fn fvalbind => get_fvalbinding fvalbind ctx)
                fvalbinds

          in
            Context.add_rec_bindings ctx fvalbindings
          end
      | Dtype typbinds =>
          Context.add_typbinds ctx typbinds
      | Ddatdec {datbinds, withtypee} =>
          (* TODO: recursive datatypes type wise *)
          List.foldl
            (fn (datbind, ctx) =>
              Context.add_datatype ctx datbind
            )
            ctx
            datbinds
      | Ddatrepl {left_tycon, right_tycon} =>
          Context.replicate_datatype ctx (left_tycon, right_tycon)
      | Dabstype _ => raise Fail "no support for abstype right now"
      | Dexception exbinds =>
          (* TODO: type stuff *)
          List.foldl
            (fn (Xnew {opp, id, ...}, ctx) =>
                Context.add_exn ctx id
            | (exbind as Xrepl {left_id, right_id, ...}, ctx) =>
                Context.replicate_exception ctx (left_id, right_id)
            )
            ctx
            exbinds
      | Dlocal {left_dec, right_dec} =>
          (* Make a temporary scope to evaluate all the bindings in the `local`.
           * Then, make a scope for all of the actual `in` decs.
           * Then pop the penultimate scope to get rid of the temporary
           * bindings.
           *)
          ( case left_dec of
              Dseq decs =>
                iter_list
                  (fn (dec, decs, ctx) =>
                    eval_dec (DLOCAL (decs, right_dec) :: CLOSURE ctx :: location) dec ctx
                  )
                  (Context.enter_scope ctx)
                  decs
                |> Context.enter_scope
                |> (fn ctx' => eval_dec (CLOSURE ctx :: location) right_dec ctx')
                |> Context.exit_local
            | _ =>
                eval_dec
                  (DLOCAL ([], right_dec) :: CLOSURE ctx :: location)
                  left_dec
                  (Context.enter_scope ctx)
                |> Context.enter_scope
                |> (fn ctx' => eval_dec (CLOSURE ctx :: location) right_dec ctx')
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
      | Dhole => raise Fail "shouldn't eval dhole"

    (* Should put all of the resulting bindings into the top-most scope.
     *)
    and eval_module location module ctx =
      case module of
        (* For a module identifier, open all the module's contents into the
         * current scope.
         *)
        Mident id => Context.open_path ctx id

        (* For a module structure, evaluate the inner declarations and put their
         * changes into the current scope.
         *)
      | Mstruct strdec =>
          eval_strdec (MSTRUCT :: location) strdec ctx

        (* For a sealed module, evaluate the inner module, reify it as a scope,
         * then ascribe the scope to the seal
         *)
      | Mseal {module, opacity, signat} =>
          eval_module (MSEAL {opacity = opacity, signat = signat} :: location) module ctx
          |> Context.pop_scope
          |> (fn scope =>
              Context.ascribe
                scope
                (SOME { opacity = opacity
                      , sigval = Value.evaluate_signat ctx signat
                      }
                )
             )
          |> Context.open_scope ctx

        (* For a functor application, evaluate the argument to a reified scope,
         * and then apply the functor to it to get a resulting scope. Then, open
         * that in the current context.
         *)
      | Mapp {functorr, arg} =>
        let
          fun apply_functor
            eval_module
            (ctx as {functordict, ...} : t)
            functor_id
            scope =
              let
                val Functorval
                      { arg_seal = {id, sigval = arg_sigval}
                      , seal
                      , body
                      } = Context.get_functor ctx functor_id

                val ascribed_arg =
                  ascribe
                    scope
                    ( SOME
                        { opacity = Transparent
                        , sigval = arg_sigval
                        }
                    )
              in
                ( case id of
                    (* Sugar application. This means the module is not given a name, and
                     * instead opened in the context.
                     *)
                    NONE =>
                      eval_module
                        (FBODY (functor_id) :: location)
                        body
                        (open_scope ctx ascribed_arg)
                  | SOME id =>
                      add_module ctx id ascribed_arg
                )
                |> pop_scope
                |> (fn scope => ascribe scope seal)
              end
        in
          (case arg of
            Normal_app module =>
              eval_module (MAPP functorr :: location) module ctx
              |> Context.pop_scope
              |> apply_functor eval_module ctx functorr
              |> Context.open_scope ctx

          | Sugar_app strdec =>
              eval_strdec (MAPP functorr :: location) strdec ctx
              |> Context.pop_scope
              |> apply_functor eval_module ctx functorr
              |> Context.open_scope ctx
          )
        end

          (* For a module let, evaluate the declaration in a new scope, and then
           * enter a new scope for the actual module itself. Get rid of the
           * local scope.
           *)
      | Mlet {dec, module} =>
          eval_strdec (MLET module :: location) dec (Context.enter_scope ctx)
          |> Context.enter_scope
          |> eval_module (CLOSURE ctx :: location) module
          |> Context.exit_local
      | Mhole => raise Fail "shouldn't eval mhole"


    and eval_strdec location strdec ctx =
      case strdec of
        DMseq strdecs =>
          iter_list
            (fn (strdec, strdecs, ctx) =>
              eval_strdec (DMSEQ strdecs :: location) strdec ctx
            )
            ctx
            strdecs
      | DMdec dec =>
          eval_dec location dec ctx
      | DMstruct modules =>
          iter_list
            (fn ({id, seal, module}, rest, acc) =>
              ( id
              , seal
              , eval_module
                (STRUCTS (id, seal, rest) :: CLOSURE ctx :: location)
                module
                (Context.enter_scope ctx)
              )
              :: acc
            )
            []
            modules
          |> List.map (fn (id, seal, ctx) => (id, seal, Context.pop_scope ctx))
          |> List.map (fn (id, seal, scope) =>
              ( id
              , Context.ascribe
                  scope
                  ( Option.map
                      (fn {opacity, signat} => { opacity = opacity
                                             , sigval =
                                                 Value.evaluate_signat
                                                   ctx
                                                   signat
                                             }
                      )
                      seal
                  )
              )
             )
          |> List.foldl (fn ((id, scope), ctx) =>
               Context.add_module ctx id scope
             ) ctx
      | DMlocal {left_dec, right_dec} =>
          ( case left_dec of
              DMseq strdecs =>
                iter_list
                  (fn (strdec, strdecs, ctx) =>
                    eval_strdec (DMLOCAL (strdecs, right_dec) :: CLOSURE ctx :: location) strdec ctx
                  )
                  (Context.enter_scope ctx)
                  strdecs
                |> Context.enter_scope
                |> (fn ctx' => eval_strdec (CLOSURE ctx :: location) right_dec ctx')
                |> Context.exit_local
            | _ =>
                eval_strdec
                  (DMLOCAL ([], right_dec) :: CLOSURE ctx :: location)
                  left_dec
                  (Context.enter_scope ctx)
                |> Context.enter_scope
                |> (fn ctx' => eval_strdec (CLOSURE ctx :: location) right_dec ctx')
                |> Context.exit_local
          )
      | DMhole => raise Fail "shouldn't eval dmhole"

    fun eval_topdec location topdec ctx =
      case topdec of
        Strdec strdec =>
          eval_strdec location strdec ctx

      | Sigdec sigdec =>
          (* All these sigbindings are done "simultaneously", as in, within
           * the same context.
           *)
          List.foldr
            (fn (sigbind, acc) =>
              Binding.generate_sigbinding ctx sigbind :: acc
            )
            []
            sigdec
          |> Binding.add_sigbindings ctx

      | Fundec fundec =>
          List.foldl
            (fn (funbind, ctx) => Binding.add_funbind ctx funbind)
            ctx
            fundec
      | Thole => raise Fail "shouldn't eval thole"

    fun eval_program ast ctx =
      iter_list
        (fn (topdec, topdecs, ctx) =>
          eval_topdec [PROG topdecs] topdec ctx
        )
        ctx
        ast
  end
