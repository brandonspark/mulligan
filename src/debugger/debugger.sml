
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
       VAL of SMLSyntax.exp * Context.value * Context.value Cont.t
     | EXP of SMLSyntax.exp * Context.value Cont.t
     | PROG of SMLSyntax.ast

    datatype perform =
        Step of
          { context : Context.t
          , location : Location.location list
          , focus : focus
          , stop : bool
          }
      | Break of bool * unit Cont.t

    exception Perform of perform
  end =
  struct
    open SMLSyntax
    open Context
    open Location
    open Error

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

    fun lightblue s = TerminalColors.text TerminalColors.lightblue s

    infix |>
    fun x |> f = f x

    (* A focus represents what the debugger is currently looking at.
     *
     * In the VAL and EXP cases, it also contains a first-class continuation to
     * be used to jump back into the evaluation of the program.
     *)
    datatype focus =
       VAL of SMLSyntax.exp * Context.value * Context.value Cont.t
     | EXP of SMLSyntax.exp * Context.value Cont.t
     | PROG of SMLSyntax.ast

    (* Using the idea of algebraic effects, the debugger performs certain
     * actions, which allow it to briefly dip into the interactive loop, before
     * resuming computation of the debugger again.
     *)
    datatype perform =
        Step of
          { context : Context.t
          , location : Location.location list
          , focus : focus
          , stop : bool
          }
      | Break of bool * unit Cont.t

    exception Perform of perform

    fun iter_list f z l =
      case l of
        [] => z
      | x::xs =>
          iter_list f (f (x, xs, z)) xs

    (* Check if these bindings are supposed to be breakpoints when
     * bound to.
     *)
    fun break_check_ids ctx bindings =
      let
        val break_assigns = !(Context.get_break_assigns ctx)
      in
        List.foldl
          (fn ((id, value), acc) =>
            if SymSet.member break_assigns id then
              true
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

    (* checkpoint will signal to the handler that we have focused on the
     * given expression, within the given location context.
     * We will evaluate that expression to a value, and then return it.
     *)
    fun checkpoint location exp ctx break =
      let
        (* We essentially change focus to this sub-expression, until it returns a
         * value.
         *)
        val new_value =
          Cont.callcc
            (fn cont =>
              raise
                Perform
                  ( Step { context = ctx
                         , location = location
                         , focus = EXP (exp, cont) (* Add the focused expression *)
                         , stop = break
                         }
                  )
            )
      in
        new_value
      end

    fun match_handler exn =
      case exn of
        Value.Mismatch _ =>
          raise Context.Raise (Vconstr {id = [Symbol.fromValue "Match"], arg = NONE})
      | _ => raise exn

    (* Does a left-to-right application of the function to each element.
     *
     * This continuation will throw the currently evaluated expression, as a
     * value, to the continuation.
     *)
    fun eval location exp ctx cont =
      let
        fun eval' ehole exp ctx =
          Cont.callcc (fn cont =>
            ( eval (EHOLE ehole :: location) exp ctx cont
            ; raise Fail "can't get here"
            )
          )

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
                      eval'
                        (mk_exp (List.rev left_exps, right'))
                        exp
                        ctx
                  in
                    eval_list' (new_value::left) right'
                  end
          in
            eval_list' [] l
          end

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
            ( eval' (Eseq (Ehole :: rest)) exp ctx
            ; redex location (Eseq rest) ctx cont
            )
        | Elet {dec, exps} =>
            let
              val new_ctx = eval_dec (ELET (exps) :: location) dec ctx
            in
              (* For the pretty printer's benefit, we have to be able to
               * remember that when printing out the context surrounding the
               * let-expression, we must forget all the bindings that only exist
               * within the let.
               *
               * So we put a CLOSURE into the location, so that we reset our
               * context when printing outwards.
               *)
              case exps of
                [] => raise Fail "impossible, empty exps in elet"
              | [exp] => eval (CLOSURE ctx :: location) exp new_ctx cont
              | exps => eval (CLOSURE ctx :: location) (Eseq exps) new_ctx cont
            end
        | Eparens exp => eval location exp ctx cont
        | Eapp {left, right} =>
            let
              val left =
                eval' (Eapp {left = Ehole, right = right}) left ctx
              val right =
                eval' (Eapp {left = Value.value_to_exp left, right = Ehole}) right ctx
            in
              case (left, right) of
                (constr as Vconstr {id, arg = NONE}, v) =>
                  throw (Vconstr { id = id, arg = SOME v})
              | (Vfn {matches, env, rec_env, break}, v) =>
                  ( let
                      val (bindings, new_ctx, new_exp) =
                        Value.apply_fn (matches, env, rec_env) v

                      (* Check if any of these bindings are under `break bind`
                       *)
                      val _ =
                        break_check_ids ctx bindings

                      (* Check if the function value being applied is currently
                       * broken.
                       *)
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
                    handle exn => match_handler exn
                  )
              | (Vselect sym, Vrecord fields) =>
                   List.find
                     (fn {lab, ...} => Symbol.eq (lab, sym))
                     fields
                   |> (fn NONE =>
                        eval_err ( "Selecting nonexistent field "
                                 ^ lightblue (Symbol.toValue sym)
                                 ^ " from record "
                                 ^ PrettyPrintAst.print_value ctx right)
                      | SOME ans => ans
                      )
                   |> #value
                   |> (fn value => redex_value location value ctx cont)
              | (Vbasis {function = f, ...}, v) => redex_value location (f v) ctx cont
              | _ =>
                  eval_err
                    ("Impossible app redex for " ^ PrettyPrintAst.print_value
                    ctx left ^ " and " ^ PrettyPrintAst.print_value ctx right
                    )
            end
        | Einfix {left, id, right} =>
            let
              val left =
                eval'
                  (Einfix {left = Ehole, id = id, right = right})
                  left
                  ctx
              val right =
                eval'
                  (Einfix {left = Value.value_to_exp left, id = id, right = Ehole})
                  right
                  ctx

              val value = Context.get_val ctx [id]
            in
              case value of
                (Vfn {matches, env, rec_env, break}) =>
                  ( let
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
                    handle exn => match_handler exn
                  )
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
                        | "div" ) => throw (f (Vtuple [left, right]))
                      | _ =>
                        redex_value location (f (Vtuple [left, right])) ctx cont
                      )
                  end
              | _ =>
                  eval_err
                    ( "Infix identifier bound to invalid value "
                    ^ PrettyPrintAst.print_value ctx value)
            end
        | Etyped {exp, ...} => eval location exp ctx cont
        | Eandalso {left, right} =>
            let
              val left =
                eval'
                  (Eandalso {left = Ehole, right = right})
                  left
                  ctx
              val right =
                eval'
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
                    eval_err
                      ("Andalso given invalid inputs of " ^
                      PrettyPrintAst.print_value ctx left ^ " and " ^
                      PrettyPrintAst.print_value ctx right)
              | _ =>
                eval_err
                  ("Andalso given invalid inputs of " ^
                  PrettyPrintAst.print_value ctx left ^ " and " ^
                  PrettyPrintAst.print_value ctx right)
            end
        | Eorelse {left, right} =>
            let
              val left =
                eval'
                  (Eorelse {left = Ehole, right = right})
                  left
                  ctx
              val right =
                eval'
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
                    eval_err
                      ("Orelse given invalid inputs of " ^
                      PrettyPrintAst.print_value ctx left ^ " and " ^
                      PrettyPrintAst.print_value ctx right)
              | _ =>
                eval_err
                  ("Orelse given invalid inputs of " ^
                  PrettyPrintAst.print_value ctx left ^ " and " ^
                  PrettyPrintAst.print_value ctx right)
            end
        | Ehandle {exp = exp', matches} =>
            ( throw
                (eval' (Ehandle {exp = Ehole, matches = matches}) exp' ctx)
              handle Context.Raise value =>
                ( let
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
            (* TODO: check if exn *)
            (case eval' (Eraise Ehole) exp ctx of
              (constr as Vconstr _) => (raise Context.Raise constr)
            | (constr as Vinfix _) => raise Context.Raise constr
            | value =>
                eval_err
                  ("Raise given non-constructor " ^
                  PrettyPrintAst.print_value ctx value)
            )
        | Eif {exp1, exp2, exp3} =>
            (case
              eval'
                (Eif {exp1 = Ehole, exp2 = exp2, exp3 = exp3})
                exp1
                ctx
             of
              (value as Vconstr {id = [x], arg = NONE}) =>
                if Symbol.eq (x, sym_true) then
                  redex location exp2 ctx cont
                else if Symbol.eq (x, sym_false) then
                  redex location exp3 ctx cont
                else
                  eval_err
                    ("If condition given non-boolean value " ^
                    PrettyPrintAst.print_value ctx value)
            | value =>
              eval_err
                ("If condition given non-boolean value " ^
                PrettyPrintAst.print_value ctx value)
            )
        | Ewhile {exp1, exp2} =>
            let
              (* TODO later when adding mutable state *)
              val value =
                (* This won't be right, because ctx will never change. *)
                eval' (Ewhile {exp1 = Ehole, exp2 = exp2}) exp1 ctx
            in
              ( while
                  (case value of
                    Vconstr {id = [x], arg = NONE} =>
                      if Symbol.eq (x, sym_true) then
                        true
                      else if Symbol.eq (x, sym_false) then
                        false
                      else
                        eval_err
                          ("While condition given non-boolean value " ^
                          PrettyPrintAst.print_value ctx value)
                  | _ =>
                    eval_err
                      ("While condition given non-boolean value " ^
                      PrettyPrintAst.print_value ctx value)
                  )
                do
                  redex location exp2 ctx cont
              ; throw Vunit
              )
            end
        | Ecase {exp, matches} =>
            ( let
                val value =
                  eval' (Ecase {exp = Ehole, matches = matches}) exp ctx
                val (bindings, new_ctx, new_exp) =
                  Value.match_against ctx matches value

                val _ =
                  break_check_ids ctx bindings
              in
                redex (CLOSURE ctx :: location) new_exp new_ctx cont
              end
              handle exn => match_handler exn
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

    (* This function is used on the expression resulting immediately from
     * evaluating a redex.
     * This will cause a checkpoint, which will trigger a pause.
     *)
    and redex location exp ctx cont =
      Cont.throw cont (checkpoint location exp ctx false)

    (* This function is used on the expression resulting from evaluating a redex
     * into a value.
     * We use this so that we report the value that was evaluated to, rather
     * than just immediately throwing.
     *)
    and redex_value location value ctx cont =
      let
        val (exp, new_location) = plug_hole value location
      in
        Cont.callcc
            (fn cont =>
              raise
                Perform
                  ( Step { context = ctx (* Our context is the outer context *)
                         , location = new_location
                         , focus = VAL (exp, value, cont)
                          (* Use the exp with a hole  to compute the new total expression *)
                         , stop = false
                         }
                  )
            );
        Cont.throw cont value
      end

    (* After the statics are finished, we only have a couple things we need to
     * do extra.
     * The dec_status value will signal to us what we should do.
     * In particular, we either need to:
     * 1) add the value bindings from a val declaration to the ctx
     * 2) add the value bindings from a fun declaration to the ctx
     * 3) proceed with the new context
     * 4) recurse on a sub-argument, which means providing the `eval_dec` function
     *)
    and eval_dec location dec orig_ctx =
      case Statics.synth_dec orig_ctx dec of
        Statics.DEC_VAL (new_ctx, exp_ctx, {tyvars, valbinds}) =>
          let
            val recc_flag =
              List.foldl
                (fn ({recc, ...}, acc) => recc orelse acc)
                false
                valbinds
            val ctx = orig_ctx
          in
            iter_list
              (fn ({recc, pat, exp}, rest, pairs) =>
                let
                  val break_assigns = !(Context.get_break_assigns ctx)

                  val _ =
                    List.foldl
                      (fn (id, acc) =>
                        SymSet.member break_assigns id orelse acc
                      )
                      false
                      (Binding.get_pat_ids ctx pat)
                    |> (fn b => if b then Cont.callcc (fn cont => raise Perform
                    (Break (false, cont))) else ())

                  (* We checkpoint here, and evaluate the expression in the
                   * context of all of the new bindings' types.
                   *)
                  val new_value =
                    checkpoint
                      ( DVALBINDS (recc_flag, tyvars, pat, rest) :: location)
                      exp
                      exp_ctx
                      false
                in
                  Value.match_pat exp_ctx pat new_value @ pairs
                end
                handle exn => match_handler exn
              )
              []
              valbinds
            |> (fn bindings =>
                (* If any say rec, then we must recursively make all the lambda
                 * expressions contain the others in their closure.
                 *)
                if List.foldl (fn (x, y) => x orelse y) false (List.map #recc valbinds) then
                  Context.add_rec_bindings new_ctx bindings
                else
                  Context.add_bindings new_ctx bindings
               )
          end
      | Statics.DEC_FUN (ctx, {tyvars, fvalbinds}) =>
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
              (fn fvalbind => get_fvalbinding fvalbind orig_ctx)
              fvalbinds
        in
          Context.add_rec_bindings ctx fvalbindings
        end
      | Statics.DEC_CTX ctx => ctx
      | Statics.DEC_CONT f =>
          f (location, eval_dec)

    (* INVARIANT: Should put all of the resulting bindings into the top-most scope.
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
              Statics.ascribe
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
                  Statics.ascribe
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
                |> (fn scope => Statics.ascribe scope seal)
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
              , Statics.ascribe
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
          eval_topdec [Location.PROG topdecs] topdec ctx
        )
        ctx
        ast
  end