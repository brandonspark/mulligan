
structure Debugger =
  struct
    open SMLSyntax

    val sym_true = Symbol.fromValue "true"
    val sym_false = Symbol.fromValue "false"

    fun is_true exp =
      case exp of
        Eident {opp, id = [x]} => Symbol.eq (x, sym_true)
      | _ => false

    fun is_false exp =
      case exp of
        Eident {opp, id = [x]} => Symbol.eq (x, sym_false)
      | _ => false

    structure Cont = MLton.Cont

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
    datatype location =
        ELET of exp list
      | EHOLE of exp
      | DLOCAL of dec list * dec
      | DSEQ of dec list
      | DVALBINDS of symbol list * pat * { recc : bool, pat : pat, exp : exp } list
      | DMLOCAL of strdec list * strdec
      | DMSEQ of strdec list
      | MLET of module
      | MSTRUCT
      | MSEAL of { opacity : opacity, signat : signat }
      | MAPP of symbol
      | STRUCTS of
           symbol
         * { opacity : opacity, signat : signat } option
         * { id : symbol
           , seal : {opacity : opacity, signat : signat } option
           , module : module
           } list
      | PROG of topdec list

   datatype 'acc status =
        VAL of exp
      | STEP of 'acc * (unit -> exp status)

    exception Perform of
      { context : Context.t
      , location : location list
      , exp : exp
      , cont : exp Cont.t
      , continue : bool
      }

    fun iter_list f z l =
      case l of
        [] => z
      | x::xs =>
          iter_list f (f (x, xs, z)) xs

    (* TODO: plug_hole needs to look at the most recent thing in the location,
     * and then plug that.
    fun plug_hole_dec e dec_hole =
      case dec_hole of
        Dval {tyvars, valbinds} =>
          Dval { tyvars = tyvars
               , valbinds =
                  List.map (fn {recc, pat, exp} => { recc = recc
                                                 , pat = pat
                                                 , exp = plug_hole e exp
                                                 }
                           ) valbinds
               }
      | Dlocal {left_dec, right_dec} =>
           Dlocal { left_dec = plug_hole_dec e left_dec
                  , right_dec = plug_hole_dec e right_dec
                  }
      | Dseq decs =>
          Dseq (List.map (plug_hole_dec e) decs)
      | ( Dfun _
        | Dtype _
        | Ddatdec _
        | Ddatrepl _
        | Dabstype _
        | Dexception _
        | Dopen _
        | Dinfix _
        | Dinfixr _
        | Dnonfix _ ) => dec_hole


    and plug_hole e exp_hole =
      case exp_hole of
        ( Enumber _
        | Estring _
        | Echar _
        | Eselect _
        | Eunit
        | Eident _
        ) => exp_hole
      | Erecord fields =>
          Erecord
            (List.map (fn {lab, exp} => {lab = lab, exp = plug_hole e exp}) fields)
      | Etuple exps =>
          Etuple (List.map (plug_hole e) exps)
      | Elist exps =>
          Elist (List.map (plug_hole e) exps)
      | Eseq exps =>
          Eseq (List.map (plug_hole e) exps)
      | Elet {dec, exps} =>
          (* Shouldn't be any holes in exps. *)
          Elet { dec = plug_hole_dec e dec
               , exps = exps
               }
      | Eparens exp => Eparens (plug_hole e exp)
      | Eapp {left, right} =>
          Eapp { left = plug_hole e left
               , right = plug_hole e right
               }
      | Einfix {left, id, right} =>
          Einfix { left = plug_hole e left
                 , id = id
                 , right = plug_hole e right
                 }
      | Etyped {exp, ty} =>
          Etyped { exp = plug_hole e exp, ty = ty }
      | Eandalso {left, right} =>
          Eandalso { left = plug_hole e left
                   , right = plug_hole e right
                   }
      | Eorelse {left, right} =>
          Eorelse { left = plug_hole e left
                   , right = plug_hole e right
                   }
      | Ehandle {exp, matches} =>
          (* Shouldn't be any holes in matches. *)
          Ehandle { exp = plug_hole e exp
                  , matches = matches
                  }
      | Eraise exp => Eraise (plug_hole e exp)
      | Eif {exp1, exp2, exp3} =>
          (* Shouldn't be any holes in exp2 or exp3. *)
          Eif { exp1 = plug_hole e exp1, exp2 = exp2, exp3 = exp3 }
      | Ewhile {exp1, exp2} =>
          (* Shouldn't be any holes in exp2. *)
          Ewhile { exp1 = plug_hole e exp1, exp2 = exp2}
      | Ecase {exp, matches} =>
          (* Shouldn't be any holes in matches. *)
          Ecase { exp = plug_hole e exp, matches = matches }
      | Efn _ => (* Shouldn't be any in a function. *) exp_hole
      | Ehole => e
    *)

    (* Given an expression with a hole (Ehole) in it, checkpoint will signal to
     * the handler that we have focused on this expression, within that hole
     * context. We will evaluate that expression to a value, and then signal
     * that we are done.
     *)
    fun checkpoint location exp ctx =
      let
        (* This first callcc is a `true` continue, meaning that it will simply
         * recurse evalping.
         *
         * We essentially change focus to this sub-expression, until it returns a
         * value.
         *)
        val new_exp =
          Cont.callcc
            (fn cont =>
              raise
                Perform { context = ctx
                        , location = location
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
                      , location = zoom_out location new_exp
                      , exp = plug_hole new_exp location
                        (* Use the exp wiht a hole  to compute the new total expression *)
                      , cont = cont (* Continuation to get back here *)
                      , continue = false (* We don't want to continue *)
                      }
          )
        ; new_exp
        )
      end

    exception Raise of exp

    (* Does a left-to-right application of the function to each element.
     *
     * This continuation will throw the currently evaluated expression, as a
     * value, to the continuation.
     *)
    fun eval location exp ctx cont =
      let
        fun eval' ehole exp ctx =
          eval (ehole :: location) exp ctx
        fun checkpoint' ehole exp ctx =
          checkpoint (EHOLE ehole :: location) exp ctx
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
                    val new_exp =
                      checkpoint
                        (EHOLE (mk_exp (List.rev left, right')) :: location)
                        exp
                        ctx
                  in
                    eval_list' (new_exp::left) right'
                  end
          in
            eval_list' [] l
          end
      in
        case exp of
          ( Enumber _
          | Estring _
          | Echar _
          | Eunit
          | Eselect _
          ) => Cont.throw (cont, exp)
        | Eident {opp, id} => (* special stuff here *)
            Cont.throw (cont, Context.get_val ctx id)
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
            end
        | Etuple exps =>
            eval_list
              (fn (left, right) => Etuple (left @ [Ehole] @ right))
              exps
        | Elist exps =>
            eval_list
              (fn (left, right) => Elist (left @ [Ehole] @ right))
              exps
        | Eseq exps =>
            eval_list
              (fn (left, right) =>
                case right of
                  [] => Ehole
                | _ => Eseq (Ehole ::right)
              )
              exps
        | Elet {dec, exps} =>
            let
              val new_ctx = eval_dec (ELET (exps) :: location) dec ctx
            in
              case exps of
                [] => raise Fail "impossible, empty exps in elet"
              | [exp] => eval location exp new_ctx cont
              | exps => eval location (Eseq exps) new_ctx cont
            end
        | Eparens exp =>
            eval
              location
              exp
              ctx
              cont
        | Eapp {left, right} =>
            let
              val left =
                checkpoint' (Eapp {left = Ehole, right = right}) left ctx
              val right =
                checkpoint' (Eapp {left = left, right = Ehole}) right ctx
            in
              case (left, right) of
                (constr as Eident {id, ...}, v) =>
                  if Context.is_constr ctx id then
                    Cont.throw
                      ( cont
                      , Eapp
                          { left = constr
                          , right = v
                          }
                      )
                  else
                    (case Context.get_val ctx id of
                      NONE => raise Fail "ident out of scope"
                    | SOME (Efn (matches, E, VE), v) =>
                        let
                          val (new_exp, new_ctx) =
                            Context.match_against
                              ctx
                              matches
                              (SOME (E, VE))
                              v
                        in
                          eval location new_exp new_ctx cont
                        end
                    | _ => raise Fail "app of non-fn"
                    )

              | (Eselect sym, Erecord fields) =>
                   List.find
                     (fn {lab, ...} => Symbol.eq (lab, sym))
                     fields
                   |> (fn NONE => raise Fail "no matching field in record"
                      | SOME ans => ans
                      )
                   |> #exp
                   |> (fn exp => eval location exp ctx cont)
              | (Efn (matches, E, VE), v) =>
                   (* TODO *)
                   let
                     val (new_ctx, new_exp) = Context.apply_fn (matches, E, VE)
                   in
                     (* CHECK: This should not induce capture, because we're using
                      * the function's own closure.
                      *)
                     eval location new_exp new_ctx cont
                   end
              | _ => raise Fail "impossible app redex"
            end
        | Einfix {left, id, right} =>
            let
              val left =
                checkpoint'
                  (Einfix {left = Ehole, id = id, right = right})
                  left
                  ctx
              val right =
                checkpoint'
                  (Einfix {left = left, id = id, right = Ehole})
                  right
                  ctx
              val valopt = Context.get_val ctx id
            in
              if Context.is_con ctx id then
                Cont.throw (cont, Einfix {left = left, id = id, right = right})
              else
                (case valopt of
                  NONE => raise Fail "infix is neither function nor constr"
                | SOME exp =>
                    eval
                      location
                      (Eapp { left = exp
                            , right = Etuple [left, right]
                            }
                      )
                      ctx
                      cont
                )
            end
        | Etyped {exp, ...} => eval location exp ctx cont
        | Eandalso {left, right} =>
            let
              val left =
                checkpoint
                  (Eandalso {left = Ehole, id = id, right = right})
                  left
                  ctx
              val right =
                checkpoint
                  (Eandalso {left = left, id = id, right = Ehole})
                  right
                  ctx
            in
              case (left, right) of
                (Eident {opp, id = [x]}, Eident {opp, id = [y]}) =>
                  if is_bool x andalso is_bool y then
                    if is_true left andalso is_true right then
                      Eident {opp = false, id = sym_true}
                    else
                      Eident {opp = false, id = sym_false}
                  else
                    raise Fail "invalid inputs to andalso"
              | _ => raise Fail "invalid inputs to andalso"
            end
        | Eorelse {left, right} =>
            let
              val left =
                checkpoint
                  (Eorelse {left = Ehole, id = id, right = right})
                  left
                  ctx
              val right =
                checkpoint
                  (Eorelse {left = left, id = id, right = Ehole})
                  right
                  ctx
            in
              case (left, right) of
                (Eident {opp, id = [x]}, Eident {opp, id = [y]}) =>
                  if is_bool x andalso is_bool y then
                    if is_false x andalso is_false right then
                      Eident {opp = false, id = sym_false}
                    else
                      Eident {opp = false, id = sym_true}
                  else
                    raise Fail "invalid inputs to orelse"
              | _ => raise Fail "invalid inputs to orelse"
            end
        | Ehandle {exp, matches} =>
            Cont.throw
              ( cont
              , checkpoint (Ehandle {exp = Ehole, matches = matches}) exp ctx
              )
            handle Raise exp =>
              let
                val (new_ctx, exp) = Context.match_against ctx matches NONE exp
              in
                eval exp new_ctx cont
              end
        | Eraise exp =>
            (case checkpoint (Eraise Ehole) exp ctx of
              (constr as
                Eapp { left = Eident {opp, id}
                     , right = v
                     }
              ) => raise Raise constr
            | (constr as
                Einfix {left, id, right}
              ) => raise Raise constr
            | _ => raise Fail "raise on non-constr"
            )
        | Eif {exp1, exp2, exp3} =>
            (case
              checkpoint
                (Eif {exp1 = Ehole, exp2 = exp2, exp3 = exp3})
                exp1
                ctx
             of
              Eident {opp, id = [x]} =>
                if Symbol.eq (x, sym_true) then
                  eval location exp2 ctx cont
                else if Symbol.eq (x, sym_false) then
                  eval location exp3 ctx cont
                else
                  raise Fail "if given non-bools"
            | _ => raise Fail "if given non-bools"
            )
        | Ewhile {exp1, exp2} =>
            let
              (* TODO later when adding mutable state *)
              val new_exp =
                (* This won't be right, because ctx will never change. *)
                checkpoint' (Ewhile {exp1 = Ehole, exp2 = exp2}) exp1 ctx
            in
              ( while
                  (case new_exp of
                    Eident {opp, id = [x]} =>
                      if Symbol.eq (x, sym_true) then
                        true
                      else if Symbol.eq (x, sym_false) then
                        false
                      else
                        raise Fail "non-bool given to while"
                  | _ => raise Fail "nonb-ool given to while"
                  )
                do
                  eval exp2 ctx cont
              ; Cont.throw (cont, Eunit)
              )
            end
        | Ecase {exp, matches} =>
            let
              val new_exp =
                checkpoint' (Ecase {exp = Ehole, matches = matches}) exp ctx
              val (new_ctx, new_exp) =
                Context.match_against matches NONE new_exp
            in
              eval location new_exp new_ctx cont
            end
          (* If we are evaluating an `Efn`, it must have already existed.
           * As in, it was an anonymous function value, and its closure is just
           * the current closure.
           *
           * This breaks down if we ever call eval twice.
           *)
        | Efn (matches, _, _) => Cont.throw (cont, Efn (matches, ctx, []))
        | Ehole => raise Fail "shouldn't happen, evaling Ehole"
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
          (* If any of them say rec, then there can't be any meaningful
           * computation being done (they're all lambda expressions)
           *
           * So just add them to the context.
           *)
          if List.foldl (fn (x, y) => x orelse y) false (List.map #recc valbinds) then
            valbinds
            |> List.map (fn {recc, pat, exp} => {pat = pat, exp = exp})
            |> (fn matches => Efn (matches, ctx, []))
            |> Context.add_rec_bindings ctx
          else
            (* Otherwise, iterate on them and evaluate each valbinding
             * sequentially, in the original context.
             *)
            iter_list
              (fn ({recc, pat, exp}, rest, pairs) =>
                let
                  val new_exp =
                    checkpoint
                      ( DVALBINDS (tyvars, pat, rest) :: location)
                      exp
                      ctx
                in
                  match_pat pat new_exp @ pairs
                end
              )
              []
              valbinds
            |> (fn bindings => Context.add_bindings bindings ctx)
      | Dfun {fvalbinds, ...} =>
          let
            fun get_id_pats fname_args =
              case fname_args of
                Fprefix {opp, id, args} => (id, args)
              | Finfix {left, id, right} => (id, Ptuple [left, right])
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
                 let
                   val new_vars = List.tabulate (num_args, fn _ => new ())
                 in
                   ( id
                   , List.foldr
                       (fn (id, exp) =>
                         Efn ( { pat = Pident {opp = false, id = id}
                               , exp = exp
                               }
                             , ctx
                             , []
                             )
                       )
                       ( Ecase
                           { exp =
                              Etuple
                                (List.map (fn id => Eident {opp = false, id = [id]}))
                           , matches =
                              List.map
                                (fn (pats, exp) => { pat = Ptuple pats
                                                 , exp = exp
                                                 }
                                )
                                matches
                           }
                       )
                       matches
                   )
                 end

            val fvalbindings =
              List.map
                (fn fvalbind => get_fvalbind_binding fvalbind ctx)
                fvalbinds

          in
            Context.add_rec_bindings ctx fvalbindings
          end
      | Dtype typbinds =>
          Context.add_typbinds ctx typbinds
      | Ddatdec {datbinds = {conbinds, ...}, withtypee} =>
          (* TODO: recursive datatypes type wise *)
          List.foldl
            (fn ({opp, id, ty}, ctx) =>
              Context.add_con ctx id
            )
            (Context.add_datatype ctx datbinds)
            conbinds
      | Ddatrepl {right_tycon, ...} =>
          Context.replicate_datatype ctx right_tycon
      | Dabstype _ => raise Fail "no support for abstype right now"
      | Dexception exbinds =>
          (* TODO: type stuff *)
          List.foldl
            (fn (Xnew {opp, id, ...}, ctx) =>
                Context.add_exn ctx id
            | (Xrepl {left_id, right_id}, ctx) =>
                Context.replicate_exception ctx right_id

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
                    eval_dec (DLOCAL (decs, right_dec) :: location) dec ctx
                  )
                  (Context.enter_scope ctx)
                  decs
                |> Context.enter_scope
                |> (fn ctx => eval_dec location right_dec ctx)
                |> Context.pop_penultimate
            | _ =>
                eval_dec
                  (DLOCAL ([], right_dec) location)
                  left_dec
                  (Context.enter_scope ctx)
                |> Context.enter_scope
                |> (fn ctx => eval_dec location right_dec ctx)
                |> Context.pop_penultimate
          )
      | Dopen ids =>
          List.foldl
            (fn (id, ctx) => Context.open_scope ctx id)
            ctx
            ids
      | Dinfix {precedence, ids} =>
          List.foldl
            (fn (id, ctx) =>
              Context.add_infix ctx
                ( id
                , Context.LEFT
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
                , Context.RIGHT
                , Option.getOpt (precedence, 0)
                )
            )
            ctx
            ids
      | Dnonfix ids =>
          List.foldl
            (fn (id, ctx) =>
              Context.remove_infixity ctx id
            )
            ctx
            ids

    (* Should put all of the resulting bindings into the top-most scope.
     *)
    and eval_module location module ctx =
      case module of
        (* For a module identifier, open all the module's contents into the
         * current scope.
         *)
        Mident id => Context.open_scope_path ctx id

        (* For a module structure, evaluate the inner declarations and put their
         * changes into the current scope.
         *)
      | Mstruct strdec =>
          eval_strdec (MSTRUCT :: location) strdec ctx

        (* For a sealed module, evaluate the inner module, reify it as a scope,
         * then ascribe the scope to the seal
         *)
      | Mseal {module, opacity, signat} =>
          eval_module (MSEAL (opacity, signat) :: location) module ctx
          |> Context.pop_scope
          |> (fn scope => Context.ascribe scope (SOME {opacity = opacity, signat = signat}))

        (* For a functor application, evaluate the argument to a reified scope,
         * and then apply the functor to it to get a resulting scope. Then, open
         * that in the current context.
         *)
      | Mapp {functorr, arg} =>
          (case arg of
            Normal_app module =>
              eval_module (MAPP functorr :: location) module ctx
              |> Context.pop_scope
              |> Context.apply_functor ctx functorr
              |> Context.open_scope ctx
          | Sugar_app strdec =>
              eval_strdec (MAPP functorr :: location) strdec ctx
              |> Context.pop_scope
              |> Context.apply_functor ctx functorr
              |> Context.open_scope ctx
          )

          (* For a module let, evaluate the declaration in a new scope, and then
           * enter a new scope for the actual module itself. Get rid of the
           * local scope.
           * TODO: need to also open the current scope into the upper context
           *)
      | Mlet {dec, module} =>
          eval_dec (MLET module :: location) dec (Context.enter_scope ctx)
          |> Context.enter_scope
          |> eval_module location module
          |> Context.pop_penultimate


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
                (STRUCTS (id, seal, rest) :: location)
                module
                (Context.enter_scope ctx)
              )
              :: acc
            )
            []
            modules
          |> List.map (fn (id, seal, ctx) => (id, seal, Context.pop_scope ctx))
          |> List.map (fn (id, seal, scope) => (id, Context.ascribe scope seal))
          |> List.foldl (fn ((id, scope), ctx) =>
               Context.add_module ctx scope id
             ) ctx
      | DMlocal {left_dec, right_dec} =>
          ( case left_dec of
              DMseq decs =>
                iter_list
                  (fn (strdec, strdecs, ctx) =>
                    eval_strdec (DMLOCAL (strdecs, right_dec) :: location) strdec ctx
                  )
                  (Context.enter_scope ctx)
                  strdecs
                |> Context.enter_scope
                |> (fn ctx => eval_strdec location right_dec ctx)
                |> Context.pop_penultimate
            | _ =>
                eval_strdec
                  (DMLOCAL ([], right_dec) location)
                  left_dec
                  (Context.enter_scope ctx)
                |> Context.enter_scope
                |> (fn ctx => eval_strdec location right_dec ctx)
                |> Context.pop_penultimate
          )

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
              Context.generate_sigbindings ctx sigbind @ acc
            )
            []
            ctx
          |> Context.add_sigbindings ctx

      | Fundec fundec =>
          List.foldl
            (fn (funbind, ctx) => Context.add_funbind ctx funbind)
            ctx
            fundec

    fun eval_program ast ctx =
      iter_list
        (fn (topdec, topdecs, ctx) =>
          eval_topdec [PROG topdecs] topdec ctx
        )
        ctx
        ast
  end
