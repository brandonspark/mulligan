(* File generated by CM-Yacc version 2.1 *)

functor ParserFun
   (structure Streamable : STREAMABLE
    structure Arg :
       sig
          type int
          type longid
          type value
          type directive

          val main : directive -> directive
          val run : unit -> directive
          val typeof_id : longid -> directive
          val do_help : unit -> directive
          val num_last : int -> directive
          val bare_last : unit -> directive
          val report : longid -> directive
          val change_setting : longid * value -> directive
          val break_fn : longid -> directive
          val break_bind : longid -> directive
          val sym_print : longid -> directive
          val sym_clear : longid -> directive
          val bare_clear : unit -> directive
          val num_reveal : int -> directive
          val bare_reveal : unit -> directive
          val num_prev : int -> directive
          val prev : unit -> directive
          val stop : unit -> directive
          val evaluate : unit -> directive
          val step : unit -> directive
          val value_num : int -> value
          val value_ident : longid -> value

          datatype terminal =
             NUM of int
           | IDENT of longid
           | STEP
           | EVALUATE
           | REVEAL
           | STOP
           | EQUAL
           | SET
           | PREV
           | BREAKBIND
           | BREAKFN
           | RUN
           | CLEAR
           | PRINT
           | REPORT
           | LAST
           | HELP
           | TYPEOF
           | EOF

          val error : terminal Streamable.t -> exn
       end)
   :>
   sig
      val parse : Arg.terminal Streamable.t -> Arg.directive * Arg.terminal Streamable.t
   end
=

(*

AUTOMATON LISTING
=================

State 0:

start -> . Main  / 0
2 : Directive -> . STEP  / 1
3 : Directive -> . EVALUATE  / 1
4 : Directive -> . STOP  / 1
5 : Directive -> . PREV  / 1
6 : Directive -> . PREV NUM  / 1
7 : Directive -> . REVEAL  / 1
8 : Directive -> . REVEAL NUM  / 1
9 : Directive -> . CLEAR  / 1
10 : Directive -> . CLEAR IDENT  / 1
11 : Directive -> . PRINT IDENT  / 1
12 : Directive -> . BREAKBIND IDENT  / 1
13 : Directive -> . BREAKFN IDENT  / 1
14 : Directive -> . SET IDENT EQUAL Value  / 1
15 : Directive -> . REPORT IDENT  / 1
16 : Directive -> . LAST  / 1
17 : Directive -> . LAST NUM  / 1
18 : Directive -> . HELP  / 1
19 : Directive -> . TYPEOF IDENT  / 1
20 : Directive -> . RUN  / 1
21 : Main -> . Directive EOF  / 0

STEP => shift 7
EVALUATE => shift 6
REVEAL => shift 5
STOP => shift 4
SET => shift 3
PREV => shift 10
BREAKBIND => shift 9
BREAKFN => shift 8
RUN => shift 2
CLEAR => shift 12
PRINT => shift 15
REPORT => shift 14
LAST => shift 13
HELP => shift 16
TYPEOF => shift 11
Directive => goto 1
Main => goto 17

-----

State 1:

21 : Main -> Directive . EOF  / 0

EOF => shift 18

-----

State 2:

20 : Directive -> RUN .  / 1

EOF => reduce 20

-----

State 3:

14 : Directive -> SET . IDENT EQUAL Value  / 1

IDENT => shift 19

-----

State 4:

4 : Directive -> STOP .  / 1

EOF => reduce 4

-----

State 5:

7 : Directive -> REVEAL .  / 1
8 : Directive -> REVEAL . NUM  / 1

NUM => shift 20
EOF => reduce 7

-----

State 6:

3 : Directive -> EVALUATE .  / 1

EOF => reduce 3

-----

State 7:

2 : Directive -> STEP .  / 1

EOF => reduce 2

-----

State 8:

13 : Directive -> BREAKFN . IDENT  / 1

IDENT => shift 21

-----

State 9:

12 : Directive -> BREAKBIND . IDENT  / 1

IDENT => shift 22

-----

State 10:

5 : Directive -> PREV .  / 1
6 : Directive -> PREV . NUM  / 1

NUM => shift 23
EOF => reduce 5

-----

State 11:

19 : Directive -> TYPEOF . IDENT  / 1

IDENT => shift 24

-----

State 12:

9 : Directive -> CLEAR .  / 1
10 : Directive -> CLEAR . IDENT  / 1

IDENT => shift 25
EOF => reduce 9

-----

State 13:

16 : Directive -> LAST .  / 1
17 : Directive -> LAST . NUM  / 1

NUM => shift 26
EOF => reduce 16

-----

State 14:

15 : Directive -> REPORT . IDENT  / 1

IDENT => shift 27

-----

State 15:

11 : Directive -> PRINT . IDENT  / 1

IDENT => shift 28

-----

State 16:

18 : Directive -> HELP .  / 1

EOF => reduce 18

-----

State 17:

start -> Main .  / 0

$ => accept

-----

State 18:

21 : Main -> Directive EOF .  / 0

$ => reduce 21

-----

State 19:

14 : Directive -> SET IDENT . EQUAL Value  / 1

EQUAL => shift 29

-----

State 20:

8 : Directive -> REVEAL NUM .  / 1

EOF => reduce 8

-----

State 21:

13 : Directive -> BREAKFN IDENT .  / 1

EOF => reduce 13

-----

State 22:

12 : Directive -> BREAKBIND IDENT .  / 1

EOF => reduce 12

-----

State 23:

6 : Directive -> PREV NUM .  / 1

EOF => reduce 6

-----

State 24:

19 : Directive -> TYPEOF IDENT .  / 1

EOF => reduce 19

-----

State 25:

10 : Directive -> CLEAR IDENT .  / 1

EOF => reduce 10

-----

State 26:

17 : Directive -> LAST NUM .  / 1

EOF => reduce 17

-----

State 27:

15 : Directive -> REPORT IDENT .  / 1

EOF => reduce 15

-----

State 28:

11 : Directive -> PRINT IDENT .  / 1

EOF => reduce 11

-----

State 29:

0 : Value -> . IDENT  / 1
1 : Value -> . NUM  / 1
14 : Directive -> SET IDENT EQUAL . Value  / 1

NUM => shift 32
IDENT => shift 31
Value => goto 30

-----

State 30:

14 : Directive -> SET IDENT EQUAL Value .  / 1

EOF => reduce 14

-----

State 31:

0 : Value -> IDENT .  / 1

EOF => reduce 0

-----

State 32:

1 : Value -> NUM .  / 1

EOF => reduce 1

-----

lookahead 0 = $ 
lookahead 1 = EOF 

*)

struct
local
structure Value = struct
datatype nonterminal =
nonterminal
| int of Arg.int
| longid of Arg.longid
| value of Arg.value
| directive of Arg.directive
end
structure ParseEngine = ParseEngineFun (structure Streamable = Streamable
type terminal = Arg.terminal
type value = Value.nonterminal
val dummy = Value.nonterminal
fun read terminal =
(case terminal of
Arg.NUM x => (1, Value.int x)
| Arg.IDENT x => (2, Value.longid x)
| Arg.STEP => (3, Value.nonterminal)
| Arg.EVALUATE => (4, Value.nonterminal)
| Arg.REVEAL => (5, Value.nonterminal)
| Arg.STOP => (6, Value.nonterminal)
| Arg.EQUAL => (7, Value.nonterminal)
| Arg.SET => (8, Value.nonterminal)
| Arg.PREV => (9, Value.nonterminal)
| Arg.BREAKBIND => (10, Value.nonterminal)
| Arg.BREAKFN => (11, Value.nonterminal)
| Arg.RUN => (12, Value.nonterminal)
| Arg.CLEAR => (13, Value.nonterminal)
| Arg.PRINT => (14, Value.nonterminal)
| Arg.REPORT => (15, Value.nonterminal)
| Arg.LAST => (16, Value.nonterminal)
| Arg.HELP => (17, Value.nonterminal)
| Arg.TYPEOF => (18, Value.nonterminal)
| Arg.EOF => (19, Value.nonterminal)
)
)
in
val parse = ParseEngine.parse (
ParseEngine.next5x1 "\128\128\128\136\135\134\133\128\132\139\138\137\131\141\144\143\142\145\140\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128j\128\128\128\128\128\128\128\128\128\128\128\128\128\128\148\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128z\128\128\128\128\128\128\128\128\128\128\128\128\128\149\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128w\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128{\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128|\128\128\128\128\128\128\128\128\128\128\128\128\128\128\150\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\151\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\152\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128y\128\128\128\128\128\128\128\128\128\128\128\128\128\128\153\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128u\128\128\128\128\128\128\128\128\128\128\128\128\128\155\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128n\128\128\128\128\128\128\128\128\128\128\128\128\128\128\156\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\157\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128l\128\128\128\128\128\128\128\128\128\128\128\128\127\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128i\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\158\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128v\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128q\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128r\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128x\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128k\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128t\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128m\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128o\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128s\128\128\128\128\128\128\128\128\128\128\128\128\128\161\160\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128p\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128~\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128}\128\128\128\128\128\128\128\128\128\128\128\128",
ParseEngine.next5x1 "\128\129\145\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\158\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128",
Vector.fromList [(0,1,(fn Value.longid(arg0)::rest => Value.value(Arg.value_ident arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn Value.int(arg0)::rest => Value.value(Arg.value_num arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.step {})::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.evaluate {})::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.stop {})::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.prev {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_prev arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.bare_reveal {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_reveal arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.bare_clear {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.sym_clear arg0)::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.sym_print arg0)::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.break_bind arg0)::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.break_fn arg0)::rest|_=>raise (Fail "bad parser"))),
(1,4,(fn Value.value(arg0)::_::Value.longid(arg1)::_::rest => Value.directive(Arg.change_setting {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.report arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.bare_last {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_last arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.do_help {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.longid(arg0)::_::rest => Value.directive(Arg.typeof_id arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.directive(Arg.run {})::rest|_=>raise (Fail "bad parser"))),
(2,2,(fn _::Value.directive(arg0)::rest => Value.directive(Arg.main arg0)::rest|_=>raise (Fail "bad parser")))],
(fn Value.directive x => x | _ => raise (Fail "bad parser")), Arg.error)
end
end
