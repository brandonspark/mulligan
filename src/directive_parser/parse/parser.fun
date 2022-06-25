(* File generated by CM-Yacc version 2.1 *)

functor ParserFun
   (structure Streamable : STREAMABLE
    structure Arg :
       sig
          type int
          type symbol
          type directive

          val main : directive -> directive
          val run : unit -> directive
          val do_help : unit -> directive
          val num_last : int -> directive
          val bare_last : unit -> directive
          val report : symbol -> directive
          val change_setting_num : symbol * int -> directive
          val change_setting : symbol * symbol -> directive
          val break_bind : symbol -> directive
          val sym_print : symbol -> directive
          val sym_clear : symbol -> directive
          val bare_clear : unit -> directive
          val break_fn : symbol -> directive
          val num_reveal : int -> directive
          val bare_reveal : unit -> directive
          val num_prev : int -> directive
          val prev : unit -> directive
          val stop : unit -> directive
          val step : unit -> directive

          datatype terminal =
             NUM of int
           | SYMBOL of symbol
           | STEP
           | REVEAL
           | STOP
           | EQUAL
           | SET
           | PREV
           | BREAK
           | RUN
           | CLEAR
           | PRINT
           | BIND
           | REPORT
           | LAST
           | HELP
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
0 : Directive -> . STEP  / 1
1 : Directive -> . STOP  / 1
2 : Directive -> . PREV  / 1
3 : Directive -> . PREV NUM  / 1
4 : Directive -> . REVEAL  / 1
5 : Directive -> . REVEAL NUM  / 1
6 : Directive -> . BREAK SYMBOL  / 1
7 : Directive -> . CLEAR  / 1
8 : Directive -> . CLEAR SYMBOL  / 1
9 : Directive -> . PRINT SYMBOL  / 1
10 : Directive -> . BREAK BIND SYMBOL  / 1
11 : Directive -> . SET SYMBOL EQUAL SYMBOL  / 1
12 : Directive -> . SET SYMBOL EQUAL NUM  / 1
13 : Directive -> . REPORT SYMBOL  / 1
14 : Directive -> . LAST  / 1
15 : Directive -> . LAST NUM  / 1
16 : Directive -> . HELP  / 1
17 : Directive -> . RUN  / 1
18 : Main -> . Directive EOF  / 0

STEP => shift 7
REVEAL => shift 6
STOP => shift 5
SET => shift 4
PREV => shift 8
BREAK => shift 3
RUN => shift 2
CLEAR => shift 10
PRINT => shift 12
REPORT => shift 11
LAST => shift 13
HELP => shift 9
Directive => goto 1
Main => goto 14

-----

State 1:

18 : Main -> Directive . EOF  / 0

EOF => shift 15

-----

State 2:

17 : Directive -> RUN .  / 1

EOF => reduce 17

-----

State 3:

6 : Directive -> BREAK . SYMBOL  / 1
10 : Directive -> BREAK . BIND SYMBOL  / 1

SYMBOL => shift 17
BIND => shift 16

-----

State 4:

11 : Directive -> SET . SYMBOL EQUAL SYMBOL  / 1
12 : Directive -> SET . SYMBOL EQUAL NUM  / 1

SYMBOL => shift 18

-----

State 5:

1 : Directive -> STOP .  / 1

EOF => reduce 1

-----

State 6:

4 : Directive -> REVEAL .  / 1
5 : Directive -> REVEAL . NUM  / 1

NUM => shift 19
EOF => reduce 4

-----

State 7:

0 : Directive -> STEP .  / 1

EOF => reduce 0

-----

State 8:

2 : Directive -> PREV .  / 1
3 : Directive -> PREV . NUM  / 1

NUM => shift 20
EOF => reduce 2

-----

State 9:

16 : Directive -> HELP .  / 1

EOF => reduce 16

-----

State 10:

7 : Directive -> CLEAR .  / 1
8 : Directive -> CLEAR . SYMBOL  / 1

SYMBOL => shift 21
EOF => reduce 7

-----

State 11:

13 : Directive -> REPORT . SYMBOL  / 1

SYMBOL => shift 22

-----

State 12:

9 : Directive -> PRINT . SYMBOL  / 1

SYMBOL => shift 23

-----

State 13:

14 : Directive -> LAST .  / 1
15 : Directive -> LAST . NUM  / 1

NUM => shift 24
EOF => reduce 14

-----

State 14:

start -> Main .  / 0

$ => accept

-----

State 15:

18 : Main -> Directive EOF .  / 0

$ => reduce 18

-----

State 16:

10 : Directive -> BREAK BIND . SYMBOL  / 1

SYMBOL => shift 25

-----

State 17:

6 : Directive -> BREAK SYMBOL .  / 1

EOF => reduce 6

-----

State 18:

11 : Directive -> SET SYMBOL . EQUAL SYMBOL  / 1
12 : Directive -> SET SYMBOL . EQUAL NUM  / 1

EQUAL => shift 26

-----

State 19:

5 : Directive -> REVEAL NUM .  / 1

EOF => reduce 5

-----

State 20:

3 : Directive -> PREV NUM .  / 1

EOF => reduce 3

-----

State 21:

8 : Directive -> CLEAR SYMBOL .  / 1

EOF => reduce 8

-----

State 22:

13 : Directive -> REPORT SYMBOL .  / 1

EOF => reduce 13

-----

State 23:

9 : Directive -> PRINT SYMBOL .  / 1

EOF => reduce 9

-----

State 24:

15 : Directive -> LAST NUM .  / 1

EOF => reduce 15

-----

State 25:

10 : Directive -> BREAK BIND SYMBOL .  / 1

EOF => reduce 10

-----

State 26:

11 : Directive -> SET SYMBOL EQUAL . SYMBOL  / 1
12 : Directive -> SET SYMBOL EQUAL . NUM  / 1

NUM => shift 27
SYMBOL => shift 28

-----

State 27:

12 : Directive -> SET SYMBOL EQUAL NUM .  / 1

EOF => reduce 12

-----

State 28:

11 : Directive -> SET SYMBOL EQUAL SYMBOL .  / 1

EOF => reduce 11

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
| symbol of Arg.symbol
| directive of Arg.directive
end
structure ParseEngine = ParseEngineFun (structure Streamable = Streamable
type terminal = Arg.terminal
type value = Value.nonterminal
val dummy = Value.nonterminal
fun read terminal =
(case terminal of
Arg.NUM x => (1, Value.int x)
| Arg.SYMBOL x => (2, Value.symbol x)
| Arg.STEP => (3, Value.nonterminal)
| Arg.REVEAL => (4, Value.nonterminal)
| Arg.STOP => (5, Value.nonterminal)
| Arg.EQUAL => (6, Value.nonterminal)
| Arg.SET => (7, Value.nonterminal)
| Arg.PREV => (8, Value.nonterminal)
| Arg.BREAK => (9, Value.nonterminal)
| Arg.RUN => (10, Value.nonterminal)
| Arg.CLEAR => (11, Value.nonterminal)
| Arg.PRINT => (12, Value.nonterminal)
| Arg.BIND => (13, Value.nonterminal)
| Arg.REPORT => (14, Value.nonterminal)
| Arg.LAST => (15, Value.nonterminal)
| Arg.HELP => (16, Value.nonterminal)
| Arg.EOF => (17, Value.nonterminal)
)
)
in
val parse = ParseEngine.parse (
ParseEngine.next5x1 "\128\128\128\136\135\134\128\133\137\132\131\139\141\128\140\142\138\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\144\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128m\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\146\128\128\128\128\128\128\128\128\128\128\145\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128}\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\148\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128z\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128~\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\149\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128|\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128n\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\150\128\128\128\128\128\128\128\128\128\128\128\128\128\128w\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\151\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\152\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\153\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128p\128\128\128\128\128\128\128\128\128\128\128\128\128\128\127\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128l\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128x\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\155\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128y\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128{\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128v\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128q\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128u\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128o\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128t\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\156\157\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128r\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128s\128\128\128\128\128\128\128\128\128\128\128\128\128\128",
ParseEngine.next5x1 "\129\142\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128",
Vector.fromList [(0,1,(fn _::rest => Value.directive(Arg.step {})::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.stop {})::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.prev {})::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_prev arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.bare_reveal {})::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_reveal arg0)::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.symbol(arg0)::_::rest => Value.directive(Arg.break_fn arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.bare_clear {})::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.symbol(arg0)::_::rest => Value.directive(Arg.sym_clear arg0)::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.symbol(arg0)::_::rest => Value.directive(Arg.sym_print arg0)::rest|_=>raise (Fail "bad parser"))),
(0,3,(fn Value.symbol(arg0)::_::_::rest => Value.directive(Arg.break_bind arg0)::rest|_=>raise (Fail "bad parser"))),
(0,4,(fn Value.symbol(arg0)::_::Value.symbol(arg1)::_::rest => Value.directive(Arg.change_setting {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(0,4,(fn Value.int(arg0)::_::Value.symbol(arg1)::_::rest => Value.directive(Arg.change_setting_num {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.symbol(arg0)::_::rest => Value.directive(Arg.report arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.bare_last {})::rest|_=>raise (Fail "bad parser"))),
(0,2,(fn Value.int(arg0)::_::rest => Value.directive(Arg.num_last arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.do_help {})::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn _::rest => Value.directive(Arg.run {})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn _::Value.directive(arg0)::rest => Value.directive(Arg.main arg0)::rest|_=>raise (Fail "bad parser")))],
(fn Value.directive x => x | _ => raise (Fail "bad parser")), Arg.error)
end
end