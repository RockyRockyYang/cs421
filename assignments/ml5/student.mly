/* Use the expression datatype defined in expressions.ml: */
%{
  open Common
(* You may want to add extra code here *)
  let andsugar l r = IfExp(l, r, ConstExp(BoolConst false))
  let orsugar l r = IfExp(l,ConstExp(BoolConst true),r)
  let ltsugar l r = BinOpAppExp(GreaterOp,r,l)
  let leqsugar l r = orsugar (ltsugar l r) (BinOpAppExp(EqOp, l, r))
  let geqsugar l r = orsugar (BinOpAppExp(GreaterOp,l,r)) (BinOpAppExp(EqOp, l, r))
  let neqsugar l r = BinOpAppExp(EqOp, BinOpAppExp (EqOp,l,r), ConstExp(BoolConst false))
%}


/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW SEMI DSEMI DCOLON NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE UNIT
       HEAD TAIL PRINT FST SND EOF

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Common.dec> main

%%

main:
    expression DSEMI            { (Anon ( $1)) }
  | LET IDENT EQUALS expression DSEMI             { (Let ($2,$4)) }
  | LET REC IDENT IDENT EQUALS expression DSEMI    { (LetRec ($3, $4, $6)) }

expression:
  |let_rec_in  {$1}

let_rec_in:
  |let_in  {$1}
  |LET REC IDENT IDENT EQUALS let_in IN let_in { LetRecInExp ($3, $4, $6, $8)}

let_in:
  |fun_ {$1}
  |LET IDENT EQUALS fun_ IN fun_ { LetInExp ($2, $4, $6)}

fun_:
  |if_then_else {$1}
  |FUN IDENT ARROW if_then_else {FunExp ($2, $4)}

if_then_else:
  |or_ {$1}
  |IF or_ THEN expression ELSE expression {IfExp ($2, $4, $6)}

or_:
  |and_ {$1}
  |or_ LOGICALOR and_ {IfExp ($1, ConstExp (BoolConst true), $3)}

and_:
  |comparison {$1}
  |and_ LOGICALAND comparison {IfExp ($1, $3, ConstExp (BoolConst false))}

comparison:
  |double_colon  {$1}
  |comparison LT double_colon {BinOpAppExp (GreaterOp, $3, $1)}
  |comparison GT double_colon {BinOpAppExp (GreaterOp, $1, $3)}
  |comparison LEQ double_colon {IfExp (BinOpAppExp (GreaterOp, $3, $1), ConstExp (BoolConst true), BinOpAppExp (EqOp, $1, $3))}
  |comparison GEQ double_colon {IfExp (BinOpAppExp (GreaterOp, $1, $3), ConstExp (BoolConst true), BinOpAppExp (EqOp, $1, $3))}
  |comparison EQUALS double_colon {BinOpAppExp (EqOp, $1, $3)}
  |comparison NEQ double_colon {IfExp (BinOpAppExp (EqOp, $1, $3), ConstExp (BoolConst false), ConstExp (BoolConst true))}

double_colon:
  |plus_minus {$1}
  |plus_minus DCOLON double_colon {BinOpAppExp (ConsOp, $1, $3)}

plus_minus:
  |times_div {$1}
  |plus_minus PLUS times_div {BinOpAppExp (IntPlusOp, $1, $3)}
  |plus_minus MINUS times_div {BinOpAppExp (IntMinusOp, $1, $3)}
  |plus_minus DPLUS times_div {BinOpAppExp (FloatPlusOp, $1, $3)}
  |plus_minus DMINUS times_div {BinOpAppExp (FloatMinusOp, $1, $3)}
  |plus_minus CARAT times_div {BinOpAppExp (ConcatOp, $1, $3)}

times_div:
  |exponetial {$1}
  |times_div TIMES exponetial {BinOpAppExp (IntTimesOp, $1, $3)}
  |times_div DIV exponetial {BinOpAppExp (IntDivOp, $1, $3)}
  |times_div DTIMES exponetial {BinOpAppExp (FloatTimesOp, $1, $3)}
  |times_div DDIV exponetial {BinOpAppExp (FloatDivOp, $1, $3)}
  |times_div MOD exponetial {BinOpAppExp (ModOp, $1, $3)}

exponetial:
  |raise_ {$1}
  |raise_ EXP exponetial {BinOpAppExp (ExpoOp, $1, $3)}

raise_:
  |application {$1}
  |RAISE application {RaiseExp $2}

application:
  |unary {$1}
  |application unary {AppExp ($1, $2)}

unary:
  |atomic_expression {$1}
  |FST atomic_expression {MonOpAppExp (FstOp, $2)}
  |SND atomic_expression {MonOpAppExp (SndOp, $2)}
  |HEAD atomic_expression {MonOpAppExp (HdOp, $2)}
  |TAIL atomic_expression {MonOpAppExp (TlOp, $2)}
  |PRINT atomic_expression {MonOpAppExp (PrintOp, $2)}
  |NEG atomic_expression {MonOpAppExp (IntNegOp, $2)}

atomic_expression: 
  |INT    {ConstExp (IntConst $1)}
  |FLOAT  {ConstExp (FloatConst $1)}
  |UNIT   {ConstExp (UnitConst)}
  |TRUE   {ConstExp (BoolConst true)}
  |FALSE  {ConstExp (BoolConst false)}
  |STRING {ConstExp (StringConst $1)}
  |NIL    {ConstExp (NilConst)}
  |IDENT     { VarExp $1 }
  |LPAREN expression RPAREN {$2}
  |LPAREN expression COMMA expression RPAREN {BinOpAppExp (CommaOp, $2, $4)}
  |list_expression_left {$1}

list_expression_left:
  | LBRAC list_expression_in {$2}

list_expression_in:
  | RBRAC {ConstExp NilConst}
  | expression RBRAC {BinOpAppExp (ConsOp, $1, ConstExp NilConst)}
  | expression SEMI list_expression_in {BinOpAppExp (ConsOp, $1, $3)}