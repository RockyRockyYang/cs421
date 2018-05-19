type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | IDENT of (string)
  | TRUE
  | FALSE
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | DPLUS
  | DMINUS
  | DTIMES
  | DDIV
  | MOD
  | EXP
  | CARAT
  | LT
  | GT
  | LEQ
  | GEQ
  | EQUALS
  | NEQ
  | PIPE
  | ARROW
  | SEMI
  | DSEMI
  | DCOLON
  | NIL
  | LET
  | REC
  | AND
  | IN
  | IF
  | THEN
  | ELSE
  | FUN
  | RAISE
  | TRY
  | WITH
  | NOT
  | LOGICALAND
  | LOGICALOR
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | COMMA
  | UNDERSCORE
  | UNIT
  | HEAD
  | TAIL
  | PRINT
  | FST
  | SND
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "student.mly"
  open Common
(* You may want to add extra code here *)
  let andsugar l r = IfExp(l, r, ConstExp(BoolConst false))
  let orsugar l r = IfExp(l,ConstExp(BoolConst true),r)
  let ltsugar l r = BinOpAppExp(GreaterOp,r,l)
  let leqsugar l r = orsugar (ltsugar l r) (BinOpAppExp(EqOp, l, r))
  let geqsugar l r = orsugar (BinOpAppExp(GreaterOp,l,r)) (BinOpAppExp(EqOp, l, r))
  let neqsugar l r = BinOpAppExp(EqOp, BinOpAppExp (EqOp,l,r), ConstExp(BoolConst false))
# 72 "student.ml"
let yytransl_const = [|
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* NEG *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIV *);
  268 (* DPLUS *);
  269 (* DMINUS *);
  270 (* DTIMES *);
  271 (* DDIV *);
  272 (* MOD *);
  273 (* EXP *);
  274 (* CARAT *);
  275 (* LT *);
  276 (* GT *);
  277 (* LEQ *);
  278 (* GEQ *);
  279 (* EQUALS *);
  280 (* NEQ *);
  281 (* PIPE *);
  282 (* ARROW *);
  283 (* SEMI *);
  284 (* DSEMI *);
  285 (* DCOLON *);
  286 (* NIL *);
  287 (* LET *);
  288 (* REC *);
  289 (* AND *);
  290 (* IN *);
  291 (* IF *);
  292 (* THEN *);
  293 (* ELSE *);
  294 (* FUN *);
  295 (* RAISE *);
  296 (* TRY *);
  297 (* WITH *);
  298 (* NOT *);
  299 (* LOGICALAND *);
  300 (* LOGICALOR *);
  301 (* LBRAC *);
  302 (* RBRAC *);
  303 (* LPAREN *);
  304 (* RPAREN *);
  305 (* COMMA *);
  306 (* UNDERSCORE *);
  307 (* UNIT *);
  308 (* HEAD *);
  309 (* TAIL *);
  310 (* PRINT *);
  311 (* FST *);
  312 (* SND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\007\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\013\000\013\000\014\000\
\014\000\015\000\015\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\018\000\019\000\019\000\
\019\000\000\000"

let yylen = "\002\000\
\002\000\005\000\007\000\001\000\001\000\008\000\001\000\006\000\
\001\000\004\000\001\000\006\000\001\000\003\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\001\000\003\000\001\000\
\002\000\001\000\002\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\005\000\001\000\002\000\001\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\051\000\052\000\056\000\058\000\054\000\055\000\
\000\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
\053\000\000\000\000\000\000\000\000\000\000\000\066\000\000\000\
\004\000\005\000\007\000\009\000\000\000\000\000\000\000\017\000\
\000\000\000\000\032\000\000\000\000\000\042\000\044\000\061\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\063\000\
\000\000\062\000\000\000\047\000\048\000\049\000\045\000\046\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\064\000\059\000\000\000\
\000\000\000\000\018\000\019\000\020\000\021\000\022\000\023\000\
\000\000\000\000\000\000\000\000\000\000\025\000\033\000\034\000\
\035\000\036\000\037\000\039\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\065\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\060\000\008\000\000\000\000\000\012\000\
\000\000\003\000\000\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\023\000\049\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\050\000"

let yysindex = "\013\000\
\058\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\195\255\000\000\005\255\050\000\022\255\139\255\000\255\120\255\
\000\000\195\255\195\255\195\255\195\255\195\255\000\000\247\254\
\000\000\000\000\000\000\000\000\244\254\249\254\109\255\000\000\
\015\255\084\255\000\000\017\255\139\255\000\000\000\000\000\000\
\000\000\018\255\038\255\233\254\023\255\139\255\008\255\000\000\
\239\254\000\000\225\254\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\000\000\120\255\046\255\
\120\255\032\000\035\255\073\255\000\255\000\000\000\000\120\255\
\249\254\109\255\000\000\000\000\000\000\000\000\000\000\000\000\
\084\255\084\255\084\255\084\255\084\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\255\047\255\060\255\048\255\
\000\000\232\255\080\255\000\000\042\255\000\000\232\255\120\255\
\120\255\047\255\063\255\000\000\000\000\064\255\057\255\000\000\
\176\255\000\000\176\255\097\255\057\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\002\000\002\217\255\000\000\
\210\001\225\000\000\000\183\000\099\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\141\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\002\233\001\000\000\000\000\000\000\000\000\000\000\000\000\
\011\001\053\001\095\001\137\001\179\001\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\076\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\147\255\185\255\024\000\095\000\057\000\
\049\000\011\000\000\000\094\000\062\000\000\000\102\000\235\255\
\048\000\000\000\033\000"

let yytablesize = 844
let yytable = "\024\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\110\000\
\042\000\085\000\127\000\083\000\081\000\001\000\051\000\078\000\
\087\000\088\000\057\000\133\000\058\000\134\000\066\000\067\000\
\078\000\045\000\068\000\069\000\086\000\010\000\047\000\058\000\
\070\000\077\000\012\000\059\000\043\000\013\000\014\000\084\000\
\079\000\080\000\122\000\071\000\015\000\048\000\016\000\125\000\
\082\000\111\000\017\000\018\000\019\000\020\000\021\000\022\000\
\041\000\114\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\052\000\053\000\054\000\055\000\056\000\091\000\092\000\
\093\000\094\000\095\000\096\000\115\000\109\000\118\000\112\000\
\119\000\102\000\120\000\123\000\121\000\129\000\117\000\010\000\
\011\000\124\000\131\000\130\000\012\000\072\000\073\000\013\000\
\014\000\074\000\075\000\076\000\083\000\007\000\015\000\005\000\
\016\000\113\000\044\000\090\000\017\000\018\000\019\000\020\000\
\021\000\022\000\089\000\046\000\000\000\116\000\126\000\128\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\060\000\
\061\000\062\000\063\000\064\000\065\000\103\000\104\000\105\000\
\106\000\107\000\108\000\003\000\004\000\005\000\006\000\007\000\
\008\000\009\000\000\000\000\000\000\000\010\000\047\000\000\000\
\000\000\000\000\012\000\000\000\000\000\013\000\014\000\097\000\
\098\000\099\000\100\000\101\000\015\000\000\000\016\000\000\000\
\010\000\000\000\017\000\018\000\019\000\020\000\021\000\022\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\015\000\
\000\000\016\000\000\000\000\000\000\000\017\000\018\000\019\000\
\020\000\021\000\022\000\003\000\004\000\005\000\006\000\007\000\
\008\000\000\000\000\000\000\000\000\000\010\000\132\000\000\000\
\000\000\000\000\012\000\000\000\000\000\013\000\014\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\016\000\000\000\
\010\000\000\000\017\000\018\000\019\000\020\000\021\000\022\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\015\000\
\000\000\016\000\000\000\015\000\015\000\017\000\000\000\000\000\
\000\000\000\000\015\000\000\000\015\000\015\000\000\000\000\000\
\000\000\000\000\000\000\015\000\015\000\010\000\015\000\000\000\
\015\000\015\000\012\000\000\000\000\000\013\000\014\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\016\000\000\000\
\000\000\000\000\017\000\018\000\019\000\020\000\021\000\022\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\016\000\010\000\
\000\000\000\000\017\000\018\000\019\000\020\000\021\000\022\000\
\014\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\016\000\000\000\000\000\000\000\017\000\018\000\019\000\020\000\
\021\000\022\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\000\000\000\000\040\000\040\000\040\000\
\000\000\000\000\000\000\000\000\040\000\000\000\040\000\040\000\
\000\000\000\000\000\000\000\000\000\000\040\000\040\000\000\000\
\040\000\000\000\040\000\040\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\000\000\000\000\041\000\
\041\000\041\000\000\000\000\000\000\000\000\000\041\000\000\000\
\041\000\041\000\000\000\000\000\000\000\000\000\000\000\041\000\
\041\000\000\000\041\000\000\000\041\000\041\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\000\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\000\000\
\000\000\038\000\038\000\038\000\000\000\000\000\000\000\000\000\
\038\000\000\000\038\000\038\000\000\000\000\000\000\000\000\000\
\000\000\038\000\038\000\000\000\038\000\000\000\038\000\038\000\
\026\000\026\000\000\000\000\000\026\000\026\000\000\000\000\000\
\000\000\000\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\000\000\000\000\026\000\026\000\026\000\000\000\000\000\
\000\000\000\000\026\000\000\000\026\000\026\000\000\000\000\000\
\000\000\000\000\000\000\026\000\026\000\000\000\026\000\000\000\
\026\000\026\000\027\000\027\000\000\000\000\000\027\000\027\000\
\000\000\000\000\000\000\000\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\000\000\000\000\027\000\027\000\027\000\
\000\000\000\000\000\000\000\000\027\000\000\000\027\000\027\000\
\000\000\000\000\000\000\000\000\000\000\027\000\027\000\000\000\
\027\000\000\000\027\000\027\000\028\000\028\000\000\000\000\000\
\028\000\028\000\000\000\000\000\000\000\000\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\000\000\000\000\028\000\
\028\000\028\000\000\000\000\000\000\000\000\000\028\000\000\000\
\028\000\028\000\000\000\000\000\000\000\000\000\000\000\028\000\
\028\000\000\000\028\000\000\000\028\000\028\000\029\000\029\000\
\000\000\000\000\029\000\029\000\000\000\000\000\000\000\000\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\000\000\
\000\000\029\000\029\000\029\000\000\000\000\000\000\000\000\000\
\029\000\000\000\029\000\029\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\000\000\029\000\000\000\029\000\029\000\
\030\000\030\000\000\000\000\000\030\000\030\000\000\000\000\000\
\000\000\000\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\000\000\000\000\030\000\030\000\030\000\000\000\000\000\
\000\000\000\000\030\000\000\000\030\000\030\000\000\000\000\000\
\000\000\000\000\000\000\030\000\030\000\000\000\030\000\000\000\
\030\000\030\000\031\000\031\000\000\000\000\000\031\000\031\000\
\000\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\000\000\000\000\031\000\031\000\031\000\
\000\000\000\000\000\000\000\000\031\000\000\000\031\000\031\000\
\000\000\000\000\000\000\000\000\000\000\031\000\031\000\000\000\
\031\000\000\000\031\000\031\000\024\000\024\000\024\000\024\000\
\024\000\024\000\000\000\000\000\024\000\024\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\024\000\024\000\000\000\
\000\000\000\000\000\000\000\000\024\000\024\000\000\000\024\000\
\000\000\024\000\024\000\016\000\016\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\016\000\016\000\000\000\000\000\
\000\000\000\000\000\000\016\000\016\000\000\000\016\000\000\000\
\016\000\016\000\013\000\013\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\013\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\013\000\000\000\013\000\
\013\000\014\000\014\000\000\000\000\000\011\000\011\000\000\000\
\014\000\000\000\014\000\014\000\011\000\000\000\000\000\011\000\
\000\000\000\000\014\000\000\000\014\000\000\000\014\000\014\000\
\011\000\000\000\011\000\011\000"

let yycheck = "\001\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\079\000\
\004\001\027\001\120\000\004\001\036\001\001\000\016\000\037\000\
\048\001\049\001\028\001\129\000\044\001\131\000\008\001\009\001\
\046\000\004\001\012\001\013\001\046\001\030\001\031\001\044\001\
\018\001\017\001\035\001\043\001\032\001\038\001\039\001\032\001\
\023\001\004\001\114\000\029\001\045\001\046\001\047\001\119\000\
\026\001\004\001\051\001\052\001\053\001\054\001\055\001\056\001\
\009\000\023\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\018\000\019\000\020\000\021\000\022\000\060\000\061\000\
\062\000\063\000\064\000\065\000\004\001\079\000\028\001\081\000\
\034\001\071\000\023\001\004\001\037\001\023\001\088\000\030\001\
\031\001\048\001\034\001\028\001\035\001\010\001\011\001\038\001\
\039\001\014\001\015\001\016\001\004\001\028\001\045\001\028\001\
\047\001\082\000\012\000\059\000\051\001\052\001\053\001\054\001\
\055\001\056\001\058\000\014\000\255\255\085\000\120\000\121\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\019\001\
\020\001\021\001\022\001\023\001\024\001\072\000\073\000\074\000\
\075\000\076\000\077\000\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\255\255\255\255\255\255\030\001\031\001\255\255\
\255\255\255\255\035\001\255\255\255\255\038\001\039\001\066\000\
\067\000\068\000\069\000\070\000\045\001\255\255\047\001\255\255\
\030\001\255\255\051\001\052\001\053\001\054\001\055\001\056\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\045\001\
\255\255\047\001\255\255\255\255\255\255\051\001\052\001\053\001\
\054\001\055\001\056\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\255\255\030\001\031\001\255\255\
\255\255\255\255\035\001\255\255\255\255\038\001\039\001\255\255\
\255\255\255\255\255\255\255\255\045\001\255\255\047\001\255\255\
\030\001\255\255\051\001\052\001\053\001\054\001\055\001\056\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\045\001\
\255\255\047\001\255\255\027\001\028\001\051\001\255\255\255\255\
\255\255\255\255\034\001\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\255\255\043\001\044\001\030\001\046\001\255\255\
\048\001\049\001\035\001\255\255\255\255\038\001\039\001\255\255\
\255\255\255\255\255\255\255\255\045\001\255\255\047\001\255\255\
\255\255\255\255\051\001\052\001\053\001\054\001\055\001\056\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\255\255\255\255\255\255\255\255\030\001\255\255\255\255\
\255\255\255\255\035\001\255\255\255\255\255\255\039\001\255\255\
\255\255\255\255\255\255\255\255\045\001\255\255\047\001\030\001\
\255\255\255\255\051\001\052\001\053\001\054\001\055\001\056\001\
\039\001\255\255\255\255\255\255\255\255\255\255\045\001\255\255\
\047\001\255\255\255\255\255\255\051\001\052\001\053\001\054\001\
\055\001\056\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\255\255\255\255\255\255\255\255\034\001\255\255\036\001\037\001\
\255\255\255\255\255\255\255\255\255\255\043\001\044\001\255\255\
\046\001\255\255\048\001\049\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\029\001\255\255\255\255\255\255\255\255\034\001\255\255\
\036\001\037\001\255\255\255\255\255\255\255\255\255\255\043\001\
\044\001\255\255\046\001\255\255\048\001\049\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\034\001\255\255\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\043\001\044\001\255\255\046\001\255\255\048\001\049\001\
\008\001\009\001\255\255\255\255\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\255\255\255\255\
\255\255\255\255\034\001\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\255\255\043\001\044\001\255\255\046\001\255\255\
\048\001\049\001\008\001\009\001\255\255\255\255\012\001\013\001\
\255\255\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\255\255\255\255\255\255\255\255\034\001\255\255\036\001\037\001\
\255\255\255\255\255\255\255\255\255\255\043\001\044\001\255\255\
\046\001\255\255\048\001\049\001\008\001\009\001\255\255\255\255\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\029\001\255\255\255\255\255\255\255\255\034\001\255\255\
\036\001\037\001\255\255\255\255\255\255\255\255\255\255\043\001\
\044\001\255\255\046\001\255\255\048\001\049\001\008\001\009\001\
\255\255\255\255\012\001\013\001\255\255\255\255\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\034\001\255\255\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\043\001\044\001\255\255\046\001\255\255\048\001\049\001\
\008\001\009\001\255\255\255\255\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\255\255\255\255\
\255\255\255\255\034\001\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\255\255\043\001\044\001\255\255\046\001\255\255\
\048\001\049\001\008\001\009\001\255\255\255\255\012\001\013\001\
\255\255\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\255\255\255\255\255\255\255\255\034\001\255\255\036\001\037\001\
\255\255\255\255\255\255\255\255\255\255\043\001\044\001\255\255\
\046\001\255\255\048\001\049\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\028\001\255\255\255\255\
\255\255\255\255\255\255\034\001\255\255\036\001\037\001\255\255\
\255\255\255\255\255\255\255\255\043\001\044\001\255\255\046\001\
\255\255\048\001\049\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\034\001\255\255\036\001\037\001\255\255\255\255\
\255\255\255\255\255\255\043\001\044\001\255\255\046\001\255\255\
\048\001\049\001\027\001\028\001\255\255\255\255\255\255\255\255\
\255\255\034\001\255\255\036\001\037\001\255\255\255\255\255\255\
\255\255\255\255\255\255\044\001\255\255\046\001\255\255\048\001\
\049\001\027\001\028\001\255\255\255\255\027\001\028\001\255\255\
\034\001\255\255\036\001\037\001\034\001\255\255\255\255\037\001\
\255\255\255\255\044\001\255\255\046\001\255\255\048\001\049\001\
\046\001\255\255\048\001\049\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  NEG\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  DPLUS\000\
  DMINUS\000\
  DTIMES\000\
  DDIV\000\
  MOD\000\
  EXP\000\
  CARAT\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  EQUALS\000\
  NEQ\000\
  PIPE\000\
  ARROW\000\
  SEMI\000\
  DSEMI\000\
  DCOLON\000\
  NIL\000\
  LET\000\
  REC\000\
  AND\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FUN\000\
  RAISE\000\
  TRY\000\
  WITH\000\
  NOT\000\
  LOGICALAND\000\
  LOGICALOR\000\
  LBRAC\000\
  RBRAC\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  UNDERSCORE\000\
  UNIT\000\
  HEAD\000\
  TAIL\000\
  PRINT\000\
  FST\000\
  SND\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 31 "student.mly"
                                ( (Anon ( _1)) )
# 512 "student.ml"
               : Common.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 32 "student.mly"
                                                  ( (Let (_2,_4)) )
# 520 "student.ml"
               : Common.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 33 "student.mly"
                                                   ( (LetRec (_3, _4, _6)) )
# 529 "student.ml"
               : Common.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_rec_in) in
    Obj.repr(
# 36 "student.mly"
               (_1)
# 536 "student.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_in) in
    Obj.repr(
# 39 "student.mly"
           (_1)
# 543 "student.ml"
               : 'let_rec_in))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'let_in) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'let_in) in
    Obj.repr(
# 40 "student.mly"
                                               ( LetRecInExp (_3, _4, _6, _8))
# 553 "student.ml"
               : 'let_rec_in))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_) in
    Obj.repr(
# 43 "student.mly"
        (_1)
# 560 "student.ml"
               : 'let_in))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fun_) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'fun_) in
    Obj.repr(
# 44 "student.mly"
                                 ( LetInExp (_2, _4, _6))
# 569 "student.ml"
               : 'let_in))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_then_else) in
    Obj.repr(
# 47 "student.mly"
                (_1)
# 576 "student.ml"
               : 'fun_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'if_then_else) in
    Obj.repr(
# 48 "student.mly"
                                (FunExp (_2, _4))
# 584 "student.ml"
               : 'fun_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_) in
    Obj.repr(
# 51 "student.mly"
       (_1)
# 591 "student.ml"
               : 'if_then_else))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'or_) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 52 "student.mly"
                                          (IfExp (_2, _4, _6))
# 600 "student.ml"
               : 'if_then_else))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_) in
    Obj.repr(
# 55 "student.mly"
        (_1)
# 607 "student.ml"
               : 'or_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_) in
    Obj.repr(
# 56 "student.mly"
                      (IfExp (_1, ConstExp (BoolConst true), _3))
# 615 "student.ml"
               : 'or_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 59 "student.mly"
              (_1)
# 622 "student.ml"
               : 'and_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 60 "student.mly"
                              (IfExp (_1, _3, ConstExp (BoolConst false)))
# 630 "student.ml"
               : 'and_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 63 "student.mly"
                 (_1)
# 637 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 64 "student.mly"
                              (BinOpAppExp (GreaterOp, _3, _1))
# 645 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 65 "student.mly"
                              (BinOpAppExp (GreaterOp, _1, _3))
# 653 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 66 "student.mly"
                               (IfExp (BinOpAppExp (GreaterOp, _3, _1), ConstExp (BoolConst true), BinOpAppExp (EqOp, _1, _3)))
# 661 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 67 "student.mly"
                               (IfExp (BinOpAppExp (GreaterOp, _1, _3), ConstExp (BoolConst true), BinOpAppExp (EqOp, _1, _3)))
# 669 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 68 "student.mly"
                                  (BinOpAppExp (EqOp, _1, _3))
# 677 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 69 "student.mly"
                               (IfExp (BinOpAppExp (EqOp, _1, _3), ConstExp (BoolConst false), ConstExp (BoolConst true)))
# 685 "student.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plus_minus) in
    Obj.repr(
# 72 "student.mly"
              (_1)
# 692 "student.ml"
               : 'double_colon))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'double_colon) in
    Obj.repr(
# 73 "student.mly"
                                  (BinOpAppExp (ConsOp, _1, _3))
# 700 "student.ml"
               : 'double_colon))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 76 "student.mly"
             (_1)
# 707 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 77 "student.mly"
                             (BinOpAppExp (IntPlusOp, _1, _3))
# 715 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 78 "student.mly"
                              (BinOpAppExp (IntMinusOp, _1, _3))
# 723 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 79 "student.mly"
                              (BinOpAppExp (FloatPlusOp, _1, _3))
# 731 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 80 "student.mly"
                               (BinOpAppExp (FloatMinusOp, _1, _3))
# 739 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'times_div) in
    Obj.repr(
# 81 "student.mly"
                              (BinOpAppExp (ConcatOp, _1, _3))
# 747 "student.ml"
               : 'plus_minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 84 "student.mly"
              (_1)
# 754 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'times_div) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 85 "student.mly"
                              (BinOpAppExp (IntTimesOp, _1, _3))
# 762 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'times_div) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 86 "student.mly"
                            (BinOpAppExp (IntDivOp, _1, _3))
# 770 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'times_div) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 87 "student.mly"
                               (BinOpAppExp (FloatTimesOp, _1, _3))
# 778 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'times_div) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 88 "student.mly"
                             (BinOpAppExp (FloatDivOp, _1, _3))
# 786 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'times_div) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 89 "student.mly"
                            (BinOpAppExp (ModOp, _1, _3))
# 794 "student.ml"
               : 'times_div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'raise_) in
    Obj.repr(
# 92 "student.mly"
          (_1)
# 801 "student.ml"
               : 'exponetial))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'raise_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exponetial) in
    Obj.repr(
# 93 "student.mly"
                         (BinOpAppExp (ExpoOp, _1, _3))
# 809 "student.ml"
               : 'exponetial))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'application) in
    Obj.repr(
# 96 "student.mly"
               (_1)
# 816 "student.ml"
               : 'raise_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'application) in
    Obj.repr(
# 97 "student.mly"
                     (RaiseExp _2)
# 823 "student.ml"
               : 'raise_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 100 "student.mly"
         (_1)
# 830 "student.ml"
               : 'application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'application) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 101 "student.mly"
                     (AppExp (_1, _2))
# 838 "student.ml"
               : 'application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 104 "student.mly"
                     (_1)
# 845 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 105 "student.mly"
                         (MonOpAppExp (FstOp, _2))
# 852 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 106 "student.mly"
                         (MonOpAppExp (SndOp, _2))
# 859 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 107 "student.mly"
                          (MonOpAppExp (HdOp, _2))
# 866 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 108 "student.mly"
                          (MonOpAppExp (TlOp, _2))
# 873 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 109 "student.mly"
                           (MonOpAppExp (PrintOp, _2))
# 880 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expression) in
    Obj.repr(
# 110 "student.mly"
                         (MonOpAppExp (IntNegOp, _2))
# 887 "student.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "student.mly"
          (ConstExp (IntConst _1))
# 894 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 114 "student.mly"
          (ConstExp (FloatConst _1))
# 901 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "student.mly"
          (ConstExp (UnitConst))
# 907 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "student.mly"
          (ConstExp (BoolConst true))
# 913 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "student.mly"
          (ConstExp (BoolConst false))
# 919 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "student.mly"
          (ConstExp (StringConst _1))
# 926 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "student.mly"
          (ConstExp (NilConst))
# 932 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "student.mly"
             ( VarExp _1 )
# 939 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 121 "student.mly"
                            (_2)
# 946 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 122 "student.mly"
                                             (BinOpAppExp (CommaOp, _2, _4))
# 954 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_expression_left) in
    Obj.repr(
# 123 "student.mly"
                        (_1)
# 961 "student.ml"
               : 'atomic_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_expression_in) in
    Obj.repr(
# 126 "student.mly"
                             (_2)
# 968 "student.ml"
               : 'list_expression_left))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "student.mly"
          (ConstExp NilConst)
# 974 "student.ml"
               : 'list_expression_in))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 130 "student.mly"
                     (BinOpAppExp (ConsOp, _1, ConstExp NilConst))
# 981 "student.ml"
               : 'list_expression_in))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expression_in) in
    Obj.repr(
# 131 "student.mly"
                                       (BinOpAppExp (ConsOp, _1, _3))
# 989 "student.ml"
               : 'list_expression_in))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Common.dec)
