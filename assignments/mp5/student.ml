open Common;;

let const_to_val c = 
	match c with
		| BoolConst b -> BoolVal b
		| IntConst i -> IntVal i
		| FloatConst f -> FloatVal f
		| StringConst s -> StringVal s
		| NilConst -> ListVal []
		| UnitConst -> UnitVal


let monOpApply op v = 
	match op with
		| HdOp -> 
				(match v with 
					| ListVal []-> raise (Failure "Not implemented yet.")
					| ListVal (x::xs) -> x)
		| TlOp -> 
				(match v with 
					| ListVal []-> raise (Failure "Not implemented yet.")
					| ListVal (x::xs) -> ListVal xs)
		| PrintOp -> 
				(match v with 
					| StringVal s -> (print_string s; UnitVal))

		| IntNegOp ->
				(match v with 
					| IntVal i -> IntVal (-i))
		| FstOp -> 
				(match v with 
					| PairVal (v1,v2) -> v1)
		| SndOp -> 
				(match v with 
					| PairVal (v1,v2) -> v2)

let binOpApply binop (v1,v2) = 
	match binop,v1,v2 with 
		| IntPlusOp, IntVal a, IntVal b -> IntVal (a+b)
		| IntMinusOp, IntVal a, IntVal b -> IntVal (a - b)
	   	| IntTimesOp, IntVal a, IntVal b -> IntVal (a * b)
	   	| IntDivOp, IntVal a, IntVal b -> IntVal (a / b)
	   	| ModOp, IntVal a, IntVal b  -> IntVal (a mod b)
	  	| FloatPlusOp, FloatVal a, FloatVal b -> FloatVal (a +. b)
	  	| FloatMinusOp, FloatVal a, FloatVal b -> FloatVal (a -. b)
	   	| FloatTimesOp, FloatVal a, FloatVal b -> FloatVal (a *. b)
		| FloatDivOp, FloatVal a, FloatVal b -> FloatVal (a /. b)
		| ExpoOp, FloatVal a, FloatVal b -> FloatVal (a ** b)
		| ConcatOp, StringVal a, StringVal b -> StringVal (a ^ b)
	   	| ConsOp, _, ListVal l -> ListVal (v1 :: l)
	   	| CommaOp, _, _ -> PairVal (v1, v2)
	   	| EqOp, _, _ -> BoolVal (v1 = v2)
	   	| GreaterOp, _, _ -> BoolVal (v1 > v2)

let rec eval_exp (exp, m) = 
	match exp with
		| ConstExp c-> const_to_val c
		| VarExp v->
			(match lookup_mem m v with
				| RecVarVal (g, y, e, m') -> 
					Closure(y, e, ins_mem m' g (RecVarVal(g, y, e, m'))) 
				| v -> v)
		| MonOpAppExp (mon_op, e) -> monOpApply mon_op (eval_exp (e, m))
		| BinOpAppExp (bin_op, e1, e2) -> binOpApply bin_op ((eval_exp (e1, m)), (eval_exp (e2, m)))
		| IfExp (e1,e2,e3) -> 
			(match eval_exp(e1, m) with 
				| BoolVal true -> eval_exp(e2, m)
				| BoolVal false -> eval_exp(e3, m))
		| LetInExp(x,e1,e2) -> 
			let v1  = eval_exp(e1, m) in 
				eval_exp(e2, ins_mem m x v1)
		| FunExp(x,e) -> Closure (x, e, m)
		| AppExp(e1,e2) -> 
			(match eval_exp (e1, m) with 
				| Closure (x, e', m') -> 
					let v' = eval_exp (e2, m) in
						eval_exp (e', (ins_mem m' x v')))
		| LetRecInExp (f, x, e1, e2) -> 
			eval_exp (e2, ins_mem m f (RecVarVal(f, x, e1, m)))
 
let eval_dec (dec, m) = 
	match dec with
		| Anon e-> ((None, eval_exp(e, m)), m)
		| Let (x,e)-> let v = eval_exp(e, m) in
						((Some x, v), ins_mem m x v)  
		| LetRec (f, x, e) -> 
   			((Some f, (RecVarVal(f, x, e, m))), (ins_mem m f (RecVarVal(f, x, e, m))))