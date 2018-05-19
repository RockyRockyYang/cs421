(* File: ml3.ml *)

(* one from 1,2,3; three from 4; two from 5; 1 extra credit *)

open Common

(* Problem 1 *)
let rec import_list lst = match lst with []->ConstExp NilConst
									|(m,n)::rest->
										BinOpAppExp(ConsOp, 
											BinOpAppExp(CommaOp, ConstExp (IntConst m), ConstExp (IntConst n)),
											import_list rest) 


(* Problem 2 *)
let pair_sums = 
let ps = VarExp "pair_sums" in
let lst = VarExp "lst" in
let nil = ConstExp NilConst in
let lsteqnil = BinOpAppExp(EqOp, lst, nil) in
let hdlst = MonOpAppExp(HdOp, lst) in
let tllst = MonOpAppExp(TlOp, lst) in
let x = VarExp "x" in
let fstx = MonOpAppExp(FstOp, x) in
let sndx = MonOpAppExp(SndOp, x) in
let fpluss = BinOpAppExp(IntPlusOp, fstx, sndx) in
let app_ps = AppExp(ps, tllst) in
let cons = BinOpAppExp(ConsOp, fpluss, app_ps) in
let letx = LetInExp("x", hdlst, cons) in
let ifexp = IfExp(lsteqnil, nil, letx) in
let final = AppExp(ps, import_list [(7,1);(4,2);(6,3)]) in
LetRecInExp("pair_sums", "lst", ifexp, final)


(* Problem 3 *)
let rec count_const_in_exp exp = 
	match exp with VarExp x->0
	| ConstExp c->1
	| MonOpAppExp (m, e)->count_const_in_exp e
	| BinOpAppExp (bin, e1, e2)-> (count_const_in_exp e1) + (count_const_in_exp e2)
	| IfExp (e1, e2, e3)-> (count_const_in_exp e1) + (count_const_in_exp e2) + (count_const_in_exp e3)
	| AppExp(e1, e2)->(count_const_in_exp e1) + (count_const_in_exp e2)
	| FunExp(str, e)->count_const_in_exp e
	| LetInExp(str, e1, e2)->(count_const_in_exp e1) + (count_const_in_exp e2)
	| LetRecInExp(str1, str2, e1,e2)->(count_const_in_exp e1) + (count_const_in_exp e2)

(* Problem 4 *)
let rec freeVarsInExp exp = 
	match exp with VarExp x->[x]
	| ConstExp c->[]
	| MonOpAppExp(m,e)->freeVarsInExp e
	| BinOpAppExp(b, e1, e2)->(freeVarsInExp e1)@(freeVarsInExp e2)
	| IfExp(e1, e2, e3)->(freeVarsInExp e1)@(freeVarsInExp e2)@(freeVarsInExp e3)
	| AppExp(e1, e2)->(freeVarsInExp e1)@(freeVarsInExp e2)
	| FunExp(str, e)->List.filter (fun y-> not(y=str)) (freeVarsInExp e) (*!!!!filter!!!!!!*)
	| LetInExp(str, e1, e2)->(freeVarsInExp e1)@(List.filter(fun y->not (y=str)) (freeVarsInExp e2))
	| LetRecInExp(str1, str2, e1, e2)->(List.filter(fun y->(not ((y=str1)||(y=str2)))) (freeVarsInExp e1))
										@(List.filter(fun y->not (y=str1)) (freeVarsInExp e2))



(* Problem 5 *)
let rec cps_exp e k =
	(* a *)
	match e with VarExp x-> VarCPS(k,x)
	(* b *)
	| ConstExp x-> ConstCPS(k,x)
	(* c *)
	| IfExp(e1, e2, e3)->
		let v = freshFor (freeVarsInContCPS k @ freeVarsInExp e2 @ freeVarsInExp e3) in
		let e2cps = cps_exp e2 k in
		let e3cps = cps_exp e3 k in
		cps_exp e1 (FnContCPS(v, IfCPS(v, e2cps, e3cps)))
	(* d *)
	| AppExp(e1, e2)->
		let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1) in
		let v1 = freshFor (v2 :: freeVarsInContCPS k) in 			(* !!!!::!!!!! fresh for take str list return str*)
		let e1cps = cps_exp e1 (FnContCPS(v1, AppCPS(k, v1, v2))) in
		cps_exp e2 (FnContCPS(v2, e1cps))
	(* e *)
	| BinOpAppExp(b, e1, e2)->
		let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1) in
		let v1 = freshFor (v2 :: freeVarsInContCPS k) in
		let e1cps = cps_exp e1 (FnContCPS(v1, BinOpAppCPS(k, b, v1, v2))) in  (* !!!!!kuohao *)
		cps_exp e2 (FnContCPS(v2, e1cps))
	(* f *)
	| MonOpAppExp(m, e)->
		let v = freshFor (freeVarsInContCPS k) in
		cps_exp e (FnContCPS(v, MonOpAppCPS(k, m, v)))
	(* gggggggggggggggggg *)
	| FunExp(x, e)->
		let ecps = cps_exp e (ContVarCPS Kvar) in
		FunCPS(k, x, Kvar, ecps)	
	| LetInExp(x, e1, e2)->
		let e2cps = cps_exp e2 k in
		let fx = FnContCPS(x, e2cps) in
		cps_exp e1 fx
	| LetRecInExp(f,x,e1,e2)->
		let e1cps = cps_exp e1 (ContVarCPS Kvar) in
		let e2cps = cps_exp e2 k in
		FixCPS(FnContCPS(f, e2cps), f, x, Kvar, e1cps)


