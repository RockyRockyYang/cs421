(* File: mp6.ml *)

open Mp6common;;


let rec lookup_handler l n = 
	match l with 
		| [] -> None
		| (Some k, v)::xs -> if n = k then Some v else lookup_handler xs n
		| (None, v)::xs -> Some v

let rec app_exn_handler_help env k a = 
	match k with 
		| ExnContVarCPS b -> 
			(match lookup_exn_cont env b with
				| None -> Failed
				| Some (k', env') -> app_exn_handler_help env' k' a)
		| EmptyExnContCPS -> UncaughtException a
		| UpdateExnContCPS (l, k') -> 
			(match lookup_handler l a with
				| None -> app_exn_handler_help env k' a 
				| Some v -> Intermediate (env, v))


let rec app_cont_to_value env k v = 
	match k with
		| External -> Final v 
		| ContVarCPS k ->
			(match lookup_cont env k with 
				| None -> Failed
				| Some (k', env') -> app_cont_to_value env' k' v)
		| FnContCPS (str, exp_cps) ->
			Intermediate(ValueBinding(str,v)::env, exp_cps)
		| ExnMatch k ->
			(match v with
				| IntVal a -> app_exn_handler_help env k a
				| _ -> Failed)



let rec one_step_exp_cps_eval env exp_cps = 
	match exp_cps with
		| ConstCPS (k, c) -> 
			app_cont_to_value env k (const_to_val c)
		| VarCPS (k, x) -> 
			(match lookup_value env x with
				| None -> Failed
				| Some v -> app_cont_to_value env k v)
		| MonOpAppCPS (k, mon_op, x, ke) ->
			(match lookup_value env x with 
				| None -> Failed
				| Some v -> 
					(match monOpApply mon_op v with 
						| Exn n -> app_exn_handler_help env ke n
						| Value v' -> app_cont_to_value env k v'))
		| BinOpAppCPS(k, bin_op, x, y, ke) -> 
			(match lookup_value env x with 
				| None -> Failed
				| Some v1 -> 
					(match lookup_value env y with
						| None -> Failed
						| Some v2 -> 
							(match binOpApply bin_op v1 v2 with
								| Exn n -> app_exn_handler_help env ke n
								| Value v' -> app_cont_to_value env k v')))
		| IfCPS(b, e1, e2) ->
			(match lookup_value env b with 
				| None -> Failed
				| Some (BoolVal true) -> Intermediate (env, e1)
				| Some (BoolVal false) -> Intermediate (env, e2))
		| FunCPS (k, x, ek, e, p) -> 
			app_cont_to_value env k (CPSClosureVal (x, ek, e, p, env))
		| FixCPS (k, f, x, ek, e, p) -> 
			 app_cont_to_value env k (CPSRecClosureVal(f, x, ek, e, p, env))
		| AppCPS (k, f, x, ke) ->
			(match lookup_value env x with
				| None -> Failed
				| Some v -> 
					(match lookup_value env f with 
						| None -> Failed
						| Some (CPSClosureVal (y, kk, ek, e, env')) -> 
							Intermediate ((ValueBinding(y, v) :: ContBinding(kk, (k, env)) :: ExnContBinding(ek, (ke, env)) :: env'), e)
					 	| Some (CPSRecClosureVal(g, y, kk, ek, e, env')) -> 
							Intermediate ((ValueBinding(y, v) :: ValueBinding(g, CPSRecClosureVal(g, y, kk, ek, e, env')) :: ContBinding(kk, (k, env)) :: ExnContBinding(ek, (ke, env)) :: env'), e)
					 		))

