(*
 * File: ml4.ml
 *)

open Common

(* Problem 1*)
let asMonoTy1 () = mk_fun_ty bool_ty (mk_list_ty int_ty)
let asMonoTy2 () = mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (fresh())))
let asMonoTy3 () = mk_fun_ty (fresh()) (mk_list_ty (mk_pair_ty (fresh()) int_ty))
let asMonoTy4 () = mk_pair_ty string_ty (mk_fun_ty (mk_list_ty (fresh())) (fresh()))

(* Problem 2*)
let rec subst_fun subst m = match subst with [] -> TyVar m
										|(n, n_ty)::rest -> if n = m then n_ty else subst_fun rest m

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = match monoTy with TyVar m -> subst_fun subst m
						|TyConst (str, typelist)-> TyConst(str, List.map(monoTy_lift_subst subst) typelist)

(* Problem 4*)
let rec occurs x ty = match ty with TyVar m -> x = m
							|TyConst (str, typelist)-> List.exists(occurs x) typelist

(* Problem 5*)
(* let rec unify eqlst = 	let typeEquals lst1 lst2 acc = 
									match lst1 lst2 with [] []-> Some acc
											| (t::tl), (t'::tl') -> typeEquals tl tl' ((t,t')::acc)
											| _ -> None
						in
						match eqlst with []->Some([])
							|(s,t)::eqs -> 
							(* delete *)
							if s=t then unify eqs else 
							(match (s,t) with
								(* decompose *) 
								(TyConst (f tl), TyConst (f' tl')) ->
								if f = f' then (match (typeEquals tl tl' eqs) with None->None
																			| Some l -> unify l)
								else None
								(* oriented *)
								| (TyConst (f tl), TyVar (m)) -> unify ((TyVar (m), TyConst (f tl))::eqs)
								(* eliminate *)
								| (TyVar (x), t) -> if (occurs x t) then None 
								else let eqs' = List.map (fun (t1,t2)-> ((monoTy_lift_subst [(x,t)] t1), (monoTy_lift_subst [(x,t)] t2))) eqs 
								in
								(match unify eqs' with None -> None
												| Some phi -> Some ((x, monoTy_lift_subst phi t)::phi))) *)
let rec unify eqlst =
  match eqlst with
    [] -> Some([])
  | (s,t)::eqs ->
    (* Delete *)
    if s = t then unify eqs
    else (
    	match s with TyConst(c, args)->
    		(match t with TyConst(c', args')->
    			if(c = c') then 
    				let rec newEqs l1 l2 acc = 
    					match l1,l2 with
            				[],[] -> Some acc
          					| t::tl, t'::tl' -> newEqs tl tl' ((t,t')::acc)
          					| _ -> None
    				in
    					(match newEqs args args' eqs with None -> None | Some l -> unify l)
    			else None
    			| TyVar m->
    				unify((TyVar m, TyConst(c, args))::eqs)
    		)
    		| TyVar n->
    			if occurs n t then None else
    			let eqs' = List.map(fun(t1,t2)-> (monoTy_lift_subst [n,t] t1, monoTy_lift_subst [n,t] t2)) eqs
    			in 
    			(match unify eqs' 
    					with None->None
    					| Some phi -> Some((n,monoTy_lift_subst phi t)::phi)
    			) 
    )

(* Extra Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
