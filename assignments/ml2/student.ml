(* CS421 - Fall 2016
 * ML2
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

(*************************
 * Patterns of Recursion *
 *************************)

(*********************
 * Forward Recursion *
 *********************)

(* Problem 1 *)
let rec even_count_fr l = match l with []->0
									| (x::xs)->let ec = even_count_fr xs in (if x mod 2 =0 then 1 else 0) + ec
(* Problem 2 *)
let rec pair_sums l = match l with []->[]
								| ((x,y)::rest)->let ps = pair_sums rest in (x+y)::ps

(* Problem 3 *)
let rec remove_even list = match list with []->[]
								| (x::xs)->let re = remove_even xs in (if x mod 2 =1 then x::re else re)

(* Problem 4 *)
let rec sift p l = match l with []->([],[])
							| (x::xs)->let (s1, s2) = sift p xs in (if (p x) then (x::s1, s2) else (s1, x::s2))

(* Problem 5 *)
(* let rec problem5helper l f g idx = match l with []->[]
										| (x::xs)->let p = problem5helper xs f g (idx+1) in 
															if (idx mod 2 =0) then [(f x)]@p else [(g x)]@p *)

let rec apply_even_odd l f g = match l with []->[]
										| [x]-> [f x]
										| (x1::x2::xs)->let ae = apply_even_odd xs f g in (f x1)::(g x2)::ae

(* Problem 6 !!!!!!!!!!!*)

let rec rle lst = match lst with []->[]
							| (x::xs)->let ret = rle xs in match ret with []->[(x,1)]
														|((a,b)::tail)-> if(x=a) then (a,b+1)::tail else (x,1)::(a,b)::tail

(******************
 * Tail Recursion *
 ******************)

(* Problem 7 *)
let rec even_count_tr l = let rec even_count_tr_aux acc_value l = match l with []->acc_value
												| (x::xs)->even_count_tr_aux (if x mod 2=0 then (1+acc_value) else acc_value) xs
										in even_count_tr_aux 0 l

(* Problem 8 *)
let rec count_element l m = let rec count_element_aux acc_value l m  = match l with []->acc_value
										| (x::xs)->count_element_aux (if x=m then 1+acc_value else acc_value) xs m
								in count_element_aux 0 l m 

(* Problem 9 *)
let rec all_nonneg list = let rec all_nonneg_aux acc_boolean list = match list with []->acc_boolean
										| (x::xs)->all_nonneg_aux (acc_boolean && (x >=0)) xs 
								in all_nonneg_aux true list

(* Problem 10 *)
let split_sum l f = let rec split_sum_aux (s1, s2) l f = match l with []->(s1, s2)
									| (x::xs)->split_sum_aux (if f x = true then (x+s1, s2) else (s1, x+s2)) xs f 
							in split_sum_aux (0,0) l f

(* Problem 11 !!!!!!!!!!*)
let rec max_index l = match l with []->[]
							|(x::xs)->let rec max_index_aux ret max lst idx = match lst with []->ret
												| (h::hs)-> if(h > max) then max_index_aux [idx] h hs (idx+1)
																		else (if h = max then max_index_aux (idx::ret) max hs (idx+1)
																				else max_index_aux ret max hs (idx+1))
											in max_index_aux [] x l 0

(* Problem 12 !!!!!!!!!!!*)
let rec concat s list = let rec concat_aux s lst str = match lst with []->str
									| [x]->(str^x)
									| (x::y::xs)-> concat_aux s (y::xs) (str^x^s)   
								in concat_aux s list ""


(**************************
 * Higher Order Functions *
 **************************)

(* Problem 13 *)
let even_count_fr_base = 0 (* You may need to change this *)
let even_count_fr_rec x rec_val = if(x mod 2 =0) then rec_val+1 else rec_val

(* Problem 14 *)
let pair_sums_map_arg (a,b) = a+b

(* Problem 15 *)
let remove_even_base = [] (* You may need to change this *)
let remove_even_rec n r = if(n mod 2 = 0) then r else n::r

(* Problem 16 *)
let sift_base = ([],[]) (* You may need to change this *)
let sift_rec p x (tl, fl) = if (p x = true) then (x::tl, fl) else (tl, x::fl)

(* Problem 17 *)
let even_count_tr_start = 0
let even_count_tr_step x rec_val = if(rec_val mod 2 = 0) then 1+x else x

(* Problem 18 *)
let count_element_start = 0
let count_element_step m counter n = if(m=n) then counter+1 else counter

(* Problem 19 *)
let all_nonneg_start = true;; 
let all_nonneg_step r x = if (x>=0) then r else false

(* Problem 20 *)
let split_sum_start = (0,0) (* You may need to change this *)
let split_sum_step f (s1, s2) x = if(f x = true) then (s1+x, s2) else (s1, s2+x) 

(* Problem 21 !!!!!!!!!!!!!!!!!*)
let app_all_with fs b l = List.map (fun f -> List.map (fun x -> f b x) l) fs

(* Problem 22 *)
let exists_between_start = false
let exists_between_step m n b x = b || (m <= x && n >=x)

(* Problem 23 !!!!!!!!!!!!!!!!!!*)
let rev_append_base l = l
let rev_append_rec x rev_base l = rev_base (x::l) 
