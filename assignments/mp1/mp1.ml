(* CS421 - Fall 2016
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let closer_to_origin (a1, a2) (b1, b2) = if (a1*.a1+.a2*.a2)<(b1*.b1+.b2*.b2) then -1 else 
    (if (a1*.a1+.a2*.a2)=(b1*.b1+.b2*.b2) then 0 else 1)

(*Problem 2*)
let swap_eq (a1, a2) (b1, b2) = if (a1=b2)&&(a2=b1) then true else false

(*Problem 3*)
let two_funs (fun1, fun2) (in1, in2) = let m = fun1 in1 in
											let n = fun2 in2 in
												(m,n);;

(*Problem 4*)
let rec ackermann m n = if m=0 then n+1 else if m>0 && n=0 then ackermann (m-1) 1 else ackermann (m-1) (ackermann m (n-1))
(*Problem 5*)
let pro5helper n = if (n mod 2 =0) then n/2 else 3*n+1

let rec collatz n = match n with 1->0
								| n-> 1+collatz(pro5helper n)

(*Problem 6*)
let rec delannoy (m, n) = match (m, n) with (0,0)->1
											|(m,0)->1
											|(0,n)->1
											|(m,n)->delannoy(m-1, n)+delannoy(m, n-1)+delannoy(m-1, n-1)

(*Problem 7*)
let rec product l = match l with []->1.0
								| (x::xs)->x*.(product xs)

(*Problem 8*)
let rec double_all l = match l with []->[]
									| (x::xs)->((2.0*.x):: double_all xs)

(*Problem 9*)
let rec upto n = if n<0 then [] else (upto (n-1) @ [n])

(*Problem 10*)
let rec pro10helper f n = if(f n = false && n < 100) then [n]@(pro10helper f (n+1)) else [] 

let rec upuntil f = pro10helper f 0

(*Problem 11*)
let rec pair_with_all x l = match l with []->[]
										| (h::hs)->[(x,h)] @ (pair_with_all x hs)

(*Problem 12*)

let rec insert_by comp x l = match l with []->[]@[x]
										| (h::hs)-> if ((comp h x)<>1) then [h]@(insert_by comp x hs) 
														else [x]@l 

(*Problem 13*)
let rec sub_list l1 l2 = match l1 with []->(match l2 with []->true
														| (h::hs)->false)
									| (x::xs)->(match l2 with []->true
															| (h::hs)-> if (x=h) then (sub_list xs hs)
																			else (sub_list xs l2))  

(*Extra Credit - Problem 14*)
let rec pro14helper lst (x,y) = match lst with []->[(x,y)]
											| (h::xs)->let (a,b)=h in (if a=x then (pro14helper xs (a,y@[b])) 
																else [(x,y)]@(pro14helper xs (a,[b]))) 

let rec collect_adjacent l = match l with []->[]
										| (x::xs)-> let (a,b)=x in pro14helper xs (a,[b])


