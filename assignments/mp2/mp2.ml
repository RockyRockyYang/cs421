open Common
(*open List*)

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 1: Warmup (0 Points)  ******)
let consk (x, l) k = k(x::l)
let concatk (s1, s2) k = k(s1^s2)
let string_of_intk s k = k(string_of_int s)
let truncatek r k = k(truncate r)

(***** Problem 2: Basic CPS *****)
let diff_flipk p k = subk(1,p)(fun x-> mulk(x,p)(fun y->mulk(y, 2) k))

(***** Problem 3: Basic CPS *****)
let quadk (a, b, c) k =
  mulk (4, b)
    (fun u -> mulk (a, a)
      (fun v -> mulk (2, v)
        (fun w -> addk (w, u)
          (fun z -> addk (z, c) k))))
(***** Problem 4: Basic CPS *****)
let three_freezek (s, p) k = concatk (s, p) (fun s1 -> concatk (s1, s1) (fun r2 -> concatk (s1, r2)k))

(***** Problem 5: Basic CPS *****)
let shiftk (s, q) k = float_addk(q, 1.57)
						(fun r1-> float_mulk(r1,r1)
							(fun r2->truncatek(r2) 
								(fun r3->string_of_intk(r3) 
									(fun r4->concatk(s,r4) 
										(fun r5-> concatk(r5, s) k)))))

(***** Problem 6a: Recursion & CPS ******)
let rec list_prod l = match l with []->1
								| (x::xs)->let lp = list_prod xs in x*lp

(***** Problem 6b: Recursion & CPS ******)
let rec list_prodk l k = match l with []-> k 1
									| (x::xs)->list_prodk xs(fun lp->mulk(x, lp) k)

(***** Problem 7a: Recursion & CPS *****)
let rec all_positive l = match l with []->true
									| (x::xs)->if x>0 then all_positive xs else false

(***** Problem 7b: Recursion & CPS *****)
let rec all_positivek l k = match l with []->k true
									| (x::xs)->gtk(x, 0)(fun b-> if b then all_positivek xs k else k false)

(***** Problem 8a: Recursion & CPS *****)
let rec even_count l = match l with []->0
								|(x::xs)->let ec = even_count xs in (if x mod 2 = 0 then 1 else 0) + ec

(***** Problem 8b: Recursion & CPS *****)
let rec even_countk l k = match l with []->k 0
									| (x::xs)->even_countk xs 
										(fun ec -> modk(x,2) 
											(fun m-> eqk(m,0) 
												(fun b-> if b then addk(1, ec) k else addk(0, ec) k)))


(******** CONTINUATIONS For HIGHER-ORDER FUNCTIONS ********)


let rec find_all (p,l) = match l with []->[]
								| (x::xs)->let fa = find_all (p, xs) in (if p x then x::fa else fa)

let rec find_allk (p,l) k = match l with []->k []
									| (x::xs)->find_allk (p, xs)
														(fun fa-> p x 
															(fun p-> if p then consk(x,fa) k else k fa))

let rec sum_all (p,l) = match l with []->0.0
								| (x::xs)->let sa = sum_all (p, xs) in (if p x then (x+.sa) else sa)

let rec sum_allk (p,l) k = match l with []-> k 0.0
									| (x::xs)->sum_allk (p, xs) 
														(fun sa-> p x 
															(fun p->if p then float_addk(x,sa) k else k sa))


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs = match fs with []->0
									|(x::xs)->x(list_compose xs)

(* Extra Credit, Problem 16b *)
let rec list_composek fsk k = match fsk with []->k 0
										| (x::xs)->list_composek xs(fun lc->x lc k)
