(* CS421 - Fall 2016
 * ML1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Common


let title = "ML 1 -- Basic OCaml" (* You want to change this *)

let greetings = "Hi there." (* You want to change this *)

let address = "Greetings, my friend!" (* You want to change this *)

let frozen = "Do you want to build a snowman?"  (* You want to change this *)

let daffy = "Th, th, that's all, Folks!"  (* You want to change this *)

let a = 17.5 (* You want to change this *)

let pi = 3.14159 (* You want to change this *)

let e = 2.71828 (* You want to change this *)

let quarter = 0.25 (* You want to change this *)

let x = 32.7 (* You want to change this *)

let myFirstFun n = (n+3)*4

let firstFun n = n*2+5

let square n = n*n

let times_13 n = n*13

let cube n = n*n*n

let add_a n = n+.a

let circumference r = 2.0*.r*.pi

let divide_e_by x = e/.x

let plus_quarter_times_3 y = (quarter+.y)*.3.0

let square_plus_x y = y*.y +. x

let salutations name = if (name="Elsa") then (print_string "Halt! Who goes there!\n") else (print_string ("Hail, " ^ name ^ ". We warmly welcome you!\n");)

let hail name = if(name = "Elsa") then (print_string "Wayell, hah theya, Ayelsa!") else (print_string ("Dear, "^name^". I wish you the best in CS421.\n"))

let welcome name = if(name = "Elsa") then (print_string "Can you come out to play?\n") else (print_string ("Aw, come on, " ^name^". We're going to have a wonderful time!\n"))

let greet name = if(name = "Elsa") then (print_string "Hey Elsa, cool man!") else (print_string ("Hello, "^name^". I hope you enjoy CS421.\n"))

let salute name = if(name = "Elsa") then (print_string "What's the low-down, man?") else (print_string("Hey, "^name^"! Give me five, man."))  

let rectangle_area l w =  if(l >=0.0 && w >=0.0) then l*.w else -1.0

let diff_square_9 m n = if m<n then n*.n-.9.0 else (if (m/.2.0 > n) then m*.m-.9.0 else  (m-.n)*.(m-.n)-.9.0)

let make_bigger x y = if x > 0.0 then x+.y else (if (y < 1.0) then 1.0+.y else y*.y)

let has_smallest_square m n = if m*m <> n*n then (if m*m < n*n then m else n) else (if m<n then m else n) 

let sign_times n m = if n*m>0 then 1 else (if n*m=0 then 0 else -1)

