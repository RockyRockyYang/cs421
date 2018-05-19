(*
 * grader for mp1
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2017 ML1"

open Grader
open Test
open Common

(*
 * use a timeout of 4 seconds
 *)
let outputOk () =
  let n = !last_string_length in
  if n < 0 then true else
  try (
  let len = String.length !output
  in let half1 = String.sub !output 0 (len - n)
  and half2 = String.sub !output (len - n) n
     in half1=half2
  ) with e -> false

(* let len = List.length !output
  in if (len mod 2) != 0 then false else
    let rec aux slist half1 half2 idx =
      if idx = (len / 2) then half1 = half2
      else match slist with
           | x::y -> aux y (half1@[x]) y (idx + 1)
    in aux !output [] [] 0
*)


let isEq i j =
    (i = j) &&
    (let res = outputOk() in output := ""; last_string_length := -1; res)

let mp1test weight pair = compare isEq 4 weight pair


(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)

(* This list is for regular problems *)
let rubric =
[
(* 1 *)
     "title", mp1test 1 (ss_pair0 Solution.title Student.title) ;
     "greetings", mp1test 1 (ss_pair0 Solution.greetings Student.greetings) ;
     "address", mp1test 1 (ss_pair0 Solution.address Student.address) ;
     "frozen", mp1test 1 (ss_pair0 Solution.frozen Student.frozen) ;
     "daffy", mp1test 1 (ss_pair0 Solution.daffy Student.daffy) ;
(* 2 *)
     "a", mp1test 1 (ss_pair0 Solution.a Student.a) ;
     "pi", mp1test 1 (ss_pair0 Solution.pi Student.pi) ;
     "e", mp1test 1 (ss_pair0 Solution.e Student.e) ;
     "quarter", mp1test 1 (ss_pair0 Solution.quarter Student.quarter) ;
     "x", mp1test 1 (ss_pair0 Solution.x Student.x) ;
(* 3 *)
     "myFirstFun"^" "^"17", mp1test 1 (ss_pair1 Solution.myFirstFun Student.myFirstFun 17) ;
     "myFirstFun"^" "^"(-1)", mp1test 1 (ss_pair1 Solution.myFirstFun Student.myFirstFun (-1)) ;
     "firstFun"^" "^"12", mp1test 1 (ss_pair1 Solution.firstFun Student.firstFun 12) ;
     "firstFun"^" "^"76", mp1test 1 (ss_pair1 Solution.firstFun Student.firstFun 76);
     "square"^" "^"7", mp1test 1 (ss_pair1 Solution.square Student.square 7) ;
     "square"^" "^"(-1)", mp1test 1 (ss_pair1 Solution.square Student.square (-1)) ;
     "times_13"^" "^"7", mp1test 1 (ss_pair1 Solution.times_13 Student.times_13 7) ;
     "times_13"^" "^"(-2)", mp1test 1 (ss_pair1 Solution.times_13 Student.times_13 (-2)) ;
     "cube"^" "^"5", mp1test 1 (ss_pair1 Solution.cube Student.cube 5);
     "cube"^" "^"(-3)", mp1test 1 (ss_pair1 Solution.cube Student.cube (-3));
(* 4 *)
     "add_a"^" "^"13.5", mp1test 1 (ss_pair1 Solution.add_a Student.add_a 13.5) ;
     "add_a"^" "^"(-17.39)", mp1test 1 (ss_pair1 Solution.add_a Student.add_a (-17.39)) ;
     "circumference"^" "^"1.0", mp1test 1 (ss_pair1 Solution.circumference Student.circumference 1.0) ;
     "circumference"^" "^"0.1", mp1test 1 (ss_pair1 Solution.circumference Student.circumference 0.1) ;
     "divide_e_by"^" "^"Solution.e", mp1test 1 (ss_pair1 Solution.divide_e_by Student.divide_e_by Solution.e) ;
     "divide_e_by"^" "^"1.5", mp1test 1 (ss_pair1 Solution.divide_e_by Student.divide_e_by 1.5);
     "plus_quarter_times_3"^" "^"23.5", mp1test 1 (ss_pair1 Solution.plus_quarter_times_3 Student.plus_quarter_times_3 23.5) ;
     "plus_quarter_times_3"^" "^"(-1.0)", mp1test 1 (ss_pair1 Solution.plus_quarter_times_3 Student.plus_quarter_times_3 (-1.0));
     "square_plus_x"^" "^"23.17", mp1test 1 (ss_pair1 Solution.square_plus_x Student.square_plus_x 23.17) ;
     "square_plus_x"^" "^"0.0", mp1test 1 (ss_pair1 Solution.square_plus_x Student.square_plus_x 0.0) ;
(* 5 *)
     "salutations"^" "^"\"Malisa\"", mp1test 1 (ss_pair1 Solution.salutations Student.salutations "Malisa") ;
     "salutations"^" "^"\"Elsa\"", mp1test 1 (ss_pair1 Solution.salutations Student.salutations "Elsa") ;
     "salutations"^" "^"\"\"", mp1test 1 (ss_pair1 Solution.salutations Student.salutations "") ;
     "hail"^" "^"\"Thomas\"", mp1test 1 (ss_pair1 Solution.hail Student.hail "Thomas") ;
     "hail"^" "^"\"Elsa\"", mp1test 1 (ss_pair1 Solution.hail Student.hail "Elsa") ;
     "hail"^" "^"\"\"", mp1test 1 (ss_pair1 Solution.hail Student.hail "") ;
     "welcome"^" "^"\"John\"", mp1test 1 (ss_pair1 Solution.welcome Student.welcome "John") ;
     "welcome"^" "^"\"Elsa\"", mp1test 1 (ss_pair1 Solution.welcome Student.welcome "Elsa") ;
     "welcome"^" "^"\"\"", mp1test 1 (ss_pair1 Solution.welcome Student.welcome "") ;
     "greet"^" "^"\"Angela\"", mp1test 1 (ss_pair1 Solution.greet Student.greet "Angela") ;
     "greet"^" "^"\"Elsa\"", mp1test 1 (ss_pair1 Solution.greet Student.greet "Elsa") ;
     "greet"^" "^"\"\"", mp1test 1 (ss_pair1 Solution.greet Student.greet "") ;
     "salute"^" "^"\"Ali\"", mp1test 1 (ss_pair1 Solution.salute Student.salute "Ali") ;
     "salute"^" "^"\"Elsa\"", mp1test 1 (ss_pair1 Solution.salute Student.salute "Elsa") ;
     "salute"^" "^"\"\"", mp1test 1 (ss_pair1 Solution.salute Student.salute "") ;
(* 6 *)
     "rectangle_area"^" "^"25.3"^" "^"19.2", mp1test 1 (ss_pair2 Solution.rectangle_area Student.rectangle_area 25.3 19.2);
     "rectangle_area"^" "^"(-1.5)"^" "^"2.7", mp1test 1 (ss_pair2 Solution.rectangle_area Student.rectangle_area (-1.5) 2.7);
     "rectangle_area"^" "^"(-1.0)"^" "^"(-2.0)", mp1test 1 (ss_pair2 Solution.rectangle_area Student.rectangle_area (-1.0) (-2.0));
     "diff_square_9"^" "^"5.5"^" "^"(-17.2)", mp1test 1 (ss_pair2 Solution.diff_square_9 Student.diff_square_9 5.5 (-17.2)) ;
     "diff_square_9"^" "^"(-5.2)"^" "^"12.0", mp1test 1 (ss_pair2 Solution.diff_square_9 Student.diff_square_9 (-5.2) 12.0);
     "diff_square_9"^" "^"(-4.0)"^" "^"(-5.0)", mp1test 1 (ss_pair2 Solution.diff_square_9 Student.diff_square_9 (-4.0) (-5.0));
     "make_bigger"^" "^"(15.2)"^" "^"12.0", mp1test 1 (ss_pair2 Solution.make_bigger Student.make_bigger (15.2) 12.0);
     "make_bigger"^" "^"(-3.1)"^" "^"(-1.5)", mp1test 1 (ss_pair2 Solution.make_bigger Student.make_bigger (-3.1) (-1.5));
     "make_bigger"^" "^"0.0"^" "^"(2.0)", mp1test 1 (ss_pair2 Solution.make_bigger Student.make_bigger 0.0 (2.0));
     "has_smallest_square"^" "^"4"^" "^"6", mp1test 1 (ss_pair2 Solution.has_smallest_square Student.has_smallest_square 4 6) ;
     "has_smallest_square"^" "^"1"^" "^"(~-1)", mp1test 1 (ss_pair2 Solution.has_smallest_square Student.has_smallest_square 1 (~-1)) ;
     "has_smallest_square"^" "^"(~-2)"^" "^"(~-1)", mp1test 1 (ss_pair2 Solution.has_smallest_square Student.has_smallest_square (~-2) (~-1)) ;
     "sign_times"^" "^"4"^" "^"3", mp1test 1 (ss_pair2 Solution.sign_times Student.sign_times 4 3) ;
     "sign_times"^" "^"0"^" "^"0", mp1test 1 (ss_pair2 Solution.sign_times Student.sign_times 0 0) ;
     "sign_times"^" "^"(-1)"^" "^"4", mp1test 1 (ss_pair2 Solution.sign_times Student.sign_times (-1) 4)
]
(* Note: the last entry should not be followed by a semicolon. *)

(* This is the list for extra credit problems *)
let extra_rubric = [ ]

let _ = Main.main rubric extra_rubric rubric_title rubric_version
