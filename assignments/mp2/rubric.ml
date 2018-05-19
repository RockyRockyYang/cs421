(*
 * grader for mp4
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2011 MP3"

open Grader
open Test
(* open Mp1common *)

(*
 * use a timeout of 4 seconds
 *)

let mptest weight pair = compare (=) 4 weight pair


(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)

let file = "student.ml"

open Check_cps
let is_cps_check _ _ = true
let is_cps_of_check _ _ _ = true

let idk x = x

open Common

(* This list is for regular problems *)
let rubric =
[
  "consk"^" "^"(1, [])"^" "^"(List.map string_of_int)", mptest 0 (ss_pair2 Solution.consk Student.consk ((1, [])) ((List.map string_of_int)));
  "consk"^" "^"(1, [])"^" "^"(fun x -> ())", mptest 0 (ss_pair2 Solution.consk Student.consk ((1, [])) ((fun x -> ())));
  "concatk"^" "^"(\"hello\", \"world\")"^" "^"(fun s -> (s , String.length s))", mptest 0 (ss_pair2 Solution.concatk Student.concatk (("hello", "world")) ((fun s -> (s , String.length s))));
  "concatk"^" "^"(\"hello\", \"world\")"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.concatk Student.concatk (("hello", "world")) ((fun s -> ())));
  "string_of_intk"^" "^"1"^" "^"(fun s -> (s , String.length s))", mptest 0 (ss_pair2 Solution.string_of_intk Student.string_of_intk (1) ((fun s -> (s , String.length s))));
  "string_of_intk"^" "^"1"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.string_of_intk Student.string_of_intk (1) ((fun s -> ())));
  "truncatek"^" "^"3.14"^" "^"string_of_int", mptest 0 (ss_pair2 Solution.truncatek Student.truncatek (3.14) (string_of_int));
  "truncatek"^" "^"3.14"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.truncatek Student.truncatek (3.14) ((fun s -> ())));

  "diff_flipk"^" "^"1"^" "^"report_int", mptest 1 (ss_pair2 Solution.diff_flipk Student.diff_flipk (1) (report_int));
  "diff_flipk"^" "^"1"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.diff_flipk Student.diff_flipk (1) ((fun s -> ())));
  "is_cps_check"^" "^"file"^" "^"\"diff_flipk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("diff_flipk"));

  "quadk"^" "^"(1, 1, 1)"^" "^"report_int", mptest 1 (ss_pair2 Solution.quadk Student.quadk ((1, 1, 1)) (report_int));
  "quadk"^" "^"(1, 1, 1)"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.quadk Student.quadk ((1, 1, 1)) ((fun s -> ())));
  "is_cps_check"^" "^"file"^" "^"\"quadk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("quadk"));

  "three_freezek"^" "^"(\"muda\", \"plop\")"^" "^"(fun s -> (s, String.length s))", mptest 1 (ss_pair2 Solution.three_freezek Student.three_freezek (("muda", "plop")) ((fun s -> (s, String.length s))));
  "three_freezek"^" "^"(\"muda\", \"plop\")"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.three_freezek Student.three_freezek (("muda", "plop")) ((fun s -> ())));
  "is_cps_check"^" "^"file"^" "^"\"three_freezek\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("three_freezek"));

  "shiftk"^" "^"(\"--\", 3.14)"^" "^"(fun s -> (s , String.length s))", mptest 1 (ss_pair2 Solution.shiftk Student.shiftk (("--", 3.14)) ((fun s -> (s , String.length s))));
  "shiftk"^" "^"(\"--\", 3.14)"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.shiftk Student.shiftk (("--", 3.14)) ((fun s -> ())));
  "is_cps_check"^" "^"file"^" "^"\"shiftk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("shiftk"));

  "list_prod"^" "^"[1;2;3]", mptest 1 (ss_pair1 Solution.list_prod Student.list_prod ([1;2;3]));
  "list_prodk"^" "^"[]"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.list_prodk Student.list_prodk ([]) ((fun s -> ())));
  "list_prodk"^" "^"[]"^" "^"(fun s -> (idk s;()))", mptest 0 (ss_pair2 Solution.list_prodk Student.list_prodk ([]) ((fun s -> (idk s;()))));
  "list_prodk"^" "^"[1;2;3]"^" "^"idk", mptest 1 (ss_pair2 Solution.list_prodk Student.list_prodk ([1;2;3]) (idk));
  "is_cps_check"^" "^"file"^" "^"\"list_prodk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("list_prodk"));
  "is_cps_of_check"^" "^"file"^" "^"\"list_prod\""^" "^"\"list_prodk\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("list_prod") ("list_prodk"));

  "all_positive"^" "^"[5;3;6;(-1);7]", mptest 1 (ss_pair1 Solution.all_positive Student.all_positive ([5;3;6;(-1);7]));
  "all_positivek"^" "^"[]"^" "^"(fun b -> ())", mptest 0 (ss_pair2 Solution.all_positivek Student.all_positivek ([]) ((fun b -> ())));
  "all_positivek"^" "^"[]"^" "^"(fun b -> (idk b;()))", mptest 0 (ss_pair2 Solution.all_positivek Student.all_positivek ([]) ((fun b -> (idk b;()))));
  "all_positivek"^" "^"[5;3;6;(-1);7]"^" "^"(fun b -> if b then \"true\" else \"false\")", mptest 1 (ss_pair2 Solution.all_positivek Student.all_positivek ([5;3;6;(-1);7]) ((fun b -> if b then "true" else "false")));
  "is_cps_check"^" "^"file"^" "^"\"all_positivek\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("all_positivek"));
  "is_cps_of_check"^" "^"file"^" "^"\"all_positive\""^" "^"\"all_positivek\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("all_positive") ("all_positivek"));

  "even_count"^" "^"[1;2;3]", mptest 1 (ss_pair1 Solution.even_count Student.even_count ([1;2;3]));
  "even_countk"^" "^"[1;2;3]"^" "^"string_of_int", mptest 1 (ss_pair2 Solution.even_countk Student.even_countk ([1;2;3]) (string_of_int));
  "even_countk"^" "^"[1;2;3]"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.even_countk Student.even_countk ([1;2;3]) ((fun s -> ())));
  "even_countk"^" "^"[1;2;3]"^" "^"(fun s -> (idk(s);()))", mptest 0 (ss_pair2 Solution.even_countk Student.even_countk ([1;2;3]) ((fun s -> (idk(s);()))));
  "is_cps_check"^" "^"file"^" "^"\"even_countk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("even_countk"));
  "is_cps_of_check"^" "^"file"^" "^"\"even_count\""^" "^"\"even_countk\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("even_count") ("even_countk"));

  "find_all"^" "^"((fun x -> x mod 2 = 0), [-3; 5; 2; -6])", mptest 1 (ss_pair1 Solution.find_all Student.find_all (((fun x -> x mod 2 = 0), [-3; 5; 2; -6]))) ;
  "find_allk"^" "^"((fun x -> fun k -> k true), [()])"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.find_allk Student.find_allk (((fun x -> fun k -> k true), [()])) ((fun s -> ())));
  "find_allk"^" "^"((fun x -> fun k -> k true), [()])"^" "^"(fun s -> idk(s);())", mptest 0 (ss_pair2 Solution.find_allk Student.find_allk (((fun x -> fun k -> k true), [()])) ((fun s -> idk(s);())));
  "find_allk"^" "^"((fun x -> fun k -> modk (x, 2) (fun n -> eqk (n, 0) k)), [-3; 5; 2; -6])"^" "^"(fun s -> (idk(List.map string_of_int s)))", mptest 1 (ss_pair2 Solution.find_allk Student.find_allk (((fun x -> fun k -> modk (x, 2) (fun n -> eqk (n, 0) k)), [-3; 5; 2; -6])) ((fun s -> (idk(List.map string_of_int s)))));
  "is_cps_check"^" "^"file"^" "^"\"find_allk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("find_allk"));
  "is_cps_of_check"^" "^"file"^" "^"\"find_all\""^" "^"\"find_allk\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("find_all") ("find_allk"));

  "sum_all"^" "^"((fun x -> truncate x >= 2), [1.3;2.5;3.9])", mptest 1 (ss_pair1 Solution.sum_all Student.sum_all (((fun x -> truncate x >= 2), [1.3;2.5;3.9])));
  "sum_allk"^" "^"((fun x -> fun k -> k true), [])"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.sum_allk Student.sum_allk (((fun x -> fun k -> k true), [])) ((fun s -> ())));
  "sum_allk"^" "^"((fun x -> fun k -> k true), [])"^" "^"(fun s -> idk(s);())", mptest 0 (ss_pair2 Solution.sum_allk Student.sum_allk (((fun x -> fun k -> k true), [])) ((fun s -> idk(s);())));
  "sum_allk"^" "^"((fun x -> fun k -> Solution.truncatek x (fun y -> geqk(y,2) k)), [1.3;2.5;3.9])"^" "^"(fun s -> idk(string_of_float s))", mptest 1 (ss_pair2 Solution.sum_allk Student.sum_allk (((fun x -> fun k -> Solution.truncatek x (fun y -> geqk(y,2) k)), [1.3;2.5;3.9])) ((fun s -> idk(string_of_float s))));
  "is_cps_check"^" "^"file"^" "^"\"sum_allk\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("sum_allk"));
  "is_cps_of_check"^" "^"file"^" "^"\"sum_all\""^" "^"\"sum_allk\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("sum_all") ("sum_allk"))
]
(* Note: the last entry should not be followed by a semicolon. *)

let extra_rubric = [
 "list_compose"^" "^"[(fun x -> x * x) ; (fun x -> x + 2)]", mptest 1 (ss_pair1 Solution.list_compose Student.list_compose ([(fun x -> x * x) ; (fun x -> x + 2)]));
 "list_composek"^" "^"[(fun x -> mulk(x,x)) ; (fun x -> addk(x,2))]"^" "^"string_of_int", mptest 1 (ss_pair2 Solution.list_composek Student.list_composek ([(fun x -> mulk(x,x)) ; (fun x -> addk(x,2))]) (string_of_int));
 "list_composek"^" "^"[(fun x -> mulk(x,x)) ; (fun x -> addk(x,2))]"^" "^"(fun s -> ())", mptest 0 (ss_pair2 Solution.list_composek Student.list_composek ([(fun x -> mulk(x,x)) ; (fun x -> addk(x,2))]) ((fun s -> ())));
 "list_composek"^" "^"[]"^" "^"(fun s -> (idk s; ()))", mptest 0 (ss_pair2 Solution.list_composek Student.list_composek ([]) ((fun s -> (idk s; ()))));
  "is_cps_check"^" "^"file"^" "^"\"list_composek\"", mptest 2 (ss_pair2 is_cps_check check_is_cps (file) ("list_composek"));
  "is_cps_of_check"^" "^"file"^" "^"\"list_compose\""^" "^"\"list_composek\"", mptest 2 (ss_pair3 is_cps_of_check check_is_cps_of (file) ("list_compose") ("list_composek"))
]


let _ = Main.main rubric extra_rubric rubric_title rubric_version
