(* File: common.ml *)

(* expressions for PicoML *)

type const =
     BoolConst of bool        (* for true and false *)
   | IntConst of int          (* 0,1,2, ... *)
   | FloatConst of float      (* 2.1, 3.0, 5.975, ... *)
   | StringConst of string    (* "a", "hi there", ... *)
   | NilConst                 (* [ ] *)
   | UnitConst                (* ( ) *)

let string_of_const = function
   BoolConst b     -> (if b then "true" else "false")
 | IntConst i      -> string_of_int i
 | FloatConst f     -> ((string_of_float f)^(if ceil f = floor f then ("0") else ("")))
 | StringConst s   -> ("\""^ (String.escaped s)^ "\"")
 | NilConst        -> "[]"
 | UnitConst       -> "()"

type mon_op =
     IntNegOp      (* integer negation *)
   | HdOp          (* hd *)
   | TlOp          (* tl *)
   | FstOp         (* fst *)
   | SndOp         (* snd *)

let string_of_mon_op = function
     IntNegOp -> "~"
   | HdOp -> "hd"
   | TlOp -> "tl"
   | FstOp -> "fst"
   | SndOp -> "snd"

type bin_op =
     IntPlusOp        (* _ + _ *)
   | IntMinusOp       (* _ - _ *)
   | IntTimesOp       (* _ * _ *)
   | IntDivOp         (* _ / _ *)
   | FloatPlusOp      (* _ +. _ *)
   | FloatMinusOp     (* _ -. _ *)
   | FloatTimesOp     (* _ *. _ *)
   | FloatDivOp       (* _ /. _ *)
   | ConcatOp         (* _ ^ _ *)
   | ConsOp           (* _ :: _ *)
   | CommaOp          (* _ , _ *)
   | EqOp             (* _ = _ *)
   | GreaterOp        (* _ > _ *)

let string_of_bin_op = function
     IntPlusOp  -> "+"
   | IntMinusOp -> "-"
   | IntTimesOp -> "*"
   | IntDivOp -> "/"
   | FloatPlusOp -> "+."
   | FloatMinusOp -> "-."
   | FloatTimesOp -> "*."
   | FloatDivOp -> "/."
   | ConcatOp -> "^"
   | ConsOp -> "::"
   | CommaOp -> ","
   | EqOp  -> "="
   | GreaterOp -> ">"

type exp =  (* Exceptions will be added in later MPs *)
   | VarExp of string                    (* variables *)
   | ConstExp of const                   (* constants *)
   | MonOpAppExp of mon_op * exp         (* % e1 - % is a builtin monadic operator *) 
   | BinOpAppExp of bin_op * exp * exp   (* e1 % e2 - % is a builtin binary operator *)
   | IfExp of exp * exp * exp            (* if e1 then e2 else e3 *)
   | AppExp of exp * exp                 (* e1 e2 *) 
   | FunExp of string * exp              (* fun x -> e1 *)
   | LetInExp of string * exp * exp      (* let x = e1 in e2 *)
   | LetRecInExp of string * string * exp * exp (* let rec f x = e1 in e2 *)
   | RaiseExp of exp                            (* raise e *)
   | TryWithExp of (exp * int option * exp * (int option * exp) list)
		        (* try e with i -> e1 | j -> e1 | ... | k -> en *)

type dec =
     Let of string * exp                 (* let x = exp *)
   | LetRec of string * string * exp     (* let rec f x = exp *)

let rec string_of_exp = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp e1) ^
                 " then " ^ (string_of_exp e2) ^
                 " else " ^ (string_of_exp e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b with CommaOp -> ("(" ^ (paren_string_of_exp e1) ^ (string_of_bin_op b) ^
                              (paren_string_of_exp e2) ^ ")")
    | _ -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b)
            ^ " " ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FunExp (x,e) ->  ("fun " ^ x ^ " -> " ^ (string_of_exp e))
 | LetInExp (x,e1,e2) -> ("let "^x^" = "^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | LetRecInExp (f,x,e1,e2) -> 
    ("let rec "^f^" "^x^" = "^(string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | RaiseExp e -> "raise " ^ (string_of_exp e)
 | TryWithExp (e,intopt1,exp1,match_list) ->
    "try " ^ (paren_string_of_exp e) ^  " with " ^
     (string_of_exc_match (intopt1,exp1)) ^
     (List.fold_left (fun s m -> (s^" | " ^ (string_of_exc_match m))) "" match_list) 

and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e
 

and string_of_exc_match (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^
    " -> " ^
    (string_of_exp e)
			
				
let string_of_dec = function
   Let (s, e) ->  ("let "^ s ^" = " ^ (string_of_exp e))
 | LetRec (fname,argname,fn) -> 
    ("let rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp fn))


let print_exp exp = print_string (string_of_exp exp) 
let print_dec dec = print_string (string_of_dec dec)


(*FIXME: type system*)

type typeVar = int

let rec expand n (list,len) =
    let q = n / 26 in
        if q = 0 then (n :: list, len + 1)
        else expand q (((n mod 26)::list), len + 1);;


let string_of_typeVar n = 
   let (num_list,len) =
       match (expand n ([],0))
       with ([],l) -> ([],l) (* can't actually happen *)
          | ([s],l) -> ([s],l)
          | (x::xs,l) -> ((x - 1) :: xs, l)
   in
   let s = (String.create len)
   in
   let _ =
    List.fold_left
    (fun n c -> (String.set s n c; n + 1))
    0
    (List.map (fun x -> Char.chr(x + 97)) num_list)  (* Char.code 'a' = 97 *)
   in "'"^s;;

type monoTy = TyVar of typeVar | TyConst of (string * monoTy list)

let rec string_of_monoTy t =
  let rec string_of_tylist = function
     []     -> ""
   | t'::[] -> string_of_monoTy t'
   | t'::ts -> string_of_monoTy t'^ ","^ string_of_tylist ts
  in
  let string_of_subty s =
  match s with 
     TyConst ("*", _) | TyConst ("->", _) -> ("("^ string_of_monoTy s^ ")")
   | _ ->  string_of_monoTy s
  in 
    match t with
       TyVar n         -> (string_of_typeVar n)
     |TyConst (name, []) -> name
     |TyConst (name, [ty]) -> (string_of_subty ty^ " "^ name)
     |TyConst ("*", [ty1; ty2]) -> (string_of_subty ty1^ " * "^ string_of_monoTy ty2)
     |TyConst ("->", [ty1; ty2]) -> (string_of_subty ty1^ " -> "^ string_of_monoTy ty2)
     |TyConst (name, tys) -> ("("^ string_of_tylist tys^ ") "^ name)


let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = accummulate_freeVarsMonoTy [] ty

(*fresh type variable*)
let (fresh, reset) =
   let nxt = ref 0 in
   let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
   let r () = nxt := 0 in
    (f, r)

let bool_ty = TyConst("bool",[])
let int_ty = TyConst ("int", [])
let float_ty = TyConst ("float",[])
let string_ty = TyConst ("string",[])
let unit_ty = TyConst("unit", [])
let mk_pair_ty ty1 ty2 = TyConst("*",[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst("->",[ty1;ty2])
let mk_list_ty ty = TyConst("list",[ty])

type polyTy = typeVar list * monoTy  (* the list is for quantified variables *)

let string_of_polyTy (bndVars, t) = match bndVars with [] -> string_of_monoTy t
    | _ ->  (List.fold_left
             (fun s v -> s ^ " " ^ string_of_typeVar v)
             "Forall"
             bndVars)
             ^ ". " ^ string_of_monoTy t

let freeVarsPolyTy ((tvs, ty):polyTy) =
    List.filter (fun x -> not(List.mem x tvs)) (freeVarsMonoTy ty)

let polyTy_of_monoTy mty = (([],mty):polyTy)

let int_op_ty = polyTy_of_monoTy(mk_fun_ty int_ty (mk_fun_ty int_ty int_ty))
let float_op_ty =
    polyTy_of_monoTy(mk_fun_ty float_ty (mk_fun_ty float_ty float_ty))
let string_op_ty =
    polyTy_of_monoTy(mk_fun_ty string_ty (mk_fun_ty string_ty string_ty))

(* fixed signatures *)
let const_signature const = match const with
   BoolConst b -> polyTy_of_monoTy bool_ty
 | IntConst n -> ([], int_ty)
 | FloatConst f -> ([], float_ty)
 | StringConst s -> ([], string_ty)
 | NilConst -> ([0],mk_list_ty (TyVar 0))
 | UnitConst -> ([], unit_ty)

let binop_signature binop = match binop with
     IntPlusOp   -> int_op_ty
   | IntMinusOp   -> int_op_ty
   | IntTimesOp   -> int_op_ty
   | IntDivOp   -> int_op_ty
   | FloatPlusOp   -> float_op_ty
   | FloatMinusOp   -> float_op_ty
   | FloatTimesOp   -> float_op_ty
   | FloatDivOp   -> float_op_ty
   | ConcatOp -> string_op_ty
   | ConsOp -> 
       let alpha = TyVar 0
       in ([0], 
              mk_fun_ty alpha (mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha)))
   | CommaOp ->
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1],
            mk_fun_ty alpha (mk_fun_ty beta (mk_pair_ty alpha beta)))
   | EqOp -> 
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))
   | GreaterOp ->
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))

let monop_signature monop = match monop with
    | HdOp -> let alpha = TyVar 0 in([0], mk_fun_ty (mk_list_ty alpha) alpha)
    | TlOp -> let alpha = TyVar 0 in
                  ([0], mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha))
    | IntNegOp -> ([], mk_fun_ty int_ty int_ty)
    | FstOp -> 
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1], mk_fun_ty (mk_pair_ty alpha beta) alpha)
    | SndOp -> 
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1], mk_fun_ty (mk_pair_ty alpha beta) beta)

(* environments *)
type 'a env = (string * 'a) list

let freeVarsEnv l =
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l []

let string_of_env string_of_entry gamma = 
  let rec string_of_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x^ " : "^ string_of_entry y^
                    match xs with [] -> "" | _  -> ", "^
                                                   string_of_env_aux xs
  in
    "{"^ string_of_env_aux gamma^ "}"

let string_of_type_env gamma = string_of_env string_of_polyTy gamma

(*environment operations*)
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

type type_env = polyTy env

let make_env x y = ([(x,y)]: 'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let sum_env (delta: 'a env) (gamma: 'a env) = ((delta@gamma): 'a env)
let ins_env (gamma: 'a env) x y = sum_env (make_env x y) gamma

(*judgment*)
type judgment =
   ExpJudgment of type_env * exp * monoTy
 | DecJudgment of type_env * dec * type_env

let string_of_judgment judgment =
  match judgment with ExpJudgment(gamma, exp, monoTy) ->
        string_of_type_env gamma ^ " |= "^ string_of_exp exp ^
         " : " ^ string_of_monoTy monoTy
  | DecJudgment (gamma, dec, delta) ->
        string_of_type_env gamma ^ " |= "^ string_of_dec dec ^
         " : " ^ string_of_type_env delta

type proof = Proof of proof list * judgment

(*proof printing*)
let string_of_proof p =
  let depth_max = 10 in
  let rec string_of_struts = function
     []    -> ""
   | x::[] -> (if x then "|-" else "|-")  (* ??? *)
   | x::xs -> (if x then "  " else "| ")^ string_of_struts xs
  in let rec string_of_proof_aux (Proof(ant,conc)) depth lst =
    "\n"^ "  "^ string_of_struts lst^
    (if (depth > 0) then "-" else "")^
    let assum = ant in
      string_of_judgment conc ^
      if depth <= depth_max
         then string_of_assum depth lst assum
      else ""
  and string_of_assum depth lst assum =
    match assum with 
       []     -> ""
     | p'::ps -> string_of_proof_aux p' (depth + 1) (lst@[ps=[]])^
                 string_of_assum depth lst ps
  in
    string_of_proof_aux p 0 []^ "\n\n"

type substitution = (typeVar * monoTy) list

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(*unification algorithm*)
(* Problem 1 *)
let rec contains n ty =
  match ty with
    TyVar m -> n=m
  | TyConst(st, typelst) ->
     List.fold_left (fun xl x -> if xl then xl else contains n x) false typelst;;

(* Problem 2 *)
let rec substitute ie ty = 
  let n,sub = ie 
  in match ty with
       TyVar m -> if n=m then sub else ty
     | TyConst(st, typelist) -> TyConst(st, List.map (fun t -> substitute ie t) typelist);;

let polyTySubstitute s (pty:polyTy) =
    match s with  (n,residue) ->
    (match pty with (bound_vars, ty) -> 
           if List.mem n bound_vars then pty
           else ((bound_vars, substitute s ty):polyTy))
    

(* Problem 3 *)
let rec monoTy_lift_subst (s:substitution) ty =
  match ty with
    TyVar m -> subst_fun s m
  | TyConst(st, typelst) ->  TyConst(st, List.map (fun t -> monoTy_lift_subst s t) typelst);;

let rec monoTy_rename_tyvars s mty =
    match mty with
      TyVar n -> (match lookup s n with Some m -> TyVar m | _ -> mty)
    | TyConst(c, tys) -> TyConst(c, List.map (monoTy_rename_tyvars s) tys)

let subst_compose (s2:substitution) (s1:substitution) : substitution =
    (List.filter (fun (tv,_) -> not(List.mem_assoc tv s1)) s2) @ 
    (List.map (fun (tv,residue) -> (tv, monoTy_lift_subst s2 residue)) s1)

let gen (env: type_env) ty =
    let env_fvs = freeVarsEnv env in
    ((List.filter (fun v -> not(List.mem v env_fvs)) (freeVarsMonoTy ty), ty):polyTy)

let freshInstance ((tvs, ty):polyTy) =
    let fresh_subst = List.fold_right (fun tv s -> ((tv,fresh())::s)) tvs [] in
    monoTy_lift_subst fresh_subst ty

let first_not_in n l =
    let rec first m n l =
        if n > 0 then
         if List.mem m l then first (m+1) n l else m :: (first (m+1) (n - 1) l)
        else []
    in first 0 n l

let alpha_conv ftvs (pty:polyTy) =
    match pty with (btvs, ty) ->
    (let fresh_bvars =
         first_not_in (List.length btvs) (ftvs @ (freeVarsPolyTy pty))
     in (fresh_bvars,
         monoTy_lift_subst (List.combine btvs (List.map (fun v -> TyVar v) fresh_bvars))
         ty))

(*
s may contain TyConst?
let polyTy_lift_subst (s:substitution) pty =
    let fvs = List.fold_left accumulate_freeTyVarsMonTy [] (snd(List.split s)) in
    let (nbvs, nty) = alpha_conv fvs pty in
    (nbvs, monoTy_lift_subst s nty)
*)

let polyTy_lift_subst s pty =
	let rec fvsfun x r = match x with
		| TyVar n -> n :: r
		| TyConst (_, l) -> List.fold_right fvsfun l r
	in
	let fvs = List.fold_right fvsfun (snd(List.split s)) [] in
    let (nbvs, nty) = alpha_conv fvs pty in
    ((nbvs, monoTy_lift_subst s nty):polyTy)

let rec mk_bty_renaming n bty =
    match bty with [] -> ([],[])
    | (x::xs) -> (match mk_bty_renaming (n-1) xs
                   with (s,l) -> (((x,n) :: s), n :: l))

let polyTy_rename_tyvars s (bty, mty) =
    let (renaming,new_bty) = mk_bty_renaming (~-7) bty in
    (new_bty, monoTy_rename_tyvars s (monoTy_rename_tyvars renaming mty))

(*
let polyTy_rename_tyvars s (btv, mty) =
    ((List.map (fun n -> match lookup s n with Some m -> m | _ -> n) btv,
      monoTy_rename_tyvars s mty):polyTy)
*)

let env_lift_subst s (env: 'a env) =
    ((List.map (fun (x,polyTy) -> (x,polyTy_lift_subst s polyTy)) env): 'a env)

let env_rename_tyvars s (env: 'a env) =
    ((List.map
      (fun (x,polyTy) -> (x,polyTy_rename_tyvars s polyTy)) env): 'a env)

(* Problem 4 *)
let rec unify eqlst : substitution option =
  let rec addNewEqs lst1 lst2 acc =
    match lst1,lst2 with
      [],[] -> Some acc
    | t::tl, t'::tl' -> addNewEqs tl tl' ((t,t')::acc)
    | _ -> None
  in
  match eqlst with
    [] -> Some([])
    (* Delete *)
  | (s,t)::eqs when s=t -> unify eqs
    (* Eliminate *)
  | (TyVar(n),t)::eqs when not(contains n t)-> 
      let eqs' = List.map (fun (t1,t2) -> (substitute (n,t) t1 , substitute (n,t) t2)) eqs
      in (match unify eqs' with
           None -> None
         | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi))
    (* Orient *)
  | (TyConst(str, tl), TyVar(m))::eqs -> unify ((TyVar(m), TyConst(str, tl))::eqs)
    (* Decompose *)
  | (TyConst(str, tl), TyConst(str', tl'))::eqs when str=str' -> 
      (match (addNewEqs tl tl' eqs) with
        None -> None
      | Some l -> unify l)
    (* Other *)
  | _ -> None
;;


(*-----------------------------------------------*)

(*constraint list*)
type consList = (monoTy * monoTy) list

(*applying a substitution to a proof*)
let rec proof_lift_subst f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_lift_subst f) assum,
          ExpJudgment(env_lift_subst f gamma, exp, monoTy_lift_subst f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_lift_subst f) assum,
          DecJudgment(env_lift_subst f gamma, dec, env_lift_subst f delta))

let rec proof_rename_tyvars f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          ExpJudgment(env_rename_tyvars f gamma, exp,
                      monoTy_rename_tyvars f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          DecJudgment(env_rename_tyvars f gamma, dec,
                      env_rename_tyvars f delta))


let get_ty = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> ty

let get_proof = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> p

let infer_exp gather_exp (gamma:type_env) (exp:exp) = 
  let ty = fresh() in
  let result = 
    match gather_exp gamma exp ty with
       None         -> None
     | Some(proof,sigma) -> match ty with
          | TyVar n -> Some (subst_fun sigma n, proof_lift_subst sigma proof)
          | _       -> None
  in let _ = reset() in
  result;;

let infer_dec gather_dec (gamma:type_env) (dec:dec) =
  let result = 
    match gather_dec gamma dec with
       None -> None
     | Some(proof,delta,sigma) -> Some (proof_lift_subst sigma proof) 
  in let _ = reset() in
  result;;

let string_of_constraints c =
  let rec aux c =
     match c with 
     | [] -> ""
     | [(s,t)] ->  (string_of_monoTy s^ " =?= "^ string_of_monoTy t)
     | (s,t)::c' -> (string_of_monoTy s^ " =?= "^ string_of_monoTy t^
		     "; "^ aux c')
  in ("["^ aux c^ "]\n")

let string_of_substitution s =
  let rec aux s =
     match s with 
     | [] -> ""
     | [(i,t)] -> ((string_of_typeVar i)  ^ " |--> " ^ string_of_monoTy t)
     | (i,t)::s' -> (((string_of_typeVar i)  ^ " |--> ")^
                     string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")

let niceInfer_exp gather_exp (gamma:type_env) exp = 
  let ty = fresh()
  in
  let result = 
    match gather_exp gamma exp ty with
     None ->
      (print_string("Failure: No type for expression: "^
       string_of_exp exp^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy gamma^ "\n");
       raise (Failure ""))
   | Some (p,s) ->
   (string_of_proof p^
	(*
   "Constraints: "^
   string_of_constraints c ^
   "Unifying..."^
   match unify c with
     None -> ("Failure: No solution for these constraints!\n"^
              raise (Failure ""))
   | Some s ->
	*)
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

let niceInfer_dec
    (gather_dec:(type_env -> dec -> (proof * type_env * substitution) option))
    (gamma:type_env) dec = 
  let result = 
    match gather_dec gamma dec with
     None ->
      (print_string("Failure: No type for declaraion: "^
       string_of_dec dec^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy gamma^ "\n");
       raise (Failure ""))
   | Some (p,d,s) ->
   (string_of_proof p^
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

(* Collect all the TyVar indices in a proof *)

let rec collectTypeVars ty lst =
  match ty with
    TyVar m -> m::lst
  | TyConst(st, typelst) -> List.fold_left (fun xl x -> collectTypeVars x xl) lst typelst


let rec collectFreeTypeVars bty ty lst =
  match ty with
    TyVar m -> if List.mem m bty then lst else m::lst
  | TyConst(st, typelst) ->
    List.fold_left (fun xl x -> collectFreeTypeVars bty x xl) lst typelst

let collectPolyTyVars (bty,mty) lst = collectFreeTypeVars bty mty lst

let collectEnvVars (gamma:type_env) lst =
    List.fold_left (fun tys (_,pty)-> collectPolyTyVars pty tys) lst gamma

let collectJdgVars jdg lst =
    match jdg with ExpJudgment(gamma, exp, monoTy) ->
        collectEnvVars gamma (collectTypeVars monoTy lst)
    | DecJudgment(gamma, dec, delta) ->
        collectEnvVars gamma (collectEnvVars delta lst)

let rec collectProofVars prf lst =
  match prf with Proof (assum, jdg)
   -> collectAssumVars assum (collectJdgVars jdg lst)
and collectAssumVars assum lst =
  match assum with 
    []     -> lst
  | p::ps -> collectAssumVars ps (collectProofVars p lst)

(* Rename all the variables in a proof in a canonical way *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

let canonicalize_proof prf_opt =
    match prf_opt with None -> None
    | Some(ty, prf) ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x,idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf (collectTypeVars ty []))) 
      ([],1)
  in Some(monoTy_rename_tyvars varlst ty, proof_rename_tyvars varlst prf)

let canon = canonicalize_proof

let canon_dec prf_opt =
    match prf_opt with None -> None
    | Some prf ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x, idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf []))
      ([],1)
  in Some(proof_rename_tyvars varlst prf)
