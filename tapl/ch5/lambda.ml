(*
  Implementation of the untyped lambda calculus
  The lambda_exp datatype is an abstract syntax tree that can be pattern matched
  ML is cool
*)

type lambda_var = LambdaVar of string;;

type lambda_exp =
 | Variable of lambda_var
 | Application of lambda_exp * lambda_exp
 | Abstraction of lambda_var * lambda_exp;;

(*
  Constructors
*)

let mvr s =
  Variable (LambdaVar s);;

let app e1 e2 =
  Application (e1, e2);;

let lam s e =
  Abstraction (LambdaVar s, e);;

(*
  Some expressions
*)

let x = mvr "x";;
let f = mvr "f";;
let y = mvr "y";;
let z = mvr "z";;
let half_y = lam "x" (app f (app x x));;
let y_combinator_call_by_name = lam "f" (app half_y half_y);;
let half_z = lam "x" (app f (lam "y" (app (app x x) y)));;
let y_combinator_call_by_value = lam "f" (app half_z half_z);;
let id = lam "x" x;;
let example = app id
                  (app id
                       (lam "z"
                            (app id
                                 (mvr "z"))));;

(*
  Evaluation
*)

module SS = Set.Make(String);;

let rec free_vars term =
  match term with
  | Variable LambdaVar lv -> SS.singleton lv
  | Application (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Abstraction (LambdaVar lv, e) -> SS.remove lv (free_vars e);;

let rec bound_vars term =
  match term with
  | Variable _ -> SS.empty
  | Application (e1, e2) -> SS.union (bound_vars e1) (bound_vars e2)
  | Abstraction (LambdaVar lv, e) -> SS.union (SS.singleton lv) (bound_vars e);;

let is_combinator term =
  SS.is_empty (free_vars term);;

let rec rename old_name new_name term =
  let renamer = rename old_name new_name
  in
  match term with
  | Variable LambdaVar lv when old_name = lv -> Variable (LambdaVar new_name)
  | Variable LambdaVar lv -> term
  | Application (e1, e2) -> Application ((renamer e1), (renamer e2))
  | Abstraction (LambdaVar lv, e) when old_name = lv ->
      Abstraction (LambdaVar new_name, renamer e)
  | Abstraction (l, e) -> Abstraction (l, renamer e);;

(*
  First attempt at writing the reduction function, on pp. 69-70 of TaPL
*)
let rec dumb_reduce var new_term term =
  let dumb_reducer = dumb_reduce var new_term
  in
  match term with
  | Variable LambdaVar lv when lv = var -> new_term
  | Variable LambdaVar lv -> term
  | Application (e1, e2) -> Application ((dumb_reducer e1), (dumb_reducer e2))
  | Abstraction (LambdaVar lv, e) when lv = var -> term
  | Abstraction (l, e) -> Abstraction (l, dumb_reducer e);;

let dumb_fail = dumb_reduce "x" z (lam "z" x);; (* Outputs \z.z *)

(*
  Adds primes to var until we reach a new variable name not free in term
*)
let rec primify var term =
  let primed = var ^ "'"
  in
  if (SS.mem primed (free_vars term)) then primify primed term else primed;;

(*
  returns a lambda-abstraction replacing var with a variable not free in new_term
*)
let alpha_convert var exp new_term =
  let new_var = primify var new_term
  in
  Abstraction (LambdaVar new_var, rename var new_var exp);;

let rec beta_reduce var new_term term =
  let beta_reducer = beta_reduce var new_term
  in
  match term with
  | Variable LambdaVar lv when lv = var -> new_term
  | Variable LambdaVar lv -> term
  | Application (e1, e2) -> Application ((beta_reducer e1), (beta_reducer e2))
  | Abstraction (LambdaVar lv, e) when lv = var -> term
  | Abstraction (LambdaVar lv, e) when SS.mem lv (free_vars new_term) ->
      beta_reducer (alpha_convert lv e new_term)
  | Abstraction (l, e) -> Abstraction (l, beta_reducer e);;

(*
  The book says raising an exception like this is an antipattern
  But then it goes on to do it???
  Idk, man
*)
exception CannotReduce;;

let rec cbv_single_step term =
  let step = cbv_single_step
  in
  match term with
  | Application (Abstraction (LambdaVar x, e1), e2) -> beta_reduce x e2 e1
  | Application (e1, e2) -> Application (step e1, e2)
  | Abstraction (lv, e) -> Abstraction (lv, step e)
  | _ -> raise CannotReduce;;

let rec cbv_eval term =
  try let term' = cbv_single_step term
  in cbv_eval term'
  with CannotReduce -> term;;
