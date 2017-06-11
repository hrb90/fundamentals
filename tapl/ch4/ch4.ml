type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term;;

let rec is_numeric t =
  match t with
  | TmZero -> true
  | TmSucc x -> is_numeric x
  | _ -> false;;

let rec is_val t =
  match t with
  | TmTrue -> true
  | TmFalse -> true
  | t when is_numeric t -> true
  | _ -> false;;

exception NoRuleApplies;;

let rec single_step t =
  match t with
  | TmIf (TmTrue, t2, _) -> t2
  | TmIf (TmFalse, _, t3) -> t3
  | TmIf (t1, t2, t3) -> let t1' = single_step t1
                         in
                         TmIf (t1', t2, t3)
  | TmSucc t1 -> let t1' = single_step t1
                in
                TmSucc (t1')
  | TmPred (TmZero) -> TmZero
  | TmPred (TmSucc nv) when is_numeric nv -> nv
  | TmPred (t1) -> let t1' = single_step t1
                 in
                 TmPred (t1')
  | TmIsZero (TmZero) -> TmTrue
  | TmIsZero (TmSucc nv) when is_numeric nv -> TmFalse
  | TmIsZero (t1) -> let t1' = single_step t1
                     in
                     TmIsZero (t1')
  | _ -> raise NoRuleApplies;;

let rec eval t =
  try let t' = single_step t
    in eval t'
  with NoRuleApplies -> t;;
