(*
  Just some simple list methods to practice OCaml
*)

let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl);;

let rec filter test l =
  match l with
  | [] -> []
  | hd::tl -> let rest = filter test tl
              in
              if (test hd) then hd::rest else rest;;

let rec foldr init comb l =
  match l with
  | [] -> init
  | hd::tl -> comb hd (foldr init comb tl);;

let len l =
  let combiner _ a = a + 1
  in
  foldr 0 combiner l;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> hd::(append tl l2);;

let rec qsort l =
  match l with
  | [] -> []
  | hd::tl -> let bigTest x = x >= hd
              and smallTest x = x < hd
              in
              append (qsort (filter smallTest tl)) (hd::(qsort (filter bigTest tl)));;
