(*
  Tree structures!
*)

type 'a binary_tree =
 | Leaf of 'a
 | Tree of 'a binary_tree * 'a binary_tree;;

let rec foldl comb tree =
  let foldc = foldl comb
  in
  match tree with
  | Leaf x -> x
  | Tree (left, right) -> comb (foldc left) (foldc right);;

let rec contains v tree =
  let containsv = contains v
  in
  match tree with
  | Leaf x -> x == v
  | Tree (left, right) -> containsv left || containsv right;;

let rec search test tree =
  let searcht = search test
  in
  match tree with
  | Leaf x -> if (test x) then [x] else []
  | Tree (left, right) -> List.append (searcht left) (searcht right);;
