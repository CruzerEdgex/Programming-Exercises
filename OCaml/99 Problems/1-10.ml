(*1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)*)

let rec last (l : 'a list) : ('a option) =
  match l with
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs
;;

(*2. Find the last but one (last and penultimate) elements of a list. (easy)*)

let rec lastButOne (l : 'a list) : ('a option) =
  match l with
    | [] -> None
    | [x] -> None
    | [x;y]-> Some x
    | _::xs -> lastButOne xs
;; 

(*3. Find the k'th element of a list. (easy)*)

let rec kthElement (l : 'a list) (k : int) : ('a option) =
  match l with
    | [] -> None
    | x::xs -> if (k = 1) then (Some x) else kthElement xs (k-1)
;; 

(*4. Find the number of elements of a list. (easy)*)

let length (l : 'a list) : int =
  let rec len l' n =
    match l' with
      | [] -> n
      | _::xs -> len xs (n+1)
  in len l 0
;;

(*5. Reverse a list. (easy)*)

let rec reverse (l : 'a list) : ('a list) =
  let rec rev l' r =
    match l', r with
      | ([], r) -> r
      | (x::xs, r) -> rev xs (x::r)
  in rev l []
;;

(*6. Find out whether a list is a palindrome. (easy)*)

let palindrome (l : 'a list) : bool =
  l = (reverse l)
;;

(*7. Flatten a nested list structure. (medium)*)

let rec flatten (l : ('a list) list) : ('a list) =
  match l with
    | [] -> []
    | [x] -> x
    | (x::xs) -> x @ (flatten xs)  
;;

(*8. Eliminate consecutive duplicates of list elements. (medium)*)

let rec delCons (l : ('a list)) : ('a list) =
  match l with
    | [] -> []
    | [x] -> [x]
    | (x1::(x2::xs)) -> if (x1 = x2) then delCons (x2::xs) else (x1::delCons (x2::xs))
;; 

(*9. Pack consecutive duplicates of list elements into sublists. (medium)*)

let addToFirst (e : 'a)  (l : (('a list) list)) : (('a list) list)  =
  match l with
    | [] -> [[e]]
    | (x::xs) -> (e::x)::xs
;;

let rec pack (l : ('a list)) :  (('a list) list) =
  match l with
    | [] -> []
    | [x] -> [[x]]
    | (x1::(x2::xs)) -> let aux = pack (x2::xs) in
      if (x1 = x2) then (addToFirst x1 aux) else ([x1] :: aux)
;;

(*10. Run-length encoding of a list. (easy)*)

let rec runLength (l : ('a list)) : (('a * int) list) =
  match l with
    | [] -> []
    | [x] -> [(x,1)]
    | (x1::(x2::xs)) -> 
        let aux = runLength (x2::xs) in
        let incTuple t =
          match t with
            | ((a,b)::ys) -> ((a,(b+1))::ys) 
            | _ -> [] in
        if (x1 = x2) then (incTuple aux) else ((x1,1) :: aux)
;;