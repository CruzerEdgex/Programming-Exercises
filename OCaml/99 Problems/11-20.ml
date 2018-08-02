(*11. Modified run-length encoding. (easy)*)

type 'a rle = 
  Singleton of 'a | 
  Lot of ('a * int)
;;

let rec modRunLength (l : 'a list) : ('a rle) list =
  match l with
    | [] -> []
    | x :: xs -> let mrl a l' =
      match l' with
        | [] -> [Singleton a]
        | (Singleton y::ys) -> if a = y then Lot (y,2) :: ys 
          else Singleton a :: Singleton y :: ys
        | (Lot (y,n)::ys) -> if a = y then Lot (y,n+1) :: ys 
          else Singleton a :: Lot (y,n) :: ys
    in mrl x (modRunLength xs)
;;

(*12. Decode a run-length encoded list. (medium)*)

let rec addnxtol (x : 'a) (n : int) (l : 'a list) : ('a list) =
  match n with
    | 0 -> l
    | k -> addnxtol x  (k-1) (x :: l)
;;

let rec decodeRunLength (l : ('a rle) list) : 'a list =
  match l with
    | [] -> []
    | Singleton x :: xs -> x :: (decodeRunLength xs)
    | Lot (x,n) :: xs -> addnxtol x n (decodeRunLength xs)
;;

(*13. Run-length encoding of a list (direct solution). (medium)*)

(*????????????????????????????*)

(*14. Duplicate the elements of a list. (easy)*)

let rec duplicate (l : 'a list) : 'a list =
  match l with
    | [] -> []
    | x :: xs -> x :: x :: duplicate xs
;;

(*15. Replicate the elements of a list a given number of times. (medium)*)

let rec replicate (l : 'a list) (n : int) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> addnxtol x n (replicate xs n)
;; 

(*16. Drop every N'th element from a list. (medium)*)

let drop_n_mults (l: 'a list) (n : int) : 'a list =
  let rec n_mults l' k =
    match l' with
      | [] -> []
      | x :: xs -> if (k mod n) = 0 then n_mults xs (k+1) else x :: n_mults xs (k+1)
    in n_mults l 1
;; 

(*17. Split a list into two parts; the length of the first part is given. (easy)*)

let rec splitAt (l : 'a list) (n : int) : ('a list) * ('a list) =
  if n >= 0 then 
    match (l, n) with
      | (xs,0) -> ([],xs)
      | (x :: xs, _) -> let rest = splitAt xs (n-1) in
        (x :: fst rest, snd rest)
      | (_,_) -> ([],[])
  else ([],l)
;;

(*18. Extract a slice from a list. (medium)*)

let slice (l : 'a list) (i : int) (j : int) : 'a list =
  let rec slc l' i' j' =
    if i' >= j' then [] else
      match l', i', j' with
        | ([],_,_) -> []
        | (x :: xs, 0, _) -> x :: (slc xs 0 (j'-1))
        | (x :: xs, _, _) -> slc xs (i'-1) (j'-1)
  in slc l (if i <= 0 then 0 else (i-1)) j

(*19. Rotate a list N places to the left. (medium)*)

let rotate (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | _ -> let len = List.length l in
      let split = if n >= 0 then splitAt l (n mod len)
      else splitAt l (len + (n mod len)) in
    (snd split) @ (fst split)
;;

(*20. Remove the K'th element from a list. (easy)*)

let rec removeNth (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | x :: xs -> if n = 1 then xs else x :: removeNth xs (n-1)
;;
