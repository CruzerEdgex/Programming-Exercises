let swap (arr: 'a array) (i : int) (j : int) : unit =
  let aux = arr.(j) in
  arr.(j) <- arr.(i);
  arr.(i) <- aux

let hoare_partition (a : 'a array) (li : int) (ls : int) : int =
  let pivot = a.(li) in
  let i = ref (li - 1) in
  let j = ref (ls + 1) in
  let flag = ref false in
  while !flag = false do
    i := !i + 1;
    j := !j - 1;

    while a.(!i) < pivot do
      i := !i + 1
    done;

    while a.(!j) > pivot do 
      j := !j - 1
    done;

    (if !i >= !j 
    then flag := true
    else swap a !i !j)

  done;
  !j

let quicksort (a : 'a array) : 'a array =
  let n = Array.length a in
  let rec qs arr li ls =
    if (ls <= li) 
    then ()
    else (
      let p = hoare_partition arr li ls in
      qs arr li p;
      qs arr (p+1) ls;   
    ) 
  in 
  qs a 0 (n-1);
  a 