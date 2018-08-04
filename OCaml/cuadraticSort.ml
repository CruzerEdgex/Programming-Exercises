let swap (arr: 'a array) (i : int) (j : int) : unit =
  let aux = arr.(j) in
  arr.(j) <- arr.(i);
  arr.(i) <- aux

let selectionSort (a : 'a array) : 'a array =
  let n = Array.length a in
  for i = 0 to (n-2) do
    let min = ref i in
    for j = i+1 to (n-1) do
      (if a.(!min) > a.(j) 
        then min := j
        else ()
      ) 
    done;
    swap a i !min
  done;
  a

let insertionSort (a : 'a array) : 'a array =
  let n = Array.length a in
  for i = 1 to (n-1) do
    let j = ref i in
    while !j > 0 && a.(!j) < a.(!j - 1) do
      swap a !j (!j - 1); 
      j := !j - 1
    done;
  done;
  a

let bubbleSort (a : 'a array) : 'a array =
  let n = Array.length a in
  let i = ref 0 in
  let is_sorted = ref false in
  while !is_sorted = false && !i < (n - 2) do
    is_sorted := true;
    for j = 0 to (n - 2 - !i) do
      if a.(j) > a.(j+1) 
      then (is_sorted := false; swap a j (j+1)) 
      else ()  
    done;
    i := !i + 1
  done;
  a