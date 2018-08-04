(*"merge" solo funciona bajo el contexto dado por mergesort, es decir:
  1)|a| = n1+n2 = n, |a1| = n1, |a2| = n2
  2) n1 = n/2
  3) a1 y a2 arreglos ordenados
  quizás debería estar como función local,
  pero dejarla metida entre medio
  hace ilegible el código*)

let merge a1 a2 a n1 n2 =
  let  i, j, k = ref 0, ref 0, ref 0 in
    while !i < n1 && !j < n2 do
      if a1.(!i) <= a2.(!j) 
      then (a.(!k) <- a1.(!i); incr i)
      else (a.(!k) <- a2.(!j); incr j)
      ;
      incr k  
    done;
    while !i < n1 do
      a.(!k) <- a1.(!i);
      incr k;
      incr i 
    done;
    while !j < n2 do
      a.(!k) <- a2.(!j);
      k := !k+1;
      j := !j+1 
    done;
    a

let mergeSort x =
  let len = Array.length x in
  let rec arrayMergeSort a n = 
      if n <= 1 then a
      else
        let n1 = n/2 in
        let n2 = n-n1 in
        let left = Array.sub a 0 n1 in
        let right = Array.sub a n1 n2 in
          merge (arrayMergeSort left n1) (arrayMergeSort right n2) a n1 n2
  in arrayMergeSort x len
