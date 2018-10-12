type t = (int * int)

let pairOrder (a : int) (b : int) : (int * int) =
  if a >= b then (a, b) else (b, a)

let mcd (a : int) (b : int) : int = 
  let (higher, lower) = pairOrder (abs a) (abs b)in
  let rec aux x y =
    match (x, y) with
      |(0, y) -> y
      |(x, 0) -> x
      |(x, y) -> aux y (x % y)
  in
  (aux higher lower)

let mcm (a : int) (b : int) : int = abs ((a * b) / mcd a b)

let normalize ((a, b) : int * int) : int * int =
  let simplifier = mcd a b in
  let (x, y) = (abs (a / simplifier), abs (b / simplifier)) in
    if a < 0 && b < 0 || a >= 0 && b >= 0 then (x, y) else (-x, y)   

let add (a, b) (c, d) = 
  let denom = mcm b d in
  let num = a * (denom / b) + c * (denom / d) in
    normalize (num, denom)
    
let sub (a, b) (c, d) = add (a, b) (neg (c, d))
    
let mult (a, b) (c, d) = 
  let (x, y) = (a * c, b * d) in
  normalize (x, y)
    
let quot (a, b) (c, d) = mult (a, b) (d, c)
    
let neg (a, b)  = normalize (-a, b)