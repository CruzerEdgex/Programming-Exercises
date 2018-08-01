
--Problem 31: Check if a number is prime
isPrime' :: (Integral a) => a -> Bool
isPrime' 1 = False
isPrime' 2 = True
isPrime' 3 = True
isPrime' n = if odd n then prime n 3 else False where 
    prime n i 
        | i >= (div n 2) = True
    prime n i = if mod n i == 0 then False else prime n (i+2)

--Problem 32: GCD
gcd' :: (Integral a) => a -> a -> a
gcd' n 0 = n
gcd' n m 
    | n >= m = gcd' m (mod n m)
    | n < m = gcd' m n


--Problem 33: Check if 2 number are coprimes.

coprimes' :: (Integral a) => a -> a -> Bool
coprimes' n m = if gcd' n m == 1 then True else False