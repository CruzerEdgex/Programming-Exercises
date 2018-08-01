--PROBLEMS WITH LISTS

--Some problems are really trivial in haskell,
--so I'm not using functions that solves them directly or any other high level function
--like fold, map, etc. 

--Problem 1: Find the last element of a list.

last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs

--Problem 2: Find the last but one element of a list.

lastButOne :: [a] -> a
lastButOne [] = error "Empty list"
lastButOne [x] = error "One element list"
lastButOne [x,y] = x 
lastButOne (x:xs) = lastButOne xs

--Problem 3: Find the kth element of a list. First is 1.

kthElement :: (Integral b) => b -> [a] -> a
kthElement k [] = error "Couldn't find the element"
kthElement 1 (x:xs) = x
kthElement k (x:xs)
    | k > 0 = kthElement (k-1) xs
    | otherwise = error "Couldn't find the element"

--Problem 4: Find the number of elements of a list.

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs 

--Problem 5: Reverse a list.(Not using fold and reverse and (++))

reverse' :: [a] -> [a]
reverse' list =  rev list [] where
    rev [] stack = stack
    rev (x:xs) stack = rev xs (x:stack)

--Problem 6: Identify if a list is palindrome

palindrome' :: (Eq a) => [a] -> Bool
palindrome' list = if list == reverse' list then True else False

--Problem 7: Flat a list

--auxiliar, (++) equivalent

concatenate :: [a] -> [a] -> [a]
concatenate [] xr = xr
concatenate (x:xl) xr = x : concatenate xl xr

flatList :: [[a]] -> [a]
flatList [] = []
flatList [xs] = xs
flatList (x:xs) =  concatenate x (flatList xs)
 
--Problem 8: Eliminate consecutive duplicates

delConsDup :: (Eq a) => [a] -> [a]
delConsDup [] = []
delConsDup [x] = [x]
delConsDup (x1:x2:xs) = if x1 == x2 then nonDup_xs else x1:nonDup_xs where
    nonDup_xs = delConsDup (x2:xs)

--Problem 9: Pack consecutive duplicates into sublists

groupCons :: (Eq a) => [a] -> [[a]]
groupCons [] = []
groupCons [x] = [[x]]
groupCons (x:xs) = if x == firstBlockClass 
    then addElement x 
    else addBlock x 
    where
        blockList = groupCons xs
        firstBlock = head blockList
        otherBlocks = tail blockList
        firstBlockClass = head firstBlock
        addElement a = (a:firstBlock) : otherBlocks
        addBlock a = [a] : blockList

--Problem 10: Run-length encoding of a list, using 9)

ineffRunLength :: (Eq a, Integral b) => [a] -> [(a,b)]
ineffRunLength list = getHeads (groupCons list) where
    getHeads [] = []
    getHeads (x:xs) = ((,) (head x) (length' x)): (getHeads xs)

--Problem 11: Modify run length enconding, only duplicates are tuples. 
--(Boring, lists in haskell doesn't allow it, so I must write all as strings)

--Problem 12: Given a run length encoding, unpack it. Gonna use 10) format, not 11).

decRunLength :: (Eq a, Integral b) => [(a,b)] -> [a] 
decRunLength [] = []
decRunLength ((e,n):xs) = unpack (e,n) (decRunLength xs) where
    unpack (e,0) list = list
    unpack (e,n) list =  e : unpack (e,n-1) list
 
--Problem 13: Solve the problem 11 without generate the groups explicitely.

runLength :: (Eq a, Integral b) => [a] -> [(a,b)]
runLength [] = []
runLength [x] = [(x,1)]
runLength (x:xs) = if x == fst' 
    then incTuple 
    else addTuple x
    where
        tupleList = runLength xs
        firstTuple = head tupleList
        fst' = fst firstTuple
        snd' = snd firstTuple
        otherTuples = tail tupleList
        incTuple = ((,) fst' (snd' + 1)) : otherTuples
        addTuple a = ((,) a 1) : tupleList

--From now, I will use standard functions instead of my functions, if I coded them before.
--If a problem is trivial, I will not use function that solve it directly, but otherwise,
--I will use functions like fold, map, etc.

--Problem 14: Duplicate elements of a list.

duplicate' :: [a] -> [a]
duplicate' [] = []
duplicate' (x:xs) = x : x : (duplicate' xs)

--Problem 15: Multiply the amount of every element by k ej: [1,2] = [k times 1 k times 2]

multiplicate :: (Integral b) => b-> [a] -> [a]
multiplicate _ [] = []
multiplicate n (x:xs)
    | n < 0 = error "Can't multiply by negative."
    | otherwise =  ntimes n x (multiplicate n xs) where
        ntimes 0 x list = list
        ntimes k x list = x : (ntimes (k-1) x list)

--Problem 16: Drop elements in n multiple position. First position is 1.

dropNk :: (Integral b) => b -> [a] -> [a]
dropNk 0 list = list
dropNk n list = dropModn n 1 list where
    dropModn _ _ [] = []
    dropModn  n n' (x:xs) = if (mod n' n) == 0
        then rest
        else x:rest where
            rest = dropModn n (n'+1) xs

--Problem 17: Split a list into two parts. The length of the first part is given.

splitAt' :: (Integral b) => b -> [a] -> ([a],[a])
splitAt' 0 list = ([],list)
splitAt' n [] = ([],[])
splitAt' n (x:xs)
    | n < 0 = ([],x:xs)
    | otherwise = (x : leftlist, rightlist) where
        recStep = splitAt' (n-1) xs
        leftlist = fst recStep
        rightlist = snd recStep

--Problem 18: Extract a slice from a list. Include the indices. First element is at index 1.
--Restrictions: 1 <= i1 <= i2 <= n
slice' :: (Integral b) => b -> b -> [a] -> [a]
slice' _ _ [] = []
slice' i1 i2 (x:xs)
    | (i2 < i1) = reverse (slice' i2 i1 (x:xs)) --Backward slice'
    | (i2 <= 0) = [] --Change (i2 <= 0) by (i2 <= 1)
    | (i1 <= 1) = x : (slice' 0 (i2-1) xs)  --And (i1 <= 1) by (i1 <= 0), then the function 
    | otherwise = (slice' (i1-1) (i2-1) xs) --doesn't include borders.

--Problem 19: Rotate a list n place to the left.

rotateLeft :: (Integral b) => b-> [a] -> [a]
rotateLeft n list = snd' ++ fst' where 
    n' = mod n (length' list) 
    split = splitAt' n' list
    snd' = snd split
    fst' = fst split

--Problem 20: Remove the kth element of a list

removeKth :: (Integral b) => b-> [a] -> [a]
removeKth _ [] = error "Out of range index."
removeKth 1 (x:xs) = xs
removeKth n (x:xs)
    | n <= 0 = error "Out of range index."
    | otherwise = x: removeKth (n-1) xs

--Problem 21: Insert an element befor a list position. So, insert in 1 is insert at start

insertElement :: (Integral b) => a -> b -> [a] -> [a]
insertElement e _ [] = [e]
insertElement e k list
    | k <= 1 = e : list
insertElement e k (x:xs) = x : (insertElement e (k-1) xs)

--Problem 22: Create a list containing all integer in a given range

rangeIntegral :: (Integral b) => b -> b -> [b]
rangeIntegral a b
    | a == b = [b]
    | a < b = a : (rangeIntegral (a+1) b) 
    | a > b = a : (rangeIntegral (a-1) b)

--Problem 23,24,25 are about randomness. 

--26) and 27) asummes a list with no repetited elements

--Problem 26: Generate every combination of k elements from a set with n elements.

--Problem 27: Group elements of a set under a predicate.

--Problem 28: Sort a list using generic criteries
--Not optimum.
quickSort :: (Ord a, Eq a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort _ [] = []
quickSort f (x:xs) =  (quickSort f leftside) ++ [x] ++ (quickSort f rightside) where
    leftside = [e | e <- xs , f e x == LT || f e x == EQ]
    rightside = [e | e <- xs , f e x == GT]
    
