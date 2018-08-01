module Logica1 where

import qualified Data.List as L

--Alias

type InfixExpression = String
type PostfixExpression = String
type Symbol = Char
type Operator = Char
type Operand = Char
type TruthValue = Char
type Row = [TruthValue]
type TruthTable = [Row]

--MINOR FUNCTIONS

--Insert an element into a list
append :: [a] -> a -> [a]
append list e = e:list

--Assign a symbol to string
assignSymbol :: String -> Symbol
assignSymbol "then" = '>'
assignSymbol "not" = '-'
assignSymbol "or" = '+'
assignSymbol "and" = '*' 
assignSymbol [x] = x

--Return the priority of an operator
priority :: Operator -> Int
priority '(' = 1
priority ')' = 6
priority '+' = 3
priority '*'  = 4
priority '-' = 5
priority '>' = 2
priority _   = 0

--Transforms a string into a equivalent [Char] who represents a logic expression
expToList :: String -> InfixExpression
expToList = map assignSymbol . words

--Obtains the distinct atoms from a postfix expression
getAtoms :: PostfixExpression -> ([Operand], Int)
getAtoms expression = (variables, length variables) where
    variables = map head ( L.group ( L.sort ( filter (\x -> elem  x ['a' .. 'z']) expression ) ) )

--Logic operators

lor :: Operand -> Operand -> TruthValue
lor '0' '0' = '0'
lor _ _ = '1'  

land :: Operand -> Operand -> TruthValue
land '1' '1' = '1'
land _ _ = '0'

lthen :: Operand -> Operand -> TruthValue
lthen '1' '0' = '0'
lthen _ _ = '1'

lnot :: Operand -> TruthValue
lnot '0' = '1'
lnot '1' = '0'


--CORE FUNCTIONS

--Infix to postfix

--Process the expression symbols from left to right (intermediate result)
toPostfix :: InfixExpression -> [[Char]]
toPostfix = foldl check [[],[]] where
    check qs '(' = leftBracket qs
    check qs ')' = rightBracket qs 
    check qs '+' = extractFromStack (\x -> priority x >= priority '+') qs '+'
    check qs '*' = extractFromStack (\x -> priority x >= priority '*') qs '*'
    check qs '-' = extractFromStack (\x -> priority x >= priority '-') qs '-'
    check qs '>' = extractFromStack (\x -> priority x > priority '>') qs '>'
    check [queue, stack] x  = [x : queue, stack]

--Drop the stack into the list
postfixExpression :: [[Char]] -> PostfixExpression
postfixExpression [queue, stack] = foldl append queue stack
    
--What to do if the extracted character is a :

--Just add '(' to the stack
leftBracket :: [[Char]] -> [[Char]]
leftBracket [queue, stack] = [queue, '(' : stack]

--Add operand from stack to queue until '(' is found
rightBracket :: [[Char]] -> [[Char]]
rightBracket [queue, stack] = [newqueue, newstack] where
    newstack = tail ( dropWhile (\x -> x /= '(') stack )
    newqueue = foldl append queue (takeWhile (\x -> x /= '(') stack)

--Given a condition to extract, this function extracts from the stack and insert into
--the list until the condition fails
extractFromStack :: (Char -> Bool) -> [[Char]] -> Char -> [[Char]]
extractFromStack _ [queue, []] operator = [queue, [operator]]
extractFromStack condition [queue, stack] operator = [newqueue, operator:newstack] where
    newstack = dropWhile condition stack
    newqueue = foldl append queue (takeWhile condition stack)

--Truth Table Generator

--Generates a truth table for a expression
truthTable :: PostfixExpression -> TruthTable
truthTable expression = map (\x -> x ++ [(evaluation ( valuate expression atoms x ) )])  valuations where 
    atomsdata = getAtoms expression
    atoms = fst atomsdata
    nAtoms = snd atomsdata
    valuations = varRep01 nAtoms

--Poor optimization, this should use a dictionary, I'm going to fix it when i undertand them completely
valuate :: PostfixExpression -> [Operand] -> Row -> PostfixExpression
valuate expression [] [] = expression
valuate expression (x:atoms) (y:valuation) = valuate newexpression atoms valuation where
    newexpression = map (\z -> if z == x then y else z) expression

--Computes a valuated expression, it's ok, but it could make dictionary and not lists
--that would be useful to valuate function
evaluation :: PostfixExpression -> TruthValue
evaluation expression = head ( foldl redux [] expression )
    where   
            redux (x1:x2:xs) '+' = (lor x1 x2) : xs
            redux (x1:x2:xs) '*' = (land x1 x2) : xs
            redux (x1:x2:xs) '>' = (lthen x2 x1) :xs
            redux (x1:xs) '-' = (lnot x1) : xs
            redux xs x = x : xs

--Variations with repetition over {0,1} 
--I need to optimize this... Sorting isn't an option since sorting 2^n has 2^n * n complexity.
--I don't know how to fold this with one iteration , and definitely using ++ isn't a good solution.
varRep01 :: Int -> [Row]
varRep01 0 = []
varRep01 1 = [['0'],['1']]
varRep01 n = foldr addn1 [] recursion ++ foldr addn0 [] recursion  where
    recursion = varRep01 (n-1)
    addn0 xs acum = ('1':xs) : acum 
    addn1 xs acum = ('0':xs) : acum

--MAIN FUNCTION

--Main program, no executable(need to improve my skills with IO)
programAux :: String -> PostfixExpression
programAux = reverse . postfixExpression . toPostfix . expToList

program :: PostfixExpression -> TruthTable
program = truthTable . programAux


