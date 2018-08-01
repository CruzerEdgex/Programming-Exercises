--Polynomials are represented from a0x^0 to a(n-1)x^(n-1)
import qualified Data.List as L
type Polynomial = [Integer]

--Polynomial evaluation
hornerEvaluation :: Integer -> Polynomial -> Integer
hornerEvaluation _ [] = 0
hornerEvaluation x (a:px) = a + x * (hornerEvaluation x px)

--Polynomial sum
polynomialSum :: Polynomial -> Polynomial -> Polynomial
polynomialSum [] [] = []
polynomialSum (a:px) [] = a : polynomialSum px [] 
polynomialSum [] (b:qx) = b : polynomialSum qx []
polynomialSum (a:px) (b:qx) = (a + b): polynomialSum px qx

--Polynomial substraction
polynomialSubs :: Polynomial -> Polynomial -> Polynomial
polynomialSubs [] [] = []
polynomialSubs (a:px) [] = a : polynomialSubs px [] 
polynomialSubs [] (b:qx) = (-b) : polynomialSubs qx []
polynomialSubs (a:px) (b:qx) = (a - b): polynomialSubs px qx

--Delete innecesary 0's from a  polynomial
adjustGrade :: Polynomial -> Polynomial
adjustGrade = reverse . dropWhile (\x -> x == 0) . reverse 

--Amplify every coeffiecient of a polynomial by an escalar
escalarProduct :: Integer -> Polynomial -> Polynomial
escalarProduct escalar = map (*escalar)

--Grade of a polynomial. May be necessary adjust grade.
polynomialGrade :: Polynomial -> Integer
polynomialGrade = (+) (-1) . L.genericLength

--Multiply a term of a polynomial with another polynomial
nto1Product :: Integer -> Integer -> Polynomial -> Polynomial
nto1Product escalar 0 px = escalarProduct escalar px
nto1Product escalar grade px = 0 : (nto1Product escalar (grade - 1) px)

--Classic polynomial product, n^2 complexity.
classicProduct :: Polynomial -> Polynomial -> Polynomial
classicProduct poly1 poly2 = classProd 0 poly1 poly2 where
    classProd n [] qx = []
    classProd n (a:px) qx = polynomialSum (nto1Product a n qx) (classProd (n+1) px qx)  
