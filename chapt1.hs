{-Exercises
1. Give another possible calculation for the result of double (double 2).

double (double 2)
(double 2) + (double 2)
(2+2) + (double 2)
(2+2) + (2+2)
8

2. Show that sum [x] “ x for any number x.

sum [x] =
x + sum [] =
x + 0 =
x


3. Define a function product that produces the product of a list of numbers,
and show using your definition that product [2,3,4] “ 24.

prod [2, 3, 4] =
2 * prod [3, 4] =
2 * (3 * prod[4]) =
2 * (3 * (4 * prod[])) =
2 * (3 * (4 * 1)) =
24
 -}

pproduct :: [Int] -> Int
pproduct [] = 1
pproduct (x:xs) = x * pproduct (xs)

{- 4. How should the definition of the function qsort be modified so that it pro
duces a reverse sorted version of a list?

Simply swap qsort smaller and qsort larger on line 2.

5. What would be the effect of replacing <= by < in the original definition of
qsort? Hint: consider the example qsort [2,2,3,1,1]

It would remove duplicate objects, i.e there would be at most 1 instance of any object (like 2 and 1 in the example)

-}

