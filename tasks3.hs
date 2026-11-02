import Data.Char  

--toUpper import versjon
toUppers :: String -> String
toUppers xs = map toUpper xs

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = takeWhile (==x) (x:xs) : group (dropWhile (==x) xs)

elTall :: [a] -> (a,Int)
elTall xs = (head xs, length xs)

--letterFreq :: String -> [(Char,Int)]

quickSort :: Ord a => [a] -> [a]
quickSort xs = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x] 
where 
    x = (xs !! ((length xs) / 2))