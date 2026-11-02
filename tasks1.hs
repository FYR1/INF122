

--Plus n to all elements
plu [] n = []
plu (x:xs) n = (x + n) : plu xs n

--Palindrome
pali str = str == reverse str

--Hjuster (overflows if length of word exceeds line width. 
--How desirable this is depends on the desired application.)
hjuster n xs 
  | n >= length xs = (replicate (n - length xs) ' ') ++ xs
  | otherwise = take n xs ++ "\n" ++ hjuster n (drop n xs)

--Vjuster
vjuster n xs 
  | n >= length xs =  xs ++ (replicate (n - length xs) ' ')
  | otherwise = take n xs ++ "\n" ++ vjuster n (drop n xs)

evens :: [a] -> [a]
evens xs = [x | (x, y) <- zip xs [0..], (y `mod` 2) == 0]

odds :: [a] -> [a]
odds xs = [x | (x, y) <- zip xs [0..], (y `mod` 2) /= 0]

evensOdds :: [a] -> ([a], [a])
evensOdds [] = ([],[])
evensOdds [x] = ([x],[])
evensOdds (x:y:xs) = 
  (x:e, y:o) where
  (e, o) = evensOdds xs




--Split list
splitList xs n = ((take n xs), (drop n xs))

main = do
  putStrLn "Hello, every\nbody!"
  putStrLn (hjuster 5 "velferd")
  putStrLn (vjuster 5 "velferd")
  print (evensOdds [1, 2, 3, 4, 5, 6, 7])