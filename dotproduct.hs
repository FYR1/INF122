

{-
dotProduct :: Num a => [a] -> [a] -> a
dotProduct ls1 ls2 = foldr (+) 0 (map (\(a, b) -> a * b) (zip ls1 ls2) )

dotProductSuperior :: Num a => [a] -> [a] -> a
dotProductSuperior ls1 ls2 = foldr (+) (zipWith (\a b -> a * b) ls1 ls2)
-}

alphabet = ['A'..'Z']

alphabetIndexed :: [(Char, Int)]
alphabetIndexed = zip alphabet [0..]

rotorI :: String
rotorI = "JGDQOXUSCAMIFRVTPNEWKBLZYH"

encrypt :: String -> String -> String -> String
encrypt [] input output = []
encrypt (x:xs) input output = output !! (indexOf x (zip input [0..])) : encrypt xs input output

--Warning! Error if char is not in list!
indexOf :: Char -> [(Char, Int)] -> Int
indexOf a (x:xs)
    | a == fst x = snd x
    | otherwise = indexOf a xs

