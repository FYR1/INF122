


poly :: [Int] -> Int -> Int
poly cs x = foldr calc 0 (zip (reverse cs) [0..])
    where calc :: (Int, Int) -> Int -> Int
          calc (c, d) s = c*x^d + s


{-
4. Bruk enkel foldl eller foldr for å programmer funksjon
kompress :: (Eq a) => [a] -> [a]
som fjerner påfølgende duplikater fra an liste, f.eks.:
kompress "aababbccc" = "ababc"
kompress [1,1,1,2,2,2] = [1,2]
-}

kompress :: (Eq a) => [a] -> [a]

kompress = foldr miniCompress [] 
    where miniCompress :: Eq a => a -> [a] -> [a]
          miniCompress e (x:xs) | x == e = (x:xs) 
                                | otherwise = e:(x:xs)
          miniCompress e [] = [e]

kompress2 :: (Eq a) => [a] -> [a]


