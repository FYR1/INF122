
--Oppgave 1
strInt :: [String] -> [Int]
strInt [] = []
strInt (x:xs) = ((read x :: Int):(strInt xs))

--Oppgave 2 (listekomprehensjon og rekursjon og filter)
fjernL :: String -> Char -> String
fjernL str c = [x | x <- str, x /= c]

fjernR :: String -> Char -> String
fjernR [] c = []
fjernR (x:xs) c 
    | x == c    = fjernR xs c
    | otherwise = (x:(fjernR xs c))

fjernF :: String -> Char -> String

foldr :: (v -> b) -> v -> [b] -> 

