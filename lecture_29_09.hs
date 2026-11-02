
--Abstract syntax tree
data Ast = A Ast Ast | M Ast Ast | V Int

parseE :: [String] -> (Ast, [String])
parseE ("+":xs) = let e1 



--naturlig fold 
