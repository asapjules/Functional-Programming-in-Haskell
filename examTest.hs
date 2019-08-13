module examTest where

g :: [Int] -> [b]
g [] = []
g (x:xs)
    | x==0 = case g xs of 
               [] -> [] 
               ys@(y:_) -> if y == 0 then ys else x : ys 
    | otherwise = x : g xs