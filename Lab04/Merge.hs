module Merge where

-- There are other questions in other files.

{-
[4 marks]
Take two lists of integers.  Precondition: Each is already sorted (non-decreasing).
Perform the "merge" of "mergesort".  Linear time.

Example:
merge [2, 3, 5] [1, 3, 4] = [1, 2, 3, 3, 4, 5]
-}
merge :: [Integer] -> [Integer] -> [Integer]
-- merge first second
    -- | first!!1 >= second!!1 = return ++ [first!!1]; drop 1 first
    -- | second!!1 > first!!1 = reutrn  ++ [first!!1]	
merge [] [] = []

merge (a:ar) [] = (a:ar)
merge [] (b:br) = (b:br)
merge (a:ar) (b:br)
    | a <= b = [a] ++ (merge ar (b:br))
    | b <= a = [b] ++ (merge (a:ar) br)