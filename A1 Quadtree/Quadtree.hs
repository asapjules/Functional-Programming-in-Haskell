module Quadtree where

import Data.Array
import Data.Word

import QuadtreeDef

quadtreeToPic :: Quadtree -> Array (Int, Int) Word8

--use a helper function to reduce complexity
quadtreeToPic (QNode x y width average children) = array ((x,y),(x+width-1, y+width-1)) (listMaker (QNode x y width average children))

listMaker :: Quadtree -> [((Int, Int), Word8)]
-- we check if the depth of the tree is equal to one or four with pattern matching 
listMaker (QNode x y width average Q0) = assocs(array ((x,y),(x+width-1,y+width-1)) [((i,j),average)|i <- [x..x+width-1], j <- [y..y+width-1]])
-- if we get a pattern of 4 children, then we recursively call the helper until we get only cases of no children 
-- we do this so that we can go through each quadtree with multiple children
listMaker (QNode x y width average (Q4 a b c d)) = pic
    where
    aOutput = listMaker a
    bOutput = listMaker b
    cOutput = listMaker c
    dOutput = listMaker d
    -- we concatinate the lists together to create a width by width array as needed.  
    pic = aOutput ++ bOutput ++ cOutput ++ dOutput

picToQuadtree :: Word8                    -- threshold
              -> Int                      -- depth cap
              -> Array (Int, Int) Word8   -- image
              -> Quadtree
--function takes in 3 inputs
picToQuadtree threshold depth masterArray
{- three cases to consider here 
1. if the depth reaches zero then we don't divide more 
2. if the max difference between the largest item and the smallest item in the array is not greater than the threshold then you don't divide further
3. if neither of those conditions are true then you need to divide it into 4 more children
-}
    | depth == 0 = (QNode i j width average Q0)
    | diff <= threshold = (QNode i j width average Q0)
    | depth > 0 && diff > threshold = (QNode i j width average (Q4 (picToQuadtree threshold (depth-1) topLeft) (picToQuadtree threshold (depth-1) bottomLeft) (picToQuadtree threshold (depth-1) topRight) (picToQuadtree threshold (depth-1) bottomRight)))
    {- if we needed to split the array further then we create the 4 regions of the array with ixmap-}
    where
    endOfQuad = div width 2
    topLeft = ixmap ((i,j), (((i + endOfQuad)-1), j+(endOfQuad-1))) (\i -> (fst i, snd i)) masterArray

    bottomLeft = ixmap ((i,j+endOfQuad),((i + endOfQuad-1),m)) (\i -> (fst i, snd i)) masterArray

    topRight = ixmap ((i+ endOfQuad,j),(k, j + endOfQuad-1)) (\i -> (fst i, snd i)) masterArray

    bottomRight = ixmap ((i + endOfQuad , j+ endOfQuad),(k, m)) (\i -> (fst i, snd i)) masterArray
{- here we set all the necessary variables to perform the calculations, i, j, k, m, width diff and average
1. i,j,k and m are the bound of the array that we are working with currently 
2. width is the size of the array 
3. average is the total of all the values in the current array block divided by the square of the width which is always half the number of items in the array block
-}
    ((i,j), (k,m)) = bounds masterArray
    width = (k-i+1)
    diff = (maximum masterArray) - (minimum masterArray)
    average = round (toRational(foldr (+) 0 masterArray) / toRational (width*width))