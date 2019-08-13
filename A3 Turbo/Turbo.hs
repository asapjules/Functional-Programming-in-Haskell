module Turbo where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map

import           TurboDef


-- "Run" a Turbo program to produce SVG path commands.
-- This is the only function that will be tested.
runTurbo :: Stmt -> [SVGPathCmd]
runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
-- But the actual execution engine and recursion are in "turbo" and "evalReal"
-- below.
 

-- Evaluate an expression. The State monad is needed to look up variables.
evalReal :: RealExpr -> State TurboMem Double
evalReal expr = do
  case expr of 
      RLit const -> return const
      RVar var -> (getVar var)
      Neg expr -> evalReal expr >>= (\x -> return (-x))
      expr1 :+ expr2 -> evalReal expr1 >>= (\x -> evalReal expr2 >>= (\y -> return (x + y)))
      expr1 :- expr2 -> evalReal expr1 >>= (\x -> evalReal expr2 >>= (\y -> return (x - y)))
      expr1 :* expr2 -> evalReal expr1 >>= (\x -> evalReal expr2 >>= (\y -> return (x * y)))
      expr1 :/ expr2 -> evalReal expr1 >>= (\x -> evalReal expr2 >>= (\y -> return (x / y)))

-- Run a Turbo statement. Use the State monad to keep state. Return SVG path
-- commands.
turbo :: Stmt -> State TurboMem [SVGPathCmd]

turbo (var := expr) = case expr of (RLit val) -> setVar var val >>= (\x -> return [])
                                   (RVar var2) -> evalReal (RVar var2) >>= (\x -> setVar var x >>= (\y -> return []))
                                   otherwise -> evalReal expr >>= (\x -> setVar var x >>= (\_ -> return []))

turbo PenDown = do
  setPen True 
  return []
  
turbo PenUp = do 
  setPen False
  return []
  
turbo (Seq (x:xs)) = turbo x >>= (\x -> turbo (Seq xs) >>= (\y -> return (x ++ y)))
turbo (Seq []) = do 
  return []

  
turbo (Turn expr) = evalReal expr >>= (\x -> turn x >>= (\_ -> return []))

turbo (Forward val) = evalReal val >>= (
    \x -> getAngle >>= (\dir -> return (x*(cos (dir*pi/180))) >>= (
        \forward -> return (x*(sin (dir*pi/180))) >>= (\upward -> getPen >>= (
            \penState -> case penState of True -> return [LineTo (forward) (upward)]
                                          False -> return [MoveTo (forward) (upward)])))))

turbo (For var expr1 expr2 stmt)
  | expr1 == expr2 = turbo (Seq stmt)
  | otherwise = evalReal expr1 >>= (\x -> setVar var x >>= (
    \_ -> turbo (Seq stmt) >>= (\cmd1 -> (turbo (For var (RLit (x+1)) expr2 stmt) >>= (
        \cmd2 -> return( cmd1 ++ cmd2))))))

-- Turbo state:
-- * dictionary of variables->values
-- * current direction (degrees away from x-axis, counterclockwise, e.g.,
--   0 points west, 90 points north)
-- * pen state (True means touching paper)
data TurboMem = TurboMem (Map String Double) Double Bool
    deriving (Eq, Show)

-- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
initTurboMem = TurboMem Map.empty 0 False

-- If you could code up the following helpers, your "turbo" implementation could
-- be pretty clean and easy to follow.  fmap, get, modify, Map.lookup, and
-- Map.insert will get you a long way.

-- Get current direction.
getAngle :: State TurboMem Double
getAngle = do
  TurboMem vars dir penState <- get
  return dir

    
-- Change direction by adding the given angle.
turn :: Double -> State TurboMem ()
turn angle = do
  t <- modify (\(TurboMem vars dir penState) -> TurboMem vars (change dir) penState)
  return t
  where
    change dir = doubleMod (angle + dir) 360

doubleMod :: Double -> Double -> Double
doubleMod a b
  | a < b = a
  | otherwise = doubleMod (a - b) b

-- Get pen state.
getPen :: State TurboMem Bool
getPen = do 
  TurboMem vars dir penState <- get
  return penState

-- -- Set pen state.
setPen :: Bool -> State TurboMem ()
setPen stateBool = do
   t <- modify (\(TurboMem vars dir penState) -> TurboMem vars dir stateBool)
   return t

-- Get a variable's current value.
getVar :: String -> State TurboMem Double
getVar var = do
  TurboMem vars angle penState <- get
  case ((Map.lookup) var vars) of 
      Nothing -> return 0
      Just output -> return output


-- Set a variable to value.
setVar :: String -> Double -> State TurboMem ()
setVar var val = do
  t <- modify (\(TurboMem vars dir penState) -> TurboMem (insert vars) dir penState)
  return t
  where
    insert = \vars -> Map.insert var val vars
   