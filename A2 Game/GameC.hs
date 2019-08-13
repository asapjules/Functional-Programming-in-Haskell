module Game where

import Control.Monad
import Data.Array

import GameDef


-- Question 1.

-- The game master for the jug game.  The parameters are the initial jug state
-- and the goal.
jugGame :: MonadGame m => Array Int Jug -> Goal -> m ()
jugGame jugs (Goal goalTarget goalAmt) = do
    response <- gmAction jugs (Goal goalTarget goalAmt)
    case response of
        FromTo fromJug toJug
            -- case where player response is out of bounds
            | fromJug > lstIndex || toJug > lstIndex -> jugGame jugs (Goal goalTarget goalAmt)
            -- case where player tries to pour from a jug to itself
            | fromJug == toJug -> jugGame jugs (Goal goalTarget goalAmt)
            -- case where the goal is met
            | targetAmt == goalAmt -> return ()
            -- all other cases where the game should continue
            | otherwise -> jugGame responseArr (Goal goalTarget goalAmt)
            where
                -- get the size of the jugs array
                (fstIndex, lstIndex) = bounds jugs
                -- the jug in the list we are watching for
                Jug targetAmt maxAmt = (!) responseArr goalTarget
                -- determine how much water to transfer
                Jug fromJugAmt fromJugMax = (!) jugs fromJug
                Jug toJugAmt toJugMax = (!) jugs toJug
                transferAmt = transfer fromJugAmt toJugAmt toJugMax
                -- create 2 new jugs representing post-transfer values
                newFromJug = Jug (fromJugAmt - transferAmt) fromJugMax
                newToJug = Jug (toJugAmt + transferAmt) toJugMax
                -- create the new array based on the response from player
                responseArr = (//) jugs [(fromJug, newFromJug), (toJug, newToJug)]

-- determines how much water to transfer
transfer :: Integer -> Integer -> Integer -> Integer
transfer fromJugAmt toJugAmt toJugMax
    | fromJugAmt <= maxTransfer = fromJugAmt
    | otherwise = maxTransfer
    where
        -- difference between destination jug's max and current capacity before any transfer
        maxTransfer = toJugMax - toJugAmt


-- Question 2.

instance Functor GameTrace where
    -- If you are confident with your Monad instance, you can just write
    fmap = liftM

instance Applicative GameTrace where
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad GameTrace where
    -- return :: a -> GameTrace a
    return a = Pure a

    -- (>>=) :: GameTrace a -> (a -> GameTrace a) -> GameTrace a
    Pure a >>= f = f a
    Step jugs goal f >>= g = Step jugs goal (\x -> ((f x) >>= g))


instance MonadGame GameTrace where
    --gmAction :: Array Int Jug -> Goal -> GameTrace PlayerMsg
    gmAction jugs goal = Step jugs goal (\x -> Pure x)


-- Question 3.

-- j0 = Jug 5 7
-- j1 = Jug 2 4
-- j2 = Jug 2 5
-- goal = Goal 2 1
-- jugs = mkJugArray [j0, j1, j2]

-- testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
-- testOneStep step = do
    -- state <- step jugs goal
    -- case state of
        -- (Pure ()) -> Just ()
        -- (Step () () ()) -> Just ()
testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testOneStep step = (do
  test2to0 <- testStep arrayOfJugs2to0 goal (Step arrayOfJugs2to0 goal f)
  test2to1 <- testStep arrayOfJugs2to1 goal (Step arrayOfJugs2to1 goal f)
  test1to0 <- testStep arrayOfJugs1to0 goal (Step arrayOfJugs1to0 goal f)
  test1to2 <- testStep arrayOfJugs1to2 goal (Step arrayOfJugs1to2 goal f)
  test0to1 <- testStep arrayOfJugs0to1 goal (Step arrayOfJugs0to1 goal f)
  test0to2 <- testStep arrayOfJugs0to2 goal (Step arrayOfJugs0to2 goal f)
  return test2to0)
  where
 
  arrayOfJugs2to0= mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  arrayOfJugs2to1 = mkJugArray([(Jug 7 7), (Jug 4 0), (Jug 5 2)])
  arrayOfJugs1to0=  mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  arrayOfJugs1to2 =  mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  arrayOfJugs0to1 =  mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  arrayOfJugs0to2 =  mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  goal = (Goal 2 1)
  f=(\x -> Pure())
  
-- tests whether a single step work 
testStep :: Array Int Jug -> Goal -> GameTrace () -> Maybe ()
testStep jugs goal (Step trueJug trueGoal f) =
    if (jugs == trueJug && goal == trueGoal) then Just ()
    else Nothing


testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testUntilDone = error "TODO"
