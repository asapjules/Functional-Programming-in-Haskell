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
                -- the jug in the list we are watching for; checks from new array after player response
                Jug maxAmt targetAmt = (!) responseArr goalTarget
                -- determine how much water to transfer
                Jug fromJugMax fromJugAmt = (!) jugs fromJug
                Jug toJugMax toJugAmt = (!) jugs toJug
                transferAmt = transfer fromJugAmt toJugAmt toJugMax
                -- create 2 new jugs representing post-transfer values
                newFromJug = Jug fromJugMax (fromJugAmt - transferAmt)
                newToJug = Jug toJugMax (toJugAmt + transferAmt)
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

--test all possible first moves
testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testOneStep step = (do
  test2to0 <- testStep arrayOfJugs2to0 goal (nextStep (FromTo 2 0))
  test2to1 <- testStep arrayOfJugs2to1 goal (nextStep (FromTo 2 1))
  test1to0 <- testStep arrayOfJugs1to0 goal (nextStep (FromTo 1 0))
  test1to2 <- testStep arrayOfJugs1to2 goal (nextStep (FromTo 1 2))
  test0to1 <- testStep arrayOfJugs0to1 goal (nextStep (FromTo 0 1))
  test0to2 <- testStep arrayOfJugs0to2 goal (nextStep (FromTo 0 2))
  Just())
  where
  arrayOfJugs2to0 =  mkJugArray([(Jug 7 7), (Jug 4 2), (Jug 5 0)])
  arrayOfJugs2to1 =  mkJugArray([(Jug 7 5), (Jug 4 4), (Jug 5 0)])
  arrayOfJugs1to0 =  mkJugArray([(Jug 7 7), (Jug 4 0), (Jug 5 2)])
  arrayOfJugs1to2 =  mkJugArray([(Jug 7 5), (Jug 4 0), (Jug 5 4)])
  arrayOfJugs0to1 =  mkJugArray([(Jug 7 3), (Jug 4 4), (Jug 5 2)])
  arrayOfJugs0to2 =  mkJugArray([(Jug 7 2), (Jug 4 2), (Jug 5 5)])
  goal = (Goal 2 1)
  arrayOfJugs = mkJugArray [Jug 7 5, Jug 4 2, Jug 5 2]
  Step _ _ nextStep = step arrayOfJugs goal
  
-- tests whether a single step worked
testStep :: Array Int Jug -> Goal -> GameTrace () -> Maybe ()
testStep arrayOfJugs goal (Pure a) = Just a
testStep arrayOfJugs goal (Step trueJug trueGoal f) =
    if (arrayOfJugs == trueJug && goal == trueGoal) then Just ()
    else Nothing

--Tests the steps of the game and see if you get a pure in the end
-- testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
-- testUntilDone final = (do
  -- test2to0 <- testStep newarray goal (Step newarray goal f)
  -- test2to1 <- testStep finalarray goal (Step finalarray goal f)
  -- testDone <- testStep finalarray goal (Pure ())
  -- return testDone)
  -- where
  -- f = (\x-> Pure())
  -- newarray = mkJugArray([Jug 2 0, Jug 3 3, Jug 4 1])
  -- finalarray = mkJugArray [Jug 2 1, Jug 3 3, Jug 4 0]
  -- arrayOfJugs = mkJugArray([(Jug 2 0), (Jug 3 0), (Jug 4 4)])
  -- goal = (Goal 0 1)
    
testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testUntilDone game = case game arrayOfJugs goal of 
    Step jugsInitial goalInitial secondStep -> case secondStep (FromTo 2 1) of
        Step jugsSecondStep goalSecondStep finished -> case finished (FromTo 2 0) of
            Pure a -> Just ()
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
    where
    arrayOfJugs = mkJugArray [Jug 2 0, Jug 3 0, Jug 4 4]
    goal = (Goal 0 1)
    
    