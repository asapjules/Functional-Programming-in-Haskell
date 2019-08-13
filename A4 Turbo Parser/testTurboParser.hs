-- How to use: runghc testTurboParser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import ParserLib
import TurboDef
import TurboParser (mainParser)

parse :: String -> Maybe Stmt
parse = runParser mainParser

tests =
    [ testHandout
    , testJunkAfter, testNoJunkAfter, testForward, testAssign, testPenUp, testList, testAssign2, testForward2, testTurn, testListCarlos, testExpr
    ]
-- more test cases when marking

testHandout =
    "handout" ~: parse inp
    ~?= Just (For "i" (RLit 0) (RLit 60)
              [ Forward (RVar "s")
              , "s" := RVar "s" :* RLit 0.99
              , Turn (RVar "i" :* RLit 0.8)
              ])
  where
    inp = "\nfor  i=0 to  60  {\n  forward s;  s   =    s  *  0.99  ;turn   i*0.8;\n  }\n"

testJunkAfter = "junk after" ~: parse "pendown ( " ~?= Nothing
testNoJunkAfter = "no junk after" ~: parse "pendown" ~?= Just(PenDown)
testPenUp = "Penup" ~: parse "penup" ~?= Just(PenUp)
testTurn = "turn" ~: parse "turn 4.0" ~?= Just(Turn (RLit 4.0))
testForward = "forward" ~: parse "forward 4.0 + 5.0" ~?= Just(Forward (RLit 4.0 :+ RLit 5.0))
testForward2 = "forward2" ~: parse "forward s" ~?= Just(Forward (RVar "s"))
testAssign = "Assign" ~: parse "i = 0.99" ~?= Just("i" := RLit 0.99)
testAssign2 = "Assign2" ~: parse "s   =    s  *  0.99" ~?= Just("s" := RVar "s" :* RLit 0.99)
testList = "seq" ~: parse "{\nforward 0.99; turn 0.99;\n}\n" ~?= Just(Seq [Forward (RLit 0.99), Turn (RLit 0.99)])
testExpr = "complicated expr" ~: parse inp ~?= Just answer
  where
    inp = "v = 1.5 + 2.25 / (cx1 - cx2) * - -dy2 - 3"
    answer = "v" := (RLit 1.5 :+ (RLit 2.25 :/ (RVar "cx1" :- RVar "cx2"))
                                 :* Neg (Neg (RVar "dy2")))
                    :- RLit 3.0

testListCarlos = "seqCarlos" ~: parse "{x = 5.0; y = 9.0;}" ~?= Just(Seq ["x" := (RLit 5), "y" := (RLit 9)])

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)