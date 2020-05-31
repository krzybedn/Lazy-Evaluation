module WellFormedTests(wellFormedTests) where


import CheckWellFormed 

import Asserts
import Utilities

import Test.Tasty.HUnit
import Test.Tasty
import Text.Parsec.Pos


assertEqAns :: String -> Maybe FormnesError -> IO () 
assertEqAns = assertEQ (\n -> (parseProgram "" n) >>= (return . checkWellFormed))

wellFormedTests = testGroup "Well-formed checking tests"
  [ 
    testCase "Simple correct program" $ assertEqAns
      ("\\ x.x") 
      Nothing
  , testCase "Simple program with unknown variable" $ assertEqAns
      ("\\ x.y") 
      (Just $ UnknownVariable (newPos "" 1 5) "y")
  , testCase "Simple program with duplicate variable inside lambda" $ assertEqAns
      ("\\ x. \\ x.x") 
      (Just $ DuplicateVariable (newPos "" 1 6) "x")
  , testCase "Duplicate variable deep inside lambda" $ assertEqAns
      ("(\\ q. \\ x1. \\ x2. \\ x3. \\ x4. \\ x5. \\ q.q)") 
      (Just $ DuplicateVariable (newPos "" 1 37) "q")
  , testCase "Simple program with duplicate variable inside application" $ assertEqAns
      ("(\\ x.x) \\ x.x") 
      Nothing
  , testCase "Duplicate variable deep inside application" $ assertEqAns
      ("(\\ x1. \\ x2. \\ x3. \\ x4. \\ q.q) (\\ y1. \\ y2. \\ y3. \\ y4. \\ q.q)") 
      Nothing
  , testCase "Constant values can have same names as variables" $ assertEqAns
      ("(\\ x.x) _ x") 
      Nothing
  , testCase "Constant functions can have same names as variables" $ assertEqAns
      ("(\\ x. / x x) _ x") 
      Nothing
  ]