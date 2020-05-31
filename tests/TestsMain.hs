-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

-- module TestsMain ( main ) where

import Test.Tasty.HUnit
import Test.Tasty

-- import qualified Distribution.TestSuite as Cabal
-- import qualified Distribution.TestSuite.HUnit as CabalHUnit

import Data.List
import Data.Ord

import AbstractMachineTests
import ParserTests
import WellFormedTests

main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [ParserTests.parserTests, AbstractMachineTests.abstractMachineTests, WellFormedTests.wellFormedTests]
tests = testGroup "Tests" [ParserTests.parserTests, AbstractMachineTests.abstractMachineTests, WellFormedTests.wellFormedTests]

-- tests = map (\(x,y) -> CabalHUnit.test x y) [("Abstract machine tests", AbstractMachineTests.abstractMachineTests)]

-- main :: IO ()
-- main = do _ <- runTestTT $ tests
--           return ()
