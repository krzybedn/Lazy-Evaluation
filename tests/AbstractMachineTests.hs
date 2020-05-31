module AbstractMachineTests(abstractMachineTests) where


import AbstractMachine

import Asserts
import Utilities

import Test.Tasty.HUnit
import Test.Tasty


assertEqAns :: String -> Term -> IO ()
assertEqAns = assertEQ (\n -> ((parseProgram "" n) >>= (return . ansToTerm . evaluateTerm)))

assertEqVal :: String -> Val -> IO ()
assertEqVal = assertEQ (\n -> ((parseProgram "" n) >>= (return . ansToVal . evaluateTerm)))

abstractMachineTests = testGroup "Abstract Machine Tests"
  [ 
    testCase "Simple identity" $ assertEqVal 
      ("\\ x.x")
      (VLam (NVar "x" 0) (TVar $ NVar "x" 0))
  , testCase "Simple application" $ assertEqAns 
      ("(\\ x.x) _ b") 
      (TApp 
        (TLam (NVar "x" 0) 
         (TCVl "b")) 
      (TCVl "b"))
  , testCase "Nested lambda single application" $ assertEqAns
      ("(\\ x. \\ y.x) _ b1 ")
      (TApp 
        (TLam (NVar "x" 0) 
          (TLam (NVar "y" 0) $ TVar $ NVar "x" 0)) 
        (TCVl "b1"))
  , testCase "Nested lambda application" $ assertEqAns 
      ("(\\ x. \\ y.y) _ b1 _ b2")
      (TApp 
        (TLam (NVar "x" 0) 
          (TApp 
            (TLam (NVar "y" 0) 
              (TCVl "b2")) 
            (TCVl "b2")))
        (TCVl "b1"))
  , testCase "Nested lambda application" $ assertEqAns 
      ("(\\ x. \\ y. \\ z.x) _ b1 _ b2")
      (TApp 
        (TLam (NVar "x" 0) 
          (TApp 
            (TLam (NVar "y" 0) 
              (TLam (NVar "z" 0) 
                (TVar $ NVar "x" 0))) 
            (TCVl "b2"))) 
        (TCVl "b1"))
  , testCase "Lazy no calculated argument" $ assertEqAns 
      ("(\\ x. \\ y. / succ x) (/ succ _ z)")
      (TApp 
        (TLam (NVar "x" 0) 
          (TLam (NVar "y" 0) 
            (TCFn "succ" $ 
              TVar $ NVar "x" 0))) 
        (TCFn "succ" $ TCVl "z"))
  , testCase "Argument calculated by need" $ assertEqAns 
      ("(\\ x. \\ y. / succ x) (/ succ _ z) _ n")
      (TApp 
        (TLam 
          (NVar "x" 0) 
          (TApp 
            (TLam (NVar "y" 0) $ TCVl "ssz") 
            (TCVl "n"))) 
        (TCVl "sz"))
  , testCase "Lazily not calculated argument" $ assertEqAns 
      ("(\\ f. \\ y. / succ y) (/ succ _ z) _ z")
      (TApp 
        (TLam 
          (NVar "f" 0) 
          (TApp 
            (TLam (NVar "y" 0) $ TCVl "sz") 
            (TCVl "z"))) 
        (TCFn "succ" $ TCVl "z"))
  , testCase "Lazily not calculated argument" $ assertEqAns 
      ("(\\ x. (\\ y. _ z) x) (/ succ _ z)")
      (TApp 
        (TLam 
          (NVar "x" 0) 
          (TApp 
            (TLam (NVar "y" 0) $ TCVl "z") 
            (TVar $ NVar "x" 0))) 
        (TCFn "succ" $ TCVl "z"))

  ]