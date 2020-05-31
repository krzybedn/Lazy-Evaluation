module ParserTests(parserTests) where

import AST

import Asserts
import Utilities

import Test.Tasty.HUnit
import Test.Tasty


assertEqualPrograms ::  IO (Expr p) -> Expr b -> IO ()
assertEqualPrograms lhs rhs_e = do
  lhs_e <- lhs
  if (dropData lhs_e) == (dropData rhs_e) 
    then 
      return ()
    else 
      assertFailure $ "Expected program: " ++ show lhs_e ++ " but got: " ++ show rhs_e


parserTests = testGroup "Parser Tests"
  [ 
    testCase "Simple identity" $
      assertEqualPrograms (parseProgram "" "\\ x.x") 
      (ELam () ("x") (EVar () "x"))
  , testCase "No body in lambda" $
      assertIOError (uncurry parseProgram) ("", "\\ x.")
  , testCase "No variable in lambda" $
      assertIOError (uncurry parseProgram) ("", "\\ y.y \\ .y")

  , testCase "Simple identity with brackets" $
      assertEqualPrograms (parseProgram "" "((((((  (( ((\\ x.x)))) )) ))))") 
      (ELam () ("x") (EVar () "x"))
  , testCase "Simple identity with unbalanced brackets on left" $
      assertIOError (uncurry parseProgram) ("", "(((\\ x.x))")
  , testCase "Simple identity with unbalanced brackets on right" $
      assertIOError (uncurry parseProgram) ("", "((\\ x.x)))")
  , testCase "Simple identity with brackets2" $
      assertEqualPrograms (parseProgram "" "\\ x.(( ((x)) ))") 
      (ELam () ("x") (EVar () "x"))
  , testCase "Simple identity with unbalanced brackets2 on left" $
      assertIOError (uncurry parseProgram) ("", "\\ x.((( ((x)) ))")
  , testCase "Simple identity with unbalanced brackets2 on right" $
      assertIOError (uncurry parseProgram) ("", "\\ x.(( ((x)) ) ))")
  , testCase "Simple identity with brackets3" $
      assertEqualPrograms (parseProgram "" "(((  ((( ((\\ x.(( ((x)) ))))  ))) )))") 
      (ELam () ("x") (EVar () "x"))

  , testCase "Simple application" $
      assertEqualPrograms (parseProgram "" "(\\ x.x) y") 
      (EApp () (ELam () ("x") (EVar () "x")) (EVar () "y"))
  , testCase "Simple application with brackets" $
      assertEqualPrograms (parseProgram "" "(\\ x.x) ( ((( ( (y)  ) )) ))") 
      (EApp () (ELam () ("x") (EVar () "x")) (EVar () "y"))

  , testCase "Simple const value" $
      assertEqualPrograms (parseProgram "" "_ b") 
      (ECVl () "b")
  , testCase "Simple const value in lambda" $
      assertEqualPrograms (parseProgram "" "\\ x. _ b") 
      (ELam () "x" (ECVl () "b"))
  , testCase "Simple const value in aplication" $
      assertEqualPrograms (parseProgram "" "(\\ x. x) _ b") 
      (EApp () (ELam () "x" (EVar () "x")) (ECVl () "b"))

  , testCase "Simple const function" $
      assertEqualPrograms (parseProgram "" "/ f x") 
      (ECFn () "f" (EVar () "x"))
  , testCase "Simple const function without argument" $
      assertIOError (uncurry parseProgram) ("", "/ f")

  , testCase "Nested lambdas" $
      assertEqualPrograms (parseProgram "" "\\ x1. \\ x2. \\ x3. \\ x4. \\ x5.x1") 
      (ELam () ("x1") (ELam () ("x2") (ELam () ("x3") (ELam () ("x4") (ELam () ("x5") (EVar () "x1"))))))

  , testCase "Application of lambda to lambda" $
      assertEqualPrograms (parseProgram "" "(\\ x . x) (\\ x . x)") 
      (EApp () (ELam () ("x") (EVar () "x")) (ELam () ("x") (EVar () "x")))
  , testCase "Application of lambda to lambda2" $
      assertEqualPrograms (parseProgram "" "(\\ x . \\ y . x y) (\\ x . x)") 
      (EApp () (ELam () ("x") (ELam () ("y") (EApp () (EVar () "x") (EVar () "y"))))
                        (ELam () ("x") (EVar () "x")))

  , testCase "Application to nested lambdas" $
      assertEqualPrograms (parseProgram "" "(\\ x1. \\ x2. \\ x3. \\ x4. \\ x5.i0) _ b1 _ b2 _ b3 _ b4 _ b5") 
        (EApp () (EApp () (EApp () (EApp () (EApp () (
          ELam () ("x1") (ELam () ("x2") (ELam () ("x3") (ELam () ("x4") (ELam () ("x5") (EVar () "i0"))))))
                  (ECVl () "b1"))
                (ECVl () "b2"))
              (ECVl () "b3"))
            (ECVl () "b4"))
          (ECVl () "b5"))
  , testCase "Application to nested constant functions" $
      assertEqualPrograms (parseProgram "" "/ f / f / f _ b") 
        (ECFn () "f" (ECFn () "f" (ECFn () "f" (ECVl () "b"))))

  -- , testCase "NAME" $
  --     assertNoError (uncurry parseProgram2) ("", "\\ x.")
  ]

