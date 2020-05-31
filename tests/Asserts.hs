module Asserts where

import System.IO.Error
import Test.Tasty.HUnit

assertEQ :: (Show b, Eq b) => (a -> IO b) -> a -> b -> IO ()
assertEQ f arg exp = do
    res <- f arg
    if res == exp
        then return ()
        else assertFailure $ "Expected " ++ show exp ++ ", got " ++ show res

assertNoError :: (Show b) => (a -> IO b) -> a -> IO ()
assertNoError f arg = do
    res <- tryIOError (f arg)
    case res of
        Left err -> assertFailure $ "Expected no error, got " ++ show err
        Right _ -> return ()



assertIOError :: (Show b) => (a -> IO b) -> a -> IO ()
assertIOError f arg = do
    res <- tryIOError (f arg)
    case res of
        Left err -> if isUserErrorType $ ioeGetErrorType err
                    then return () 
                    else assertFailure $ "Expected IOError, got " ++ show err
        Right _ -> assertFailure "Expected IOError, got correct program."


assertIOErrorMsg :: (Show b) => (a -> IO b) -> a -> String -> IO ()
assertIOErrorMsg f arg str = do
    res <- tryIOError (f arg)
    case res of
        Left err -> if isUserErrorType $ ioeGetErrorType err 
                    then if ioeGetErrorString err == str 
                        then return ()
                        else assertFailure $ "Expected " ++ show str ++ ", got " ++ show err
                    else assertFailure $ "Expected IOError, got " ++ show err
        Right _ -> assertFailure "Expected IOError, got correct program."
