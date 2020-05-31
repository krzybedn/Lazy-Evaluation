{-# LANGUAGE Safe #-}
module Utilities(parseProgram) where

import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Pos

import AST
import qualified Parser as Pars
-- import qualified AbstractMachine3 as AM
-- import qualified CheckWellFormed as WF

-- parseProgram :: SourceName -> String -> IO (AST.Program SourcePos)
-- parseProgram fname chars =
--   case Parser.parseProgram fname chars of
--     Right p -> 
--       return p
--     Left error -> ioError $ userError $ show error

parseProgram :: SourceName -> String -> IO (Expr SourcePos)
parseProgram fname chars =
  case Pars.parseProgram fname chars of
    Right (Program body) -> return body
    Left error -> ioError $ userError $ show error