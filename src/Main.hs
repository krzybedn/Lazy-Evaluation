-- {-# LANGUAGE Safe #-}

import Control.Exception
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Pos

import qualified AST
import qualified Parser
import qualified AbstractMachine as AM
import qualified CheckWellFormed as WF



{-========================================
    ARGUMENTS
========================================-}

data Flag = FCheck | FHelp deriving (Eq)

options =
  [ 
    Option ['w'] ["well-formed"]      (NoArg FCheck)
      "do not run programs"
  , Option ['h'] ["help"] (NoArg FHelp) "display this list of options"
  ]
usageHeader = "Usage: Main [OPTION...] files..."

data Action
  = AMessage String
  | ACheck   [SourceName]
  | ARun     [SourceName]

parseArgs :: [String] -> Action
parseArgs args =
  case getOpt RequireOrder options args of
    (flags,fnames,[])  -> 
      if elem FHelp flags then
        AMessage $ usageInfo usageHeader options
      else if elem FCheck flags then
        ACheck fnames
      else 
        ARun fnames
    (_,_, errs)        -> AMessage $ concat errs ++ usageInfo usageHeader options



{-========================================
    MAIN
========================================-}

main = do
  args <- getArgs
  case parseArgs args of
    AMessage msg  -> hPutStrLn stderr msg
    ACheck fnames -> checkWellFormnes fnames
    ARun fnames   -> runPrograms fnames


{-========================================
    RUN CALCUALTIONS
========================================-}

parsePrograms :: [SourceName] -> IO ()
parsePrograms =  mapM_ $ (\n -> ((parseProgramFile n) >>= (\x -> print $ WF.checkWellFormed x)))

checkWellFormnes :: [SourceName] -> IO ()
checkWellFormnes =  mapM_ $ (\n -> ((parseProgramFile n) >>= (\pr -> aux pr)))
  where
    aux e = case WF.checkWellFormed e of
      Nothing -> hPutStrLn stdout $ (sourceName $ AST.getData e) ++  "\tCorrect"
      Just err -> 
        case err of
        WF.DuplicateVariable p v -> hPutStrLn stderr $ (show p ++ ("\n\tName of the variable " ++ v ++ " is not unique") )
        WF.UnknownVariable p v   -> hPutStrLn stderr $ (show p ++ ("\n\tUnknown variable name " ++ v))

runPrograms :: [SourceName] -> IO ()
runPrograms = mapM_ $ runProgram

runProgram :: SourceName -> IO () 
runProgram name = do
  expr <- parseProgramFile name
  case WF.checkWellFormed expr of
      Nothing -> 
            hPutStrLn stdout $ (sourceName $ AST.getData expr) ++ ("\t") ++ show (AM.evaluateTerm expr)
      Just err -> 
        case err of
        WF.DuplicateVariable p v -> hPutStrLn stderr $ (show p ++ ("\n\tName of the variable " ++ v ++ " is not unique") )
        WF.UnknownVariable p v   -> hPutStrLn stderr $ (show p ++ ("\n\tUnknown variable name " ++ v))


{-========================================
    RUN PARSER
========================================-}

parseProgram :: SourceName -> String -> IO (AST.Program SourcePos)
parseProgram fname chars =
  case Parser.parseProgram fname chars of
    Right p -> 
      return p
    Left error -> ioError $ userError $ show error

parseProgramFile :: SourceName -> IO (AST.Expr SourcePos)
parseProgramFile fname = do
  chars <- readFile fname
  AST.Program body <- parseProgram fname chars
  return body
