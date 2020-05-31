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
import qualified AbstractMachine3 as AM
import qualified CheckWellFormed as WF

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

-- run :: Options -> SourceName -> IO AM.Ans
-- run _ n = do 
--     r <- parseProgramFile n
--     return $ AM.findAns $ AM.translate r

-- runProgram :: Options -> SourceName -> IO ()
-- runProgram o n = (run o n) >>= (\a -> print a)  

-- runPrograms :: Options -> [SourceName] -> IO ()
-- runPrograms opts = mapM_ $ runProgram opts


data Flag = FCheck | FHelp deriving (Eq)

options =
  [ 
    Option ['w'] ["well-formed"]      (NoArg FCheck)
      "do not run programs"
  , Option ['h'] ["help"] (NoArg FHelp) "display this list of options"
  ]
usageHeader = "Usage: Main [OPTION...] file..."
-- data Options = Options
--   { 
--     -- noTypecheck :: Bool
--     noEval      :: Bool
--   }

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


main = do
  args <- getArgs
  case parseArgs args of
    AMessage msg  -> hPutStrLn stderr msg
    ACheck fnames -> checkWellFormnes fnames
    ARun fnames   -> runPrograms fnames

parsePrograms :: [SourceName] -> IO ()
-- parsePrograms = mapM_ $ fip ((.) (>>=) parseProgramFile) print
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
      Nothing -> let term = AM.translate expr in print $ AM.getFinalAns $ AM.findAns term
      Just err -> 
        case err of
        WF.DuplicateVariable p v -> hPutStrLn stderr $ (show p ++ ("\n\tName of the variable " ++ v ++ " is not unique") )
        WF.UnknownVariable p v   -> hPutStrLn stderr $ (show p ++ ("\n\tUnknown variable name " ++ v))
  -- let term = AM.translate expr in 
  -- -- isCorrect <- 
  --   if AM.isWellFormed term then
  --     do print $ AM.getFinalAns $ AM.findAns term
  --   else
  --     do print "Program is not well formed" 

-- run2 :: SourceName -> IO ()
-- run2 fname = do
--     chars <- readFile fname
--     r <- parseProgramFile fname chars
--     print r

-- run :: SourceName -> IO ()
-- run n = do
--     r <- parseProgramFile n
--     print r

-- calcAns :: SourceName -> IO AbstractMachine3.Ans
-- calcAns n = do
--   (vs, e) <- parseProgramFile n
--   return (AbstractMachine3.tmp2 (AbstractMachine3.transform e))


--   -- AbstractMachine3.Ans c v) <- (AbstractMachine3.tmp2 (AbstractMachine3.transform e))
--   -- return $ AbstractMachine3.plug c

-- calcTerm :: SourceName -> IO AbstractMachine3.Term
-- calcTerm n = do
--   AbstractMachine3.Ans c v <- calcAns n
--   return $ AbstractMachine3.plug c $ AbstractMachine3.valToTerm v

-- calcVal :: SourceName -> IO AbstractMachine3.Val
-- calcVal n = do
--   AbstractMachine3.Ans c v <- calcAns n
--   return v

