module Main where

import           Control.Monad.State (execState)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPutStrLn, stderr)

import           Environment
import           ErrM
import           ParLatte
import           StaticChecker       (checkProgram)


runProgram :: String -> IO ()
runProgram progTxt = case pProgram (myLexer progTxt) of
  Ok prog -> case getErrors (execState (checkProgram prog) initEnv) of
    []     -> do
      putStrLn "StaticChecker OK"
      exitSuccess
    errors -> showErrors errors
  Bad s -> do
    hPutStrLn stderr $ "[Syntax error] " ++ s
    exitFailure


getErrors :: Environment -> [ErrorMsg]
getErrors env = let (_, _, e) = env in e


showErrors :: [ErrorMsg] -> IO ()
showErrors [] = return ()
showErrors (e:errors) = do
  showErrors errors
  hPutStrLn stderr e
  exitFailure


showHelp :: IO ()
showHelp = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  filePath        Parse content of file."
    ]
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    [file]     -> readFile file >>= runProgram
    []         -> getContents >>= runProgram
    _          -> showHelp
