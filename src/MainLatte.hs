module Main where

import           Control.Monad.State    (execState)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (hPutStrLn, stderr)

import           ErrM
import qualified Frontend.Environment   as F (initEnv)
import           Frontend.StaticChecker (checkProgram)
import           ParLatte


runProgram :: String -> IO ()
runProgram progTxt = case pProgram (myLexer progTxt) of
  Ok prog -> do
    let (_, _, e) = execState (checkProgram prog) F.initEnv
    case e of
      []     -> do
        putStrLn "StaticChecker OK"
        exitSuccess
      errors -> showErrors errors
  Bad s -> do
    hPutStrLn stderr $ "[Syntax error] " ++ s
    exitFailure
  where
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
