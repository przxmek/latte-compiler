module Main where

import           Control.Monad.State    (evalState, execState)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (hPutStrLn, stderr)

import           ErrM
import qualified Frontend.Environment   as F (initEnv)
import           Frontend.StaticChecker (checkProgram)
import qualified Generator.Environment  as G (initEnv)
import           Generator.Generator    (genProgram)
import           ParLatte


runProgram :: String -> IO ()
runProgram progTxt = case pProgram (myLexer progTxt) of
  Ok prog -> do
    let (_, _, e) = staticCheck prog
    case e of
      []     -> do
        putStr $ genCode prog
        exitSuccess
      errors -> showErrors errors >> exitFailure
  Bad s -> do
    hPutStrLn stderr $ "[Syntax error] " ++ s
    exitFailure
  where
    staticCheck prog = execState (checkProgram prog) F.initEnv
    genCode prog = evalState (genProgram prog) G.initEnv


showErrors :: [String] -> IO ()
showErrors [] = return ()
showErrors (e:errs) = do
  showErrors errs
  hPutStrLn stderr e


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
