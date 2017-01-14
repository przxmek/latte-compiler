module MainCommon where

  import System.Exit ( exitFailure )
  import System.FilePath ( takeBaseName )

  import Parser.AbsInstant ( Program )
  import Parser.ErrM ( Err(..) )
  import Parser.ParInstant ( myLexer, pProgram )


  type CompileFunc = Program -> String -> IO String


  run :: FilePath -> CompileFunc -> IO String
  run file compile = do
    codeTxt <- readFile file
    let fileName = takeBaseName file
    let tokens = myLexer codeTxt in
        case pProgram tokens of
          Bad s -> do
            putStrLn "ERROR: Parse failed"
            putStrLn "Tokens:"
            print tokens
            putStrLn s
            putStrLn codeTxt
            
            exitFailure
          Ok program -> compile program fileName


  showHelp :: IO ()
  showHelp = do
    putStrLn $ unlines
      [ "usage: Call with one of the following argument combinations:"
      , "  --help          Display this help message."
      , "  filePath        Parse content of file."
      ]
    exitFailure
