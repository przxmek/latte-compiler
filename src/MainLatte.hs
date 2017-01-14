module Main where

  import System.Environment ( getArgs )
  import System.FilePath ( replaceExtension )
  import System.Process ( system )

  import CompilerLLVM ( compile )
  import MainCommon ( run, showHelp )


  main :: IO ()
  main = do
    args <- getArgs
    case args of
      ["--help"]  -> showHelp
      [file]      -> do
        txt <- run file compile

        let outputFilePath = replaceExtension file ".ll"
        writeFile outputFilePath txt
        putStrLn $ "Generated: " ++ outputFilePath
        -- llvm-link -o out.bc foo.bc runtime.bc
        system $ "llvm-link -v -o " ++ replaceExtension outputFilePath ".bc" ++ " " ++ outputFilePath

        return ()    
      _           -> showHelp

