module Main where

import Lang.Php.Ast
import System.Directory
import System.FilePath

assertUnchanged :: String -> IO ()
assertUnchanged s = do
  let
    astMb :: Either ParseError Ast
    astMb = runParser parse () "." s
  case astMb of
    Left e -> error $ "assertUnchanged:\n" ++ show s ++ "\n ->\n" ++ show e
    Right ast -> do
      let s' = unparse ast
      when (s /= s') . error $
        "assertUnchanged:\n" ++ show s ++ "\n ->\n" ++
        show ast ++ "\n ->\n" ++ show s'

testDir = "src/Lang/Php/Ast/Test"

main = do
  assertUnchanged "<?php ''.'';"
  doesDirectoryExist testDir >>= \ e -> when e $
    mapM_ ((assertUnchanged =<<) . readFile . (testDir </>)) =<<
    filter ((/= '.') . head) <$> getDirectoryContents testDir
  putStrLn "all tests passed"

