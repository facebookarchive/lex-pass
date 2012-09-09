module Config where

import Data.List
import HSH

-- put any custom default excluded directories or aliased filetypes here etc
sourceFiles :: [String] -> [String] -> String -> IO [String]
sourceFiles ftypes exlcDirs dir = run (findCommand ftypes exlcDirs dir)

findCommand :: [String] -> [String] -> String -> (String, [String])
findCommand ftypes exclDirs dir = ("find", allArgs) where
  allArgs = dir:(exclArgs ++ inclArgs ++ ["-print"])
  inclArgs = orArgs [["-iname", "*." ++ ftype] | ftype <- ftypes]
  exclArgs = if exclDirs /= []
	     then (paren . orArgs $ [["-wholename", dir] | dir <- exclDirs]) ++ ["-prune", "-o"]
             else []
  paren args = ["("] ++ args ++ [")"]
  orArgs = intercalate ["-or"]
