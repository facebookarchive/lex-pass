module Config where

import Data.List
import HSH

-- put any custom default excluded directories or aliased filetypes here etc
sourceFiles :: [String] -> String -> IO [String]
sourceFiles ftypes dir = run ("find", dir:args) where
  args = intercalate ["-or"] [["-iname", "*." ++ ftype] | ftype <- ftypes]

