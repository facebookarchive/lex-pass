module Lang.Php.Ast.LexWS where

import Common

tokStartComment = "/*"
tokStartCommentP = try $ string tokStartComment
tokLineComment = "//";
tokLineCommentP = try $ string tokLineComment
tokEndComment = "*/"
tokEndCommentP = try $ string tokEndComment
tokPound = "#"
tokPoundP :: Parser String
tokPoundP = string tokPound

upToCharsParser c1 c2 = do
  (gotChars, r) <- upToCharsOrEndParser (const True) c1 c2
  if gotChars then return r
    else fail $ "Unexpected <eof>, expecting " ++ [c1, c2] ++ "."

upToCharsOrEndParser f c1 c2 = do
  s <- many (satisfy (\ x -> x /= c1 && f x))
  r1Mb <- optionMaybe (char c1)
  second (s ++) <$> case r1Mb of
    Nothing -> return (False, "")
    Just _ -> upToCharsOrEndParserC2 f c1 c2

upToCharsOrEndParserC2 f c1 c2 = do
  r2Mb <- optionMaybe $ satisfy f
  case r2Mb of
    Nothing -> return (False, [c1])
    Just r2 -> if r2 == c2
      then return (True, "")
      else second (c1:) <$> if r2 == c1
        then upToCharsOrEndParserC2 f c1 c2
        else second (r2:) <$> upToCharsOrEndParser f c1 c2
