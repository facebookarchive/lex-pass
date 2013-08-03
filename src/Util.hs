module Util where

import Control.Arrow
import Control.Monad.Error

type CanErrStrIO a = ErrorT String IO a

rePairRight :: ((a, b), c) -> (a, (b, c))
rePairRight ((a, b), c) = (a, (b, c))

rePairLeft :: (a, (b, c)) -> ((a, b), c)
rePairLeft (a, (b, c)) = ((a, b), c)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons [] = error "uncons: empty list"

bothond :: (Arrow a) => a b c -> a (b, b) (c, c)
bothond f = f *** f

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs

-- Reversify (work from end instead of beginning) a function.
reversify :: ([a] -> [a]) -> [a] -> [a]
reversify f = reverse . f . reverse

-- Reversify (work from end instead of beginning) a function that makes a tuple
-- (such as span).
reversifyTup :: ([a] -> ([b], [b])) -> [a] -> ([b], [b])
reversifyTup f = swap . bothond reverse . f . reverse
