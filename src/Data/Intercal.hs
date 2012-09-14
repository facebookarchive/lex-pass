{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Intercal where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Data
import Prelude hiding (concatMap, map)
import qualified Prelude

import Common
import Parse
import Unparse
import Text.PrettyPrint.GenericPretty

data Intercal a b = Intercal a b (Intercal a b) | Interend a
  deriving (Data, Eq, Generic, Show, Typeable)

-- we're using method that should be faster-but-bigger instead of storing
-- length.  this is probably the same as the derive one, just use that?
instance (Binary a, Binary b) => Binary (Intercal a b) where
  put (Intercal x y r) = put (0 :: Word8) >> put x >> put y >> put r
  put (Interend x)     = put (1 :: Word8) >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM3 Intercal get get get
      1 -> liftM  Interend get

intercalParser :: Parser a -> Parser b -> Parser (Intercal a b)
intercalParser a b = do
  aRes <- a
  bResMb <- optionMaybe b
  case bResMb of
    Nothing -> return $ Interend aRes
    Just bRes -> liftM (Intercal aRes bRes) $ intercalParser a b

instance (Parse a, Parse b) => Parse (Intercal a b) where
  parse = intercalParser parse parse

intercalUnparser :: (a -> String) -> (b -> String) -> Intercal a b -> String
intercalUnparser f g i =
  Prelude.concatMap (\ (a, b) -> f a ++ g b) xInit ++ f xEnd where
  (xInit, xEnd) = breakEnd i

instance (Unparse a, Unparse b) => Unparse (Intercal a b) where
  unparse = intercalUnparser unparse unparse

concatMapM :: (Monad m) => (a -> b -> a -> m (Intercal a b)) ->
  Intercal a b -> m (Intercal a b)
concatMapM _f i@(Interend _) = return i
concatMapM f (Intercal x1 y (Interend x2)) = f x1 y x2
concatMapM f (Intercal x1 y (Intercal x2 y2 rest)) = do
  (fResMain, fResEnd) <- liftM breakEnd $ f x1 y x2
  liftM (prepend fResMain) . concatMapM f $ Intercal fResEnd y2 rest

concatMap :: (a -> b -> a -> Intercal a b) -> Intercal a b -> Intercal a b
concatMap _f i@(Interend _) = i
concatMap f (Intercal x1 y (Interend x2)) = f x1 y x2
concatMap f (Intercal x1 y (Intercal x2 y2 rest)) =
  prepend fResMain . concatMap f $ Intercal fResEnd y2 rest
  where
  (fResMain, fResEnd) = breakEnd $ f x1 y x2

prepend :: [(a, b)] -> Intercal a b -> Intercal a b
prepend [] = id
prepend ((x, y):rest) = Intercal x y . prepend rest

breakEnd :: Intercal a b -> ([(a, b)], a)
breakEnd (Interend x) = ([], x)
breakEnd (Intercal x y rest) = first ((x, y):) $ breakEnd rest

unbreakEnd :: [(a, b)] -> a -> Intercal a b
unbreakEnd [] xEnd = Interend xEnd
unbreakEnd ((x, y):xys) xEnd = Intercal x y $ unbreakEnd xys xEnd

rePairStart :: a -> [(b, a)] -> ([(a, b)], a)
rePairStart x0 [] = ([], x0)
rePairStart x0 ((y, x):yxs) = first ((x0, y):) $ rePairStart x yxs

rePairEnd :: [(a, b)] -> a -> (a, [(b, a)])
rePairEnd [] xEnd = (xEnd, [])
rePairEnd ((x, y):xys) xEnd = (x, (y, xys0):xysRest) where
  (xys0, xysRest) = rePairEnd xys xEnd

breakStart :: Intercal a b -> (a, [(b, a)])
breakStart = uncurry rePairEnd . breakEnd

unbreakStart :: a -> [(b, a)] -> Intercal a b
unbreakStart = (uncurry unbreakEnd .) . rePairStart

map :: (a -> c) -> (b -> d) -> Intercal a b -> Intercal c d
map f _g (Interend x) = Interend (f x)
map f  g (Intercal x y rest) = Intercal (f x) (g y) (map f g rest)

mapA :: (Applicative m) => (a -> m c) -> (b -> m d) -> Intercal a b ->
  m (Intercal c d)
mapA f _g (Interend x) = liftA Interend (f x)
mapA f  g (Intercal x y rest) = liftA3 Intercal (f x) (g y) (mapA f g rest)

-- this is a singleton "in b" and you would just use Interend if you wanted a
-- singleton "in a"
singleton :: a -> b -> a -> Intercal a b
singleton a1 b a2 = Intercal a1 b $ Interend a2

append :: a -> b -> Intercal b a -> Intercal b a
append a b (Interend b0) = Intercal b0 a $ Interend b
append a b (Intercal b0 a0 rest) = Intercal b0 a0 $ append a b rest

instance (Out a, Out b) => Out (Intercal a b)
