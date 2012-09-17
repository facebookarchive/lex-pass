module Unparse where

type Unparser a = a -> String

class Unparse a where
  unparse :: a -> String

instance (Unparse a, Unparse b) => Unparse (a, b) where
  unparse (a, b) = unparse a ++ unparse b

instance (Unparse a) => Unparse [a] where
  unparse = concatMap unparse

instance (Unparse a) => Unparse (Maybe a) where
  unparse = maybe "" unparse

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left a) = unparse a
  unparse (Right a) = unparse a
