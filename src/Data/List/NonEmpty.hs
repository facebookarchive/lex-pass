{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.List.NonEmpty where

import Data.Binary
import Data.Data
import qualified Data.List as L
import Text.PrettyPrint.GenericPretty

-- | A non-empty list
data NonEmpty a = NonEmpty
    { head :: a
    , tail :: [a]
    } deriving (Data, Eq, Generic, Show, Typeable)

toList :: NonEmpty a -> [a]
toList (NonEmpty x xs) = x:xs

fromList :: [a] -> Maybe (NonEmpty a)
fromList [] = Nothing
fromList (x:xs) = Just $ NonEmpty x xs

length :: NonEmpty a -> Int
length = L.length . toList

instance (Out a) => Out (NonEmpty a)
