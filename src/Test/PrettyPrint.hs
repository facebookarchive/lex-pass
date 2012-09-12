{-# LANGUAGE DeriveDataTypeable #-}

module PrettyPrint where

import Control.Applicative
import Data.Generics
import Data.List.Split
import Data.Tree

{-
data Bar = Bar Int deriving (Data, Typeable)

data Baz = Baz Int deriving (Data, Typeable)

data FooD = Foo {
  foo1 :: Bar,
  foo2 :: Baz} deriving (Data, Typeable)

data Foo2D = Foo2 Bar Baz deriving (Data, Typeable)

lol = Foo (Bar 4) (Baz 5)

lol2 = Foo2 (Bar 4) (Baz 5)
-}

pp4 :: Data a => a -> Tree (String, [String])
pp4 var = Node (show c, fields) $ gmapQ pp4 var
  where
  c = toConstr var
  fields = if isAlgType $ dataTypeOf var then constrFields c else []

pp3 :: Data a => a -> Tree String
pp3 = treeMergeSndToNextFst (\ fieldNames constrReps ->
  zipWith (++) (map (++ "=") fieldNames ++ repeat "") constrReps) . pp4

pp2 :: Data a => a -> String
pp2 = unlines . map head . chunksOf 2 . lines . drawTree . pp3

pp :: Data a => a -> IO ()
pp = putStr . pp2

treeMergeSndToNextFst :: (b -> [a] -> [a]) -> Tree (a, b) -> Tree a
treeMergeSndToNextFst f (Node (a, b) kids) =
  Node a $ zipWith Node kidAs' forests
  where
  kidAs' = f b kidAs
  (kidAs, forests) = unzip $ map (treeToPair . treeMergeSndToNextFst f) kids

treeToPair :: Tree a -> (a, Forest a)
treeToPair (Node a kids) = (a, kids)
