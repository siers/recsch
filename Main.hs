module Main where

import Data.List ( intercalate, nub )
import Data.Fix (Fix(..))
import Data.Functor.Base ( ListF(Cons, Nil), TreeF(..) )
import Data.Functor.Foldable ( Corecursive(ana), hylo, cata )

halves :: Int -> [Int]
halves = ana (\n -> if n == 0 then Nil else Cons n (div n 2))

type Tree a = Fix (TreeF a)

split :: Int -> Tree Int
split = ana splitF

splitF :: Int -> TreeF Int Int
splitF n = NodeF n (if n > 1 then [div n 2, div n 2] else [])

showTree :: Show a => Tree a -> String
showTree (Fix (NodeF n ns)) =
  show n ++ drop (spaceLength - 1) (formatSubtree (map showTree ns))
  where
    spaceLength = length (show n) + 1
    formatSubtree :: [String] -> String
    formatSubtree = concatMap (unlines . prefixLines . lines)

    prefixLines :: [String] -> [String]
    prefixLines [] = []
    prefixLines (a:rest) = prefixWith "` " a : map (prefixWith "| ") rest

    prefixWith :: String -> String -> String
    prefixWith s = ((replicate spaceLength ' ' ++ s) ++)

showTreeF :: Show a => TreeF a String -> String
showTreeF (NodeF n subtrees) =
  show n ++ drop (spaceLength - 1) (formatSubtree subtrees)
  where
    spaceLength = length (show n) + 1
    formatSubtree :: [String] -> String
    formatSubtree = concatMap (unlines . prefixLines . lines)

    prefixLines :: [String] -> [String]
    prefixLines [] = []
    prefixLines (a:rest) = prefixWith "` " a : map (prefixWith "| ") rest

    prefixWith :: String -> String -> String
    prefixWith s = ((replicate spaceLength ' ' ++ s) ++)

waysToFold :: [String]
waysToFold =
  [ showTree (split 16)
  , hylo showTreeF splitF 16
  , cata showTreeF (split 16)
  ]

main :: IO ()
main = do
  putStr (hylo showTreeF splitF 16)
  print (length (nub waysToFold) == 1)
  print (halves 256)

{-
16 ` 8 ` 4 ` 2 ` 1
   |   |   |   ` 1
   |   |   ` 2 ` 1
   |   |   |   ` 1
   |   ` 4 ` 2 ` 1
   |   |   |   ` 1
   |   |   ` 2 ` 1
   |   |   |   ` 1
   ` 8 ` 4 ` 2 ` 1
   |   |   |   ` 1
   |   |   ` 2 ` 1
   |   |   |   ` 1
   |   ` 4 ` 2 ` 1
   |   |   |   ` 1
   |   |   ` 2 ` 1
   |   |   |   ` 1
True
[256,128,64,32,16,8,4,2,1]
-}
