module Main where

import Data.List ( intercalate )
import Data.Fix (Fix(..))
import Data.Functor.Base ( ListF(Cons, Nil), TreeF(..) )
import Data.Functor.Foldable ( Corecursive(ana), cata )

halves :: Int -> [Int]
halves = ana (\n -> if n == 0 then Nil else Cons n (div n 2))

type Tree a = Fix (TreeF a)

split :: Int -> Tree Int
split = ana go
  where
    go :: Int -> TreeF Int Int
    go n = NodeF n (if n > 1 then [div n 2, div n 2] else [])

showTree :: Show a => Tree a -> String
showTree (Fix (NodeF n ns)) =
  show n ++ drop (spaceLength - 1) (formatSubtree (map showTree ns))
  where
    spaceLength = length (show n) + 1
    formatSubtree :: [String] -> String
    formatSubtree = intercalate "\n" . map (unlines . map ((replicate spaceLength ' ' ++ "| ") ++) . lines)

showTreeF :: Show a => TreeF a String -> String
showTreeF (NodeF n subtrees) =
  show n ++ drop (spaceLength - 1) (formatSubtree subtrees)
  where
    spaceLength = length (show n) + 1
    formatSubtree :: [String] -> String
    formatSubtree = intercalate "\n" . map (unlines . map ((replicate spaceLength ' ' ++ "| ") ++) . lines)

main :: IO ()
main = do
  print (showTree (split 16) == cata showTreeF (split 16))
  putStrLn (cata showTreeF (split 16))
  print (halves 256)

{-
True
16 | 8 | 4 | 2 | 1
   |   |   |
   |   |   |   | 1
   |   |
   |   |   | 2 | 1
   |   |   |
   |   |   |   | 1
   |
   |   | 4 | 2 | 1
   |   |   |
   |   |   |   | 1
   |   |
   |   |   | 2 | 1
   |   |   |
   |   |   |   | 1

   | 8 | 4 | 2 | 1
   |   |   |
   |   |   |   | 1
   |   |
   |   |   | 2 | 1
   |   |   |
   |   |   |   | 1
   |
   |   | 4 | 2 | 1
   |   |   |
   |   |   |   | 1
   |   |
   |   |   | 2 | 1
   |   |   |
   |   |   |   | 1

[256,128,64,32,16,8,4,2,1]
-}
