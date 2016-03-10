module Main where

import           Data.List
import           Lib0
import qualified Lib1


data Tree =  Node String [Tree] deriving Show

tree = Node "top" [  Node "A" [], Node "B" [ Node "h" [], Node "e" [], Node "l" [] ,Node "l" [] ,Node "o" []   ] , Node "C" [ Node "D" [], Node "E" []] ]


toString1 :: Tree -> String
toString1 (Node header trees ) = "Node " ++ header  ++ "(" ++ subtrees ++ ")"
    where subtrees = intercalate "," (fmap toString1 trees)

print1 :: IO ()
print1 = putStrLn (toString1 tree)

toString2 :: Tree -> String
toString2 (Node header trees ) = "Node " ++ header ++ "\n" ++
                                        "(" ++ subtrees ++ ")"
    where subtrees = intercalate "," (fmap toString2 trees)

print2 :: IO ()
print2 = putStrLn (toString2 tree)














----------

showTree :: Tree -> Doc
showTree (Node s ts) =  text s <> nest (length s) (showBracket ts)
 where
   showBracket [] =  nil
   showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

   showTrees [t] = showTree t
   showTrees (t:ts) =  showTree t <> text "," <> line <> showTrees ts


printLib0 :: IO ()
printLib0 = putStrLn $ layout $ showTree tree


-----------------
-- Bonus : styles

showTree' :: Tree -> Doc
showTree' (Node s ts) =  text s <> showBracket' ts
    where showBracket' []=  nil
          showBracket' ts =  text "[" <>
                             nest 2 (line <> showTrees' ts) <>
                             line <> text "]"
          showTrees' [t]=  showTree t
          showTrees' (t:ts)=  showTree t <> text "," <> line <> showTrees' ts

printLib0' :: IO ()
printLib0' = putStrLn $ layout $  showTree' tree



main :: IO ()
main = undefined
