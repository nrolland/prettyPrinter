
module Main1 where

import           Data.List hiding (group)
import           Lib3


data Tree =  Node String [Tree] deriving Show

tree = Node "top" [  Node "A" [], Node "B" [ Node "h" [], Node "e" [], Node "l" [] ,Node "l" [] ,Node "o" []  ] , Node "C" [ Node "D" [], Node "E" []] ]


toStringFirst :: Tree -> String
toStringFirst (Node header trees ) = "Node " ++ header  ++
                                        "(" ++ subtrees ++ ")"
    where subtrees = intercalate "," (fmap toStringFirst trees)

printFirst :: IO ()
printFirst = putStrLn (toStringFirst tree)

toString2 :: Tree -> String
toString2 (Node header trees ) = "Node " ++ header ++ "\n" ++
                                        "(" ++ subtrees ++ ")"
    where subtrees = intercalate "," (fmap toString2 trees)

print2 :: IO ()
print2 = putStrLn (toString2 tree)



----------

showTree :: Tree -> Doc
showTree (Node s ts) =  text s <> (nest (length s) (showBracket ts) )

showBracket [] =  nil
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t] = showTree t
showTrees (t:ts) =  showTree t <> text "," <> line <> showTrees ts


----------

showTree' (Node s ts) =  text s <> showBracket' ts
showBracket' []=  nil
showBracket' ts =  text "[" <>
                   nest 2 (line <> showTrees' ts) <>
                   line <> text "]"
showTrees' [t]=  showTree t
showTrees' (t:ts)=  showTree t <> text "," <> line <> showTrees ts






----------

printLib3  = putStrLn ( pretty 10 ( showTree tree ))
printLib4  = putStrLn ( pretty 50 ( showTree tree ))
printLib5  = putStrLn ( pretty 100 ( showTree tree ))



main :: IO ()
main = undefined
