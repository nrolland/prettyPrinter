module Lib2( Docs
           , nil
           , text
           , line
           , nest
           , (<>)
           , group
           , pretty ) where

import           Data.List hiding (group)

(<>) ::Docs->Docs->Docs
nil :: Docs
text :: String -> Docs
line :: Docs
nest ::Int->Docs->Docs

layout :: Doc -> String


data  Doc  =  Nil
            | Text String  Doc
            | Line Int Doc

type Docs = [Doc]

nil   = [ Nil ]
text s = [ s `Text` Nil ]
line = [0 `Line` Nil]


xs <> ys = [ x `cat` y |  x <- xs, y <- ys ]
        where
          (s `Text` x) `cat` y = s `Text` (x `cat` y)
          (i `Line` x) `cat` y = i `Line` (x `cat` y)
          Nil `cat` y = y

nest i xs = [ nest' i x | x <- xs ]
    where nest' i (s `Text` x) =     s `Text` nest' i x
          nest' i (j `Line` x) = (i+j) `Line` nest' i x
          nest' i Nil = Nil


layout (s `Text` x) = s ++ layout x
layout(i `Line` x)= '\n' : copy i ' '  ++ layout x
                    where copy i x = [ x | _<- [1..i] ]
layout Nil = ""

pretty w xs = layout someBestChoice
    where
      someBestChoice = firstLessThanW $ sortOn (length . firstLine) $ xs
      firstLine d = ""
      firstLessThanW  = head

group xs               =  (flatten xs) : xs

flatten (x:xs) = flatten' x
    where flatten' Nil = Nil
          flatten' (i `Line` x)  =  " " `Text` flatten' x
          flatten' (s `Text` x)  =  s `Text` flatten' x



