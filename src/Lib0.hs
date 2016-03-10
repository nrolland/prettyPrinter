{-# LANGUAGE OverloadedStrings #-}
module Lib0( Doc
           , nil
           , text
           , line
           , nest
           , (<>)
           , layout
                 ) where

import           Data.List


(<>) ::Doc->Doc->Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest ::Int->Doc->Doc

layout :: Doc -> String



type  Doc  =  String


nil  = ""
text s = s
line = "\n"
l <> r =  l ++ r
nest i d = unlines (fmap addSpaces (lines d))
           where addSpaces line = iSpaces ++ line
                 iSpaces = take i (repeat ' ')
layout s = s


