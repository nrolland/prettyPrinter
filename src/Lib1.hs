module Lib1( Doc
           , nil
           , text
           , line
           , nest
           , (<>)
           , layout ) where


(<>) ::Doc->Doc->Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest ::Int->Doc->Doc

layout :: Doc -> String

-- LAWS
-- text s <> text t = text (s ++ t)
-- nil              = text ""

-- nest i (nest j x) = nest (i+j) x
--  x                = nest 0     x

-- nest i (x <> y) = nest i x <> nest i y
-- nest i nil      = nil
-- nest i (text s) = text s

-- (x <> y) <> z =  x <> (y <> z )

-- Par ces lois, tout document se reduit a
-- text s <> x
-- nest i line <>x
-- nil


data  Doc  =  Nil               -- = nil
            | Text String  Doc  -- = text s <> x
            | Line Int Doc      -- = nest i line <>x


-- And now we can deduce *every* operations from equations

nil   = Nil
text s = s `Text` Nil
line = 0 `Line` Nil



(s `Text` x) <> y -- = (text s <> x) <> y
                  -- = text s <> (x <> y)
                  =  s `Text` (x <> y)
(i `Line` x) <> y = i `Line` (x <> y)
Nil <> y = y


nest i (s `Text` x) =     s `Text` nest i x
nest i (j `Line` x) = (i+j) `Line` nest i x
nest i Nil = Nil


layout (s `Text` x) = s ++ layout x
layout(i `Line` x)= '\n' : copy i ' '  ++ layout x
                    where copy i x = [ x | _<- [1..i] ]
layout Nil = ""



