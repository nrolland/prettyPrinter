module Lib3( Doc
           , nil
           , text
           , line
           , nest
           , (<>)
           , group
           , pretty ) where


(<>) ::Doc->Doc->Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest ::Int->Doc->Doc

layout :: Doc -> String

-- lois
-- ...
-- flatten (nest i line <> x) = text " " <> flatten x
-- group x               =  flatten x <|> x
-- (text s <> flatten x) <|> (text s <> x) = text s <> (flatten x <|> x)
-- ..

-- representation syntaxique de la forme normale
data  Doc  =  Nil                -- = nil
            | Text String  Doc   -- = text s <> x
            | Line Int Doc       -- = nest i line <>x
            | Doc `Union` Doc    -- = x <|> y
              deriving Show



-- And now we can deduce *every* operations from equations

nil   = Nil
text s = s `Text` Nil
line = 0 `Line` Nil


(s `Text` x) <> y = s `Text` (x <> y)
(i `Line` x) <> y = i `Line` (x <> y)
(x `Union` y) <> z    =  (x <> z) `Union` (y <> z)
Nil <> y = y

nest i (s `Text` x) =     s `Text` nest i x
nest i (j `Line` x) = (i+j) `Line` nest i x
nest i Nil = Nil
nest k (x `Union` y)  =  nest k x `Union` nest k y


layout (s `Text` x) = s ++ layout x
layout(i `Line` x)= '\n' : copy i ' '  ++ layout x
                    where copy i x = [ x | _<- [1..i] ]
layout Nil = ""


flatten Nil = Nil
flatten (i `Line` x)  =  " " `Text` flatten x
flatten (s `Text` x)  =  s `Text` flatten x
flatten (x `Union` y) =  flatten x



group Nil           =  Nil
group (i `Line` x) -- = group (nest i line <> x)                           def syntaxe
                   -- = flatten (nest i line <> x) <|> (nest i line <> x)  loi group
                   -- (text " " <> flatten x) <|> (nest i line <> x)       loi flatten
                    =  (" " `Text` flatten x) `Union` (i `Line` x)         -- syntaxe

group (s `Text` x) -- = group (text s <> x)                               def syntaxe
                   -- = flatten (text s <> x) <|> (text s <> x)           loi group
                   -- = text s <> (flatten x <|> x)                       loi
                    =  s `Text` group x                                   -- syntaxe
group (x `Union` y) =  group x `Union` y



best w k Nil             = Nil
best w k (i `Line`  x)   = i `Line` best w i x
best w k (s `Text`  x)   = s `Text` best w (k + length s) x
best w k (x `Union` y)   = better w k (best w k x)(best w k y)
    where
      better w k x y = if fits (w-k) x then x else y
      fits w x | w < 0  = False
      fits w Nil  = True
      fits w (s `Text` x) = fits (w - length s) x
      fits w (i `Line` x) = True

pretty w x = layout (best w 0 x)

