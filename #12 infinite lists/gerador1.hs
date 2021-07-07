import Data.List
gerador1 = 0:iterate (\x -> if x > 0 then -x else 1 -x) 1