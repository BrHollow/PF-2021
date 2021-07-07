import Data.List
gerador3 = [x*2| x <- [1..]]

/// sem list
gerador3= iterate (*2) 1