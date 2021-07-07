import Data.List
gerador5 num = unfoldr fn num
    where
        fn 0 = Nothing 
        fn x = Just (x, x `div` 2)