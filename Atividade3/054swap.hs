swap xs p1 p2
          |p1 < size && p2 <size = left++(xs !! p2):miolo ++ (xs !! p1):right
          |otherwise = xs
          where
               size = lenght xs
               left = take(p1) xs
               right = drop (p2+1)xs
               miolo = take(p2-p1-1)(drop p1+1)xs)