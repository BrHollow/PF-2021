delete a (x:xs)
            |a==x=xs
            |otherwise = x: delete a xs