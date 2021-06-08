elemento n xs = xs !!n'
    where
        size = length xs
        n'= if n<0 then n + size else n