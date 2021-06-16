rotesq 0 xs = xs
rotesq n(x:xs)=rot (n-1)(xs ++ [x])