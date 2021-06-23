unique [] = []
unique (x:xs) = x : unique [y | y<-xs, y/=x]