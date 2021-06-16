rot 0 xs = xs
rot n xs=rot (n-1)(last xs : init xs)