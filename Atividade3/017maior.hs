maior [x] = x
maior (x:xs) = if maior1 > x then maior1 else x
            where maior1 = maior xs