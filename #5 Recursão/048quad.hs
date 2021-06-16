guard i n
    | i * i == n = True
    | i * i > n = False
    | otherwise = guard (i + 1) n

quadperf n = guard 1 n