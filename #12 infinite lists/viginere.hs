cifra :: (Int -> Int -> Int) -> Char -> Char -> Char
cifra op offset ch = numToChar $ (charToNum ch) `op` (charToNum offset)
  where
    charToNum ch = ord ch - ord 'A'
    numToChar n = chr $ (n `mod` 26) + ord 'A'

vigenere :: String -> String -> String
vigenere secret = zipWith (cifra (+)) (cycle secret) . concat . words