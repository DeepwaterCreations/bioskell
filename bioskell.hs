-- For string s and substring p, return the number of times p appears in s
patternCount :: String -> String -> Int
patternCount string@(x:xs) pattern
    | null xs = if string == pattern then 1 else 0
    | otherwise = patternCount xs pattern + patternMatch string pattern
    where patternMatch string@(s:ss) pattern@(p:ps)
            | s /= p = 0
            | null ps = 1
            | null ss = 0
            | otherwise = patternMatch ss ps
