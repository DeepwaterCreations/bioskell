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

-- Transforms a string of k DNA nucleotides into an integer between 0 and 4^k-1
-- Currently reads the string backwards from how the book does it.
patternToNumber :: String -> Int
patternToNumber pattern@(x:xp)
    |null xp = baseTo4git x
    |otherwise = (4 * patternToNumber xp) + baseTo4git x
    where baseTo4git 'a' = 0
          baseTo4git 'c' = 1
          baseTo4git 'g' = 2
          baseTo4git 't' = 3
          baseTo4git c = error ("Character '" ++ c:[] ++ "' not in dictionary")
