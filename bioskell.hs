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
patternToNumber :: String -> Int
patternToNumber pattern
    |null (init pattern) = baseTo4git (last pattern)
    |otherwise = (4 * patternToNumber (init pattern)) + baseTo4git (last pattern)
    where baseTo4git 'a' = 0
          baseTo4git 'c' = 1
          baseTo4git 'g' = 2
          baseTo4git 't' = 3
          baseTo4git c = error ("Character '" ++ c:[] ++ "' not in dictionary")

-- Reverses the patternToNumber transformation
numberToPattern :: Int -> String
numberToPattern number
    |number < 4 = (fourgitToBase number)
    |otherwise = numberToPattern (number `div` 4) ++ fourgitToBase (number - ((number `div` 4) * 4))
    where fourgitToBase 0 = 'a':[]
          fourgitToBase 1 = 'c':[]
          fourgitToBase 2 = 'g':[]
          fourgitToBase 3 = 't':[]
          fourgitToBase n = error ("Number out of range. Expected 0-3.")
