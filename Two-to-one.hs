-- Description:
-- Take 2 strings s1 and s2 including only letters from ato z. Return a new sorted string, the longest possible, containing distinct letters, each taken only once - coming from s1 or s2

-- Examples:
-- a = "xyaabbbccccdefww"
-- b = "xxxxyyyyabklmopq"
-- longest(a, b) -> "abcdefklmopqwxy"

-- a = "abcdefghijklmnopqrstuvwxyz"
-- longest(a, a) -> "abcdefghijklmnopqrstuvwxyz"

-- Solution:
module Codewars.G964.Longest where
  
sort :: [Char] -> [Char]
sort [] = []
sort (p:xs) = (sort lesser) ++ [p] ++ (sort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

unique :: [Char] -> [Char]
unique [] = []
unique (x:xs) = x : unique (dropWhile (==x) xs)

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = unique $ sort $ s1 ++ s2