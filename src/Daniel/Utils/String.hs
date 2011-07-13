module Daniel.Utils.String
(
    stripPunctuation,
    stripNewLines,
    stripLeadingWhitespace,
    stripVowels
)
where

import Data.Char (isSpace)

stripPunctuation :: String -> String
stripPunctuation str = filter (\x -> not (any (==x) ".?!,_()[]\"")) str

stripNewLines :: String -> String
stripNewLines str = filter (\x -> not (any (==x) "\n")) str

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace str = dropWhile isSpace str

stripVowels :: String -> String
stripVowels str = filter (\x -> not (any (==x) "aeiou")) str


