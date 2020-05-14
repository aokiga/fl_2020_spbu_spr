module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative c Empty = Empty
derivative c Epsilon = Empty
derivative c (Char c') | c == c' = Epsilon
                       | otherwise = Empty
derivative c (Alt l r) = Alt (derivative c l) (derivative c r)
derivative c (Seq l r) | nullable l = Alt (Seq (derivative c l) r) (derivative c r)
                       | otherwise = Seq (derivative c l) r
derivative c (Star x) = Seq (derivative c x) (Star x)

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char _) = False
nullable (Seq l r) = nullable l && nullable r
nullable (Alt l r) = nullable l || nullable r
nullable (Star _) = True