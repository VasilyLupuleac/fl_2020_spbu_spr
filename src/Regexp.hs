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

nu :: Regexp -> Regexp -> Regexp
nu r | nullable r = id
     | otherwise  = const Empty

derivative :: Char -> Regexp -> Regexp
derivative c (Char c') | c == c'   = Epsilon
                       | otherwise = Empty
derivative c (Seq r r') = Alt (Seq (derivative c r) r') (nu r (derivative c r'))
derivative c (Alt r r')  = Alt (derivative c r) (derivative c r')
derivative c (Star r)    = Seq (derivative c r) (Star r)
derivative _ _           = Empty

nullable :: Regexp -> Bool
nullable (Char _)   = False
nullable Empty      = False
nullable (Seq r r') = (nullable r) && (nullable r')
nullable (Alt r r') = (nullable r) || (nullable r')
nullable _          = True