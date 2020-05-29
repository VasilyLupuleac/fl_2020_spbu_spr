module AST where

import           Text.Printf (printf)

data Operator = Plus
              | Mult
              deriving (Eq)

data AST = BinOp Operator AST AST
         | Num Int
         | Ident String
         deriving (Eq)

instance Show Operator where
  show Plus  = "+"
  show Mult  = "*"
  
instance Show AST where
  show (Num x) = show x
  show (Ident x) = x
  show (BinOp Plus a b) = printf "%s + %s" (show a) (show b)
  show (BinOp Mult x@(BinOp Plus a b) y@(BinOp Plus c d)) = printf "(%s) * (%s)" (show x) (show y)
  show (BinOp Mult x@(BinOp Plus a b) y) = printf "(%s) * %s" (show x) (show y)
  show (BinOp Mult x y@(BinOp Plus c d)) = printf "%s * (%s)" (show x) (show y)
  show (BinOp Mult a b) = printf "%s * %s" (show a) (show b)

{--instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i        -> show i
          Ident x      -> x
      ident = (+1)--}