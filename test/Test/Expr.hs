module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..),
                                      Result (..), runParser,
                                      symbol, word)
import           Control.Applicative ((<|>))
import           Expr                
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

testFailure = assertBool "" . isFailure

isFailure (Failure _) = True
isFailure  _          = False

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
    assertBool "" $ isFailure (runParser parseNum "+3")
    assertBool "" $ isFailure (runParser parseNum "a")

unit_parseIdent :: Assertion
unit_parseIdent = do
    runParser parseIdent "abc def" @?= Success " def" "abc"
    runParser parseIdent "AbC dEf" @?= Success " dEf" "AbC"
    runParser parseIdent "_123" @?= Success "" "_123"
    runParser parseIdent "a_b_c d_e" @?= Success " d_e" "a_b_c"
    runParser parseIdent "x_ " @?= Success " " "x_"
    runParser parseIdent "abc123" @?= Success "" "abc123"
    runParser parseIdent "_" @?= Success "" "_"
    runParser parseIdent "abc*1" @?= Success "*1" "abc"
    assertBool "" $ isFailure $ runParser parseIdent "123abc"
    assertBool "" $ isFailure $ runParser parseIdent "123"
    assertBool "" $ isFailure $ runParser parseIdent ""

unit_opParser :: Assertion
unit_opParser = do
    runParser (opParser "+") "+1" @?= Success "1" Plus
    runParser (opParser "*") "**" @?= Success "*" Mult
    assertBool "" $ isFailure (runParser (opParser "+") "_!+")
    assertBool "" $ isFailure (runParser (opParser "*") "+*2")

unit_staticPlus :: Assertion
unit_staticPlus = do
    staticPlus (Num 1) (Num 2) @?= Num 3
    staticPlus (Num 1) (Ident "X") @?= BinOp Plus (Num 1) (Ident "X")
    staticPlus (BinOp Mult (Num 2) (Ident "a")) (Num 0) @?= (BinOp Mult (Num 2) (Ident "a"))
    staticPlus (staticPlus (Ident "a") (Num 1)) (staticPlus (Num 0) (Num 0)) @?= BinOp Plus (Num 1) (Ident "a")
    staticPlus (BinOp Plus (Num 10) (Ident "x")) (BinOp Plus (Num 2) (Ident "y")) @?= BinOp Plus (Num 12) (BinOp Plus (Ident "x") (Ident "y"))
    staticPlus (BinOp Plus (Num 1) (Ident "x")) (Num 2) @?= BinOp Plus (Num 3) (Ident "x")
    

unit_staticMult :: Assertion
unit_staticMult = do
    staticMult (Num 1) (Num 2) @?= Num 2
    staticMult (Num 1) (Ident "x") @?= Ident "x"
    staticMult (BinOp Mult (Num 1) (Ident "x")) (Num 2) @?= BinOp Mult (Num 2) (Ident "x")
    staticMult (BinOp Mult (Num 10) (Ident "x")) (BinOp Mult (Num 2) (Ident "y")) @?= BinOp Mult (Num 20) (BinOp Mult (Ident "x") (Ident "y"))
    staticMult (BinOp Mult (Num 2) (Ident "a")) (Num 0) @?= Num 0
    staticMult (staticMult (Ident "a") (Num 1)) (staticMult (Num 1) (Num 1)) @?= Ident "a"

unit_parseExpr :: Assertion
unit_parseExpr = do
    runParser parseExpr "1*2*3"   @?= Success "" (Num 6)
    runParser parseExpr "123"     @?= Success "" (Num 123)
    runParser parseExpr "abc"     @?= Success "" (Ident "abc")
    runParser parseExpr "1*2+3*4" @?= Success "" (Num 14)
    runParser parseExpr "1+2*3+4" @?= Success "" (Num 11)
    runParser parseExpr "1*x*3"   @?= Success "" (BinOp Mult (Num 3) (Ident "x"))
    runParser parseExpr "xyz"     @?= Success "" (Ident "xyz")
    runParser parseExpr "1*x+z*4" @?= Success "" (BinOp Plus (Ident "x") (BinOp Mult (Num 4) (Ident "z")))
    runParser parseExpr "1+y*3+z" @?= Success "" (BinOp Plus (Num 1) (BinOp Plus (BinOp Mult (Num 3) (Ident "y")) (Ident "z")))
    runParser parseExpr "1*x*3*y*y*8*9*x*10" @?= Success "" (BinOp Mult (Num 2160) (BinOp Mult (BinOp Mult (BinOp Mult (Ident "x") (Ident "y")) (Ident "y")) (Ident "x")))
    runParser parseExpr "3*(1*x*3*8*9*x*10 + 12*y * 45*z * 2 * 1 + 0*t)" @?= Success "" (BinOp Mult (Num 3) (BinOp Plus (BinOp Mult (Num 2160) (BinOp Mult (Ident "x") (Ident "x"))) (BinOp Mult (Num 1080) (BinOp Mult (Ident "y") (Ident "z")))))
