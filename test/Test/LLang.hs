module Test.LLang where

import           LLang
import           AST
import           Expr
import           Combinators
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False


unit_parseIf = do
    runParser parseIf "if(x>  y)z =x else z = y" @?= Success "" (If {cond = BinOp Gt (Ident "x") (Ident "y"), thn = Assign {var = "z", expr = Ident "x"}, els = Assign {var = "z", expr = Ident "y"}})
    runParser parseIf "if ( x ) if (y) {} else if (z) k=1 else k=0" @?= Success "" (If {cond = Ident "x", thn = If {cond = Ident "y", thn = Seq {statements = []}, els = If {cond = Ident "z", thn = Assign {var = "k", expr = Num 1}, els = Assign {var = "k", expr = Num 0}}}, els = Seq {statements = []}})
    runParser parseIf "if (a == 1) a =  1" @?= Success "" (If {cond = BinOp Equal (Ident "a") (Num 1), thn = Assign {var = "a", expr = Num 1}, els = Seq {statements = []}})
    runParser parseIf "if (a + b) {a = 1; b = -1;}" @?= Success "" (If {cond = BinOp Plus (Ident "a") (Ident "b"), thn = Seq {statements = [Assign {var = "a", expr = Num 1},Assign {var = "b", expr = UnaryOp Minus (Num 1)}]}, els = Seq {statements = []}})
    assertBool "" $ isFailure $ runParser parseIf "if (a = 1) a = 1"
    assertBool "" $ isFailure $ runParser parseIf "if a {a =0;}"
    assertBool "" $ isFailure $ runParser parseIf "if () {}"
    assertBool "" $ isFailure $ runParser parseIf "else {}"
    assertBool "" $ isFailure $ runParser parseIf "if (1) else {}"

unit_parseWhile = do
    runParser parseWhile "while(x>  y)x=x + 1" @?= Success "" (While {cond = BinOp Gt (Ident "x") (Ident "y"), body = Assign {var = "x", expr = BinOp Plus (Ident "x") (Num 1)}})
    runParser parseWhile "while (_a) {}" @?= Success "" (While {cond = Ident "_a", body = Seq {statements = []}})
    runParser parseWhile "while(c1) while (c2) body = 1" @?= Success "" (While {cond = Ident "c1", body = While {cond = Ident "c2", body = Assign {var = "body", expr = Num 1}}})
    runParser parseWhile "while (!1) {x = 1; while (1) {};}" @?= Success "" (While {cond = UnaryOp Not (Num 1), body = Seq {statements = [Assign {var = "x", expr = Num 1},While {cond = Num 1, body = Seq {statements = []}}]}})
    assertBool "" $ isFailure $ runParser parseWhile "while (a = 1) {}"
    assertBool "" $ isFailure $ runParser parseWhile "while () {}"
    assertBool "" $ isFailure $ runParser parseWhile "while 1 {}"
    assertBool "" $ isFailure $ runParser parseWhile "while"

unit_parseAssign = do
    runParser parseAssign "x = 1" @?= Success "" (Assign {var = "x", expr = Num 1})
    runParser parseAssign "x=1 == 1" @?= Success "" (Assign {var = "x", expr = BinOp Equal (Num 1) (Num 1)})
    runParser parseAssign "x = x" @?= Success "" (Assign {var = "x", expr = Ident "x"})
    runParser parseAssign "_ =-x + y" @?= Success "" (Assign {var = "_", expr = BinOp Plus (UnaryOp Minus (Ident "x")) (Ident "y")})
    assertBool "" $ isFailure $ runParser parseAssign "1 = x"
    assertBool "" $ isFailure $ runParser parseAssign "1 = 1"
    assertBool "" $ isFailure $ runParser parseAssign "x == 9"
    assertBool "" $ isFailure $ runParser parseAssign "a + b = c"

unit_parseRead = do
    runParser parseRead "read(x)" @?= Success "" (Read {var = "x"})
    runParser parseRead "read ( _123 )" @?= Success "" (Read {var = "_123"})
    assertBool "" $ isFailure $ runParser parseRead "read(-x)"
    assertBool "" $ isFailure $ runParser parseRead "read(1)"
    assertBool "" $ isFailure $ runParser parseRead "read(x = 1)"
    assertBool "" $ isFailure $ runParser parseRead "read()"

unit_parseWrite = do
    runParser parseWrite "print(x)" @?= Success "" (Write {expr = Ident "x"})
    runParser parseWrite "print ( 1 + 2 )" @?= Success "" (Write {expr = BinOp Plus (Num 1) (Num 2)})
    assertBool "" $ isFailure $ runParser parseWrite "print (x = 1)"
    assertBool "" $ isFailure $ runParser parseWrite "print()"

unit_parseL = do
    runParser parseL " {read(n); \n\
       \ while (n > 1) { \n\
           \ if (n % 2) \n\
              \ n = 3 * n + 1 \n\
           \ else \n\
              \ n = n / 2; \n\
      \  }; \n\
  \  } " @?= Success "" 
        (Seq {statements = 
            [Read {var = "n"},
            While {cond = BinOp Gt (Ident "n") (Num 1), body = Seq {statements =
                [If {cond = BinOp Mod (Ident "n") (Num 2),
                    thn = Assign {var = "n", expr = BinOp Plus (BinOp Mult (Num 3) (Ident "n")) (Num 1)},
                    els = Assign {var = "n", expr = BinOp Div (Ident "n") (Num 2)}
                    }
                ]
            }}]
        })
    



