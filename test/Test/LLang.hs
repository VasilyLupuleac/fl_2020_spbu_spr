module Test.LLang where

import           LLang
import           AST
import           Expr
import           Combinators
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import qualified Data.Map as Map
import           Debug.Trace      (trace)
import           Text.Printf      (printf)

isFailure (Failure _) = True
isFailure  _          = False

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Read "z") (BinOp Plus (Ident "x") (Ident "y"))
    , Function "g" ["x"] (If (Ident "x") (Assign "res" (Ident "x")) (Assign "res" (BinOp Mult (Ident "x") (Num 13)))) (Ident "res")
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )


unit_parseIf = do
    runParser parseIf "if(x>  y)z =x else z = y" @?= Success (toStream "" 24) (If {cond = BinOp Gt (Ident "x") (Ident "y"), thn = Assign {var = "z", expr = Ident "x"}, els = Assign {var = "z", expr = Ident "y"}})
    runParser parseIf "if ( x ) if (y) {} else if (z) k=1 else k=0" @?= Success (toStream "" 43) (If {cond = Ident "x", thn = If {cond = Ident "y", thn = Seq {statements = []}, els = If {cond = Ident "z", thn = Assign {var = "k", expr = Num 1}, els = Assign {var = "k", expr = Num 0}}}, els = Seq {statements = []}})
    runParser parseIf "if (a == 1) a =  1" @?= Success (toStream "" 18) (If {cond = BinOp Equal (Ident "a") (Num 1), thn = Assign {var = "a", expr = Num 1}, els = Seq {statements = []}})
    runParser parseIf "if (a + b) {a = 1; b = -1;}" @?= Success (toStream "" 27) (If {cond = BinOp Plus (Ident "a") (Ident "b"), thn = Seq {statements = [Assign {var = "a", expr = Num 1},Assign {var = "b", expr = UnaryOp Minus (Num 1)}]}, els = Seq {statements = []}})
    assertBool "" $ isFailure $ runParser parseIf "if (a = 1) a = 1"
    assertBool "" $ isFailure $ runParser parseIf "if a {a =0;}"
    assertBool "" $ isFailure $ runParser parseIf "if () {}"
    assertBool "" $ isFailure $ runParser parseIf "else {}"
    assertBool "" $ isFailure $ runParser parseIf "if (1) else {}"

unit_parseWhile = do
    runParser parseWhile "while(x>  y)x=x + 1" @?= Success (toStream "" 19) (While {cond = BinOp Gt (Ident "x") (Ident "y"), body = Assign {var = "x", expr = BinOp Plus (Ident "x") (Num 1)}})
    runParser parseWhile "while (_a) {}" @?= Success (toStream "" 13) (While {cond = Ident "_a", body = Seq {statements = []}})
    runParser parseWhile "while(c1) while (c2) body = 1" @?= Success (toStream "" 29)(While {cond = Ident "c1", body = While {cond = Ident "c2", body = Assign {var = "body", expr = Num 1}}})
    runParser parseWhile "while (!1) {x = 1; while (1) {};}" @?= Success (toStream "" 33) (While {cond = UnaryOp Not (Num 1), body = Seq {statements = [Assign {var = "x", expr = Num 1},While {cond = Num 1, body = Seq {statements = []}}]}})
    assertBool "" $ isFailure $ runParser parseWhile "while (a = 1) {}"
    assertBool "" $ isFailure $ runParser parseWhile "while () {}"
    assertBool "" $ isFailure $ runParser parseWhile "while 1 {}"
    assertBool "" $ isFailure $ runParser parseWhile "while"

unit_parseAssign = do
    runParser parseAssign "x = 1" @?= Success (toStream "" 5) (Assign {var = "x", expr = Num 1})
    runParser parseAssign "x=1 == 1" @?= Success (toStream "" 8) (Assign {var = "x", expr = BinOp Equal (Num 1) (Num 1)})
    runParser parseAssign "x = x" @?= Success (toStream "" 5) (Assign {var = "x", expr = Ident "x"})
    runParser parseAssign "_ =-x + y" @?= Success (toStream "" 9) (Assign {var = "_", expr = BinOp Plus (UnaryOp Minus (Ident "x")) (Ident "y")})
    assertBool "" $ isFailure $ runParser parseAssign "1 = x"
    assertBool "" $ isFailure $ runParser parseAssign "1 = 1"
    assertBool "" $ isFailure $ runParser parseAssign "x == 9"
    assertBool "" $ isFailure $ runParser parseAssign "a + b = c"

unit_parseRead = do
    runParser parseRead "read(x)" @?= Success (toStream "" 7) (Read {var = "x"})
    runParser parseRead "read ( _123 )" @?= Success (toStream "" 13) (Read {var = "_123"})
    assertBool "" $ isFailure $ runParser parseRead "read(-x)"
    assertBool "" $ isFailure $ runParser parseRead "read(1)"
    assertBool "" $ isFailure $ runParser parseRead "read(x = 1)"
    assertBool "" $ isFailure $ runParser parseRead "read()"

unit_parseWrite = do
    runParser parseWrite "print(x)" @?= Success (toStream "" 8) (Write {expr = Ident "x"})
    runParser parseWrite "print ( 1 + 2 )" @?= Success (toStream "" 15) (Write {expr = BinOp Plus (Num 1) (Num 2)})
    assertBool "" $ isFailure $ runParser parseWrite "print (x = 1)"
    assertBool "" $ isFailure $ runParser parseWrite "print()"

unit_parseReturn = do
    runParser parseReturn "return x" @?= Success (toStream "" 8) (Ident "x")
    runParser parseReturn "return (x)" @?= Success (toStream "" 10) (Ident "x")
    runParser parseReturn "return -1 + 2" @?= Success (toStream "" 13) (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    assertBool "" $ isFailure $ runParser parseReturn "return (x = 1)"
    assertBool "" $ isFailure $ runParser parseWrite "return"

unit_parseFunCall = do
  runParser parseFunCall "f(1)" @?= Success (toStream "" 4) (FunCall {funName = "f", funArgs = [Num 1]})
  runParser parseFunCall "f(1,2, x + y)" @?= Success (toStream "" 13) (FunCall {funName = "f", funArgs = [Num 1, Num 2, BinOp Plus (Ident "x") (Ident "y")]})
  runParser parseFunCall "g()" @?= Success (toStream "" 3) (FunCall {funName = "g", funArgs = []})
  assertBool "" $ isFailure $ runParser parseFunCall "f({})"

unit_parseL = do
    runParser parseL " {read(n); \n\
       \ while (n > 1) { \n\
           \ if (n % 2) \n\
              \ n = 3 * n + 1 \n\
           \ else \n\
              \ n = n / 2; \n\
      \  }; \n\
  \  } " @?= Success (toStream "" 89)
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

unit_parseDef = do 
  runParser parseDef "fun f () {return 0}" @?= Success (toStream "" 19) (Function {name = "f", args = [], funBody = Seq {statements = []}, returnExpr = Num 0})
  runParser parseDef "fun g_(x, y) {x = y; return 0}" @?= Success (toStream "" 30) (Function {name = "g_", args = ["x","y"], funBody = Seq {statements = [Assign {var = "x", expr = Ident "y"}]}, returnExpr = Num 0})
  runParser parseDef "fun g(a) {a = 1; return 1}" @?= Success (toStream "" 26) (Function {name = "g", args = ["a"], funBody = Seq {statements = [Assign {var = "a", expr = Num 1}]}, returnExpr = Num 1})
  assertBool "" $ isFailure $ runParser parseDef "fun f {}"

unit_parseProg = do
  runParser parseProg "{x = 1; x = 2;}" @?= Success (toStream "" 15) (Program {functions = [], main = Seq {statements = [Assign {var = "x", expr = Num 1},Assign {var = "x", expr = Num 2}]}})
  runParser parseProg "fun g(a) {a = 1; return a} fun f () {print(1); return (1 + 2)} {}" @?= Success (toStream "" 65) (Program {functions = [Function {name = "g", args = ["a"], funBody = Seq {statements = [Assign {var = "a", expr = Num 1}]}, returnExpr = Ident "a"}, Function {name = "f", args = [], funBody = Seq {statements = [Write {expr = Num 1}]}, returnExpr = BinOp Plus (Num 1) (Num 2)}], main = Seq {statements = []}})
