module Test.LLang where

import           LLang
import           AST
import           Expr
import           Combinators
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import qualified Data.Map as Map

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
    



-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing
