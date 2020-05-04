module Test.LEval where

import           AST
import qualified Data.Map         as Map
import           Data.Maybe       (isNothing)
import           LEval
import           LLang            (Configuration (..), Function (..), LAst (..),
                                   Program (..), initialConf)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

ignoreDefs :: Maybe Configuration -> Maybe Configuration -> Assertion
ignoreDefs (Just conf) (Just conf') = do
    subst conf @?= subst conf'
    input conf @?= input conf'
    output conf @?= output conf'
ignoreDefs Nothing conf = assertBool "" (isNothing conf)
ignoreDefs conf Nothing = assertBool "" (isNothing conf)


unit_evaluate :: Assertion
unit_evaluate = do
    evaluate [] "1" @?= Just 1
    evaluate [] "1+2" @?= Just (1 + 2)
    evaluate [] "2 + 4 +8" @?= Just (2 + 4 + 8)
    evaluate [] "11+22" @?= Just (11+22)
    evaluate [] "13 + 42 + 777" @?= Just (13 + 42 + 777)
    evaluate [] "31+ 24+777" @?= Just (31 + 24 + 777)
    evaluate [] "1+2*3+4" @?= Just (1 + 2 * 3 + 4)
    evaluate [] "12+23*34+456" @?= Just (12 + 23 * 34 + 456)
    evaluate [] "1-2*3+4" @?= Just (1 - 2 * 3 + 4)
    evaluate [] "1-2-3" @?= Just (1 - 2 - 3)
    evaluate [] "4/2-2" @?= Just (4 `div` 2 - 2)
    evaluate [] "(1+2)*(3+4)" @?= Just ((1 + 2) * (3 + 4))
    evaluate [] "12+(23 * (34)+456)" @?= Just (12 + (23 * (34) + 456))
    evaluate [] "((1-(2*3))+4)" @?= Just ((1 - (2 * 3)) + 4)
    evaluate [] "1-2+3-4" @?= Just (1 - 2 + 3 - 4)
    evaluate [] "6/2*3" @?= Just (6 `div` 2 * 3)
    evaluate [("x", 1)] "x" @?= (Just 1)
    evaluate [("x", 10), ("y", 100)] "x * y" @?= Just 1000
    evaluate [("x", 10), ("y", 100)] "x + y" @?= Just 110
    evaluate [("x", 10), ("y", 100)] "x - y" @?= Just (-90)
    evaluate [("x", 10), ("y", 100)] "y / x" @?= Just 10
    evaluate [("x", 10), ("y", 100)] "x < y" @?= Just 1
    evaluate [("x", 10), ("y", 100)] "x <= y" @?= Just 1
    evaluate [("x", 10)] "x <= x" @?= Just 1
    evaluate [("x", 10), ("y", 100)] "x > y" @?= Just 0
    evaluate [("x", 10), ("y", 100)] "x >= y" @?= Just 0
    evaluate [("x", 224)] "x >= x" @?= Just 1
    evaluate [("x", 747)] "x == x" @?= Just 1
    evaluate [("x", 380)] "x /= x" @?= Just 0
    evaluate [("x", 10), ("y", 100)] "x % y" @?= Just 10
    evaluate [("x", 10), ("y", 100)] "x % y" @?= Just 10
    evaluate [("x", 10), ("y", 100)] "x % y" @?= Just 10
    evaluate [("x", 10), ("y", 2)] "-x ^ y" @?= Just (-100)
    evaluate [("x", 10), ("y", 3)] "!x" @?= Just 0
    evaluate [("x", 0), ("y", 1)] "x&&y" @?= Just 0
    evaluate [("x", 0), ("y", 1)] "x||y" @?= Just 1


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
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7] Map.empty)
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70] Map.empty)
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42] Map.empty)


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
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [] Map.empty)
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0] Map.empty)
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]) Map.empty)

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
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0] Map.empty)
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1] Map.empty)
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
         (Seq [(Write (Num 1))])
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
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1] Map.empty)
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1] Map.empty)
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] Map.empty)
  eval stmt4 (initialConf []) @?= Nothing


-- f x y = read z ; return (x + z * y)
-- g x = if (x) then res = y else res = x*13; return res
-- {read x; read y; write (f x y); write (g x)}"
prog =
  Program
    [ Function "f" ["x", "y"] (Read "z") (BinOp Plus (Ident "x") (BinOp Mult (Ident "z") (Ident "y")))
    , Function "g" ["x"] (If (Ident "x")
                             (Seq [Write (Num 42), Assign "res" (Ident "x")])
                             (Assign "res" (BinOp Mult (Ident "x") (Num 13))))
                         (Ident "res")
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

unit_evalProg = do
    let vars x y = Map.fromList [("x", x), ("y", y)]
    ignoreDefs (evalProg prog [1..10])    (Just $ Conf (vars 1 2) [4..10] [1, 42, 7] Map.empty)
    ignoreDefs (evalProg prog [13, 1, 2]) (Just $ Conf (vars 13 1) [] [13, 42, 15] Map.empty)
    ignoreDefs (evalProg prog [0, 1, 2])  (Just $ Conf (vars 0 1) [] [0, 2] Map.empty)

-- f x = if (x < 10) then {x := x + 1; write x; r := f x} else {}; return 0
-- {read x; _ := f x}
prog1 :: Program
prog1 =
  Program
    [ Function "f" ["x"]
        (If (BinOp Lt (Ident "x") (Num 10))
            (Seq [ Assign "x" (BinOp Plus (Ident "x") (Num 1))
                 , Write (Ident "x")
                 , Assign "r" (FunctionCall "f" [Ident "x"])
                 ]
            )
            (Seq []))
        (Num 0)
    ]
    (
      Seq
        [ Read "x"
        , Assign "_" (FunctionCall "f" [Ident "x"])
        ]
    )

unit_evalProg1 = do
    let vars x y = Map.fromList [("x", x), ("_", y)]
    ignoreDefs (evalProg prog1 [1..10]) (Just $ Conf (vars 1 0) [2..10] (reverse [2..10]) Map.empty)
    ignoreDefs (evalProg prog1 [13]) (Just $ Conf (vars 13 0) [] [] Map.empty)

-- f x = if (x < 10) then {x := x + 1; r := f x; write x} else {}; return 0
-- {read x; _ := f x}
prog2 :: Program
prog2 =
  Program
    [ Function "f" ["x"]
        (If (BinOp Lt (Ident "x") (Num 10))
            (Seq [ Assign "x" (BinOp Plus (Ident "x") (Num 1))
                 , Assign "r" (FunctionCall "f" [Ident "x"])
                 , Write (Ident "x")
                 ]
            )
            (Seq []))
        (Num 0)
    ]
    (
      Seq
        [ Read "x"
        , Assign "_" (FunctionCall "f" [Ident "x"])
        ]
    )

unit_evalProg2 = do
    let vars x y = Map.fromList [("x", x), ("_", y)]
    ignoreDefs (evalProg prog2 [1..10]) (Just $ Conf (vars 1 0) [2..10] [2..10] Map.empty)
    ignoreDefs (evalProg prog2 [13]) (Just $ Conf (vars 13 0) [] [] Map.empty)
    ignoreDefs (evalProg prog2 []) Nothing

-- even n = if n == 0 then {res = 0} else {res = odd (n - 1)}; return res
-- odd n = if n == 0 then {res = 1} else {if n == 1 then {res = 0} else {res = even (n - 1)}}; return res
-- {read n; write (even n); write (odd n)}
prog3 =
  Program
    [ Function "even" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 0))
            (Assign "res" (FunctionCall "odd" [BinOp Minus (Ident "n") (Num 1)]))
        )
        (Ident "res")
    , Function "odd" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 1))
            (If (BinOp Equal (Ident "n") (Num 1))
                (Assign "res" (Num 0))
                (Assign "res" (FunctionCall "even" [BinOp Minus (Ident "n") (Num 1)]))
            )
        )
        (Ident "res")
    ]
    (Seq
      [ Read "n"
      , Write (FunctionCall "even" [Ident "n"])
      , Write (FunctionCall "odd" [Ident "n"])
      ]
    )

unit_evalProg3 = do
    let vars n = Map.fromList [("n", n)]
    ignoreDefs (evalProg prog3 [1..10]) (Just $ Conf (vars 1) [2..10] [0, 1] Map.empty)
    ignoreDefs (evalProg prog3 [13]) (Just $ Conf (vars 13) [] [0, 1] Map.empty)
    ignoreDefs (evalProg prog3 [0]) (Just $ Conf (vars 0) [] [1, 0] Map.empty)


-- even n = if n == 0 then {res = 0} else {res = odd (n - 1)}; return res
-- odd n = if n == 0 then {res = 1} else {if n == 1 then {res = 0} else {res = even (n - 1)}}; return res
-- {write(odd 0)}
prog4 =
  Program
    [ Function "even" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 0))
            (Assign "res" (FunctionCall "odd" [BinOp Minus (Ident "n") (Num 1)]))
        )
        (Ident "res")
    , Function "odd" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 1))
            (If (BinOp Equal (Ident "n") (Num 1))
                (Assign "res" (Num 0))
                (Assign "res" (FunctionCall "even" [BinOp Minus (Ident "n") (Num 1)]))
            )
        )
        (Ident "res")
    ]
    (Seq [ Write (FunctionCall "odd" [Num 0])
         , Write (FunctionCall "odd" [Num 1])
         , Write (FunctionCall "odd" [Num 2])
         , Write (FunctionCall "odd" [Num 13])
         , Write (FunctionCall "even" [Num 0])
         , Write (FunctionCall "even" [Num 1])
         , Write (FunctionCall "even" [Num 2])
         , Write (FunctionCall "even" [Num 13])
         ]
    )

unit_evalProg4 = do
    ignoreDefs (evalProg prog4 []) (Just $ Conf Map.empty [] [1,0,1,0,0,1,0,1] Map.empty)
