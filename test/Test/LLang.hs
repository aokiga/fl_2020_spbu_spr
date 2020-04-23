module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream, symbol)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang            (LAst (..), parseReturn, parseL, parseProg, parseDef,
                                   Configuration (..), eval, initialConf, Function (..), Program (..))
import           Expr             (Associativity (..), evaluate, parseExpr,
                                   parseNum, parseOp, toOperator, uberExpr, parseIdent)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Control.Applicative ((<|>))
import qualified Data.Map as Map

isFailure (Failure _) = True
isFailure  _          = False

-- -- f x y = read z ; return (x + z * y)
-- -- g x = if (x) then return x else return x*13
-- -- {read x; read y; write (f x y); write (g x)}"

-- prog =
--   Program
--     [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
--     , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
--     ]
--     (
--       Seq
--         [ Read "x"
--         , Read "y"
--         , Write (FunctionCall "f" [Ident "x", Ident "y"])
--         , Write (FunctionCall "g" [Ident "x"])
--         ]
--     )

-- -- read x;
-- -- if (x > 13)
-- -- then { write x }
-- -- else {
-- --     while (x < 42) {
-- --       x := x * 7;
-- --       write (x);
-- --     }
-- -- }
-- stmt1 :: LAst
-- stmt1 =
--   Seq
--     [ Read "x"
--     , If (BinOp Gt (Ident "x") (Num 13))
--          (Seq [(Write (Ident "x"))])
--          (Seq [(While (BinOp Lt (Ident "x") (Num 42))
--                 (Seq [ Assign "x"
--                         (BinOp Mult (Ident "x") (Num 7))
--                      , Write (Ident "x")
--                      ]
--                 )
--          )])
--     ]

-- unit_stmt1 :: Assertion
-- unit_stmt1 = do
--   let xIs n = Map.fromList [("x", n)]
--   eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
--   eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
--   eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- -- read x;
-- -- if (x)
-- -- then {
-- --   while (x) {
-- --     x := x - 2;
-- --     write (x);
-- --   }
-- -- else {}
-- stmt2 :: LAst
-- stmt2 =
--   Seq
--     [ Read "x"
--     , If (Ident "x")
--          (Seq [(While (Ident "x")
--                 (Seq
--                    [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
--                    , (Write (Ident "x"))
--                    ]
--                 )
--          )])
--          (Seq [])
--     ]

-- unit_stmt2 :: Assertion
-- unit_stmt2 = do
--   let xIs n = Map.fromList [("x", n)]
--   eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
--   eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
--   eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- -- read x;
-- -- read y;
-- -- write (x == y);
-- stmt3 :: LAst
-- stmt3 =
--   Seq
--     [ Read "x"
--     , Read "y"
--     , Write (BinOp Equal (Ident "x") ((Ident "y")))
--     ]

-- unit_stmt3 :: Assertion
-- unit_stmt3 = do
--   let subst x y = Map.fromList [("x", x), ("y", y) ]
--   eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
--   eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
--   eval stmt3 (initialConf [42]) @?= Nothing

-- -- read n;
-- -- if (n == 1 || n == 2)
-- -- then {
-- --   write 1;
-- -- }
-- -- else {
-- --   i := 2;
-- --   cur := 1
-- --   prev := 1
-- --   while (i < n) {
-- --     temp := cur + prev;
-- --     prev := cur;
-- --     cur := temp;
-- --     i := i + 1;
-- --   }
-- --   write (cur);
-- -- }
-- stmt4 :: LAst
-- stmt4 =
--   Seq
--     [ Read "n"
--     , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
--          (Seq [(Write (Num 1))])
--          (Seq
--             [ Assign "i" (Num 2)
--             , Assign "cur" (Num 1)
--             , Assign "prev" (Num 1)
--             , While (BinOp Lt (Ident "i") (Ident "n"))
--                      (Seq
--                         [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
--                         , Assign "prev" (Ident "cur")
--                         , Assign "cur" (Ident "temp")
--                         , Assign "i" (BinOp Plus (Ident "i") (Num 1))
--                         ]
--                      )
--             , Write (Ident "cur")
--             ]
--          )
--     ]

-- unit_stmt4 :: Assertion
-- unit_stmt4 = do
--   let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
--   let subst' n = Map.fromList [("n", n)]
--   eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
--   eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
--   eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
--   eval stmt4 (initialConf []) @?= Nothing


{-
unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing

unit_parseReturn :: Assertion
unit_parseReturn = do
    runParser parseReturn "etun (a)" @?= Success (toStream "" 8) (Return (Ident "a"))
    runParser parseReturn "etun (12)" @?= Success (toStream "" 9) (Return (Num 12))
    runParser parseReturn "etun (x+y)" @?= Success (toStream "" 10) (Return (BinOp Plus (Ident "x") (Ident "y")))     

unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "   { ead x; pint (x); }   " @?= Success (toStream "" 26) (Seq [Read "x", Write (Ident "x")])

    --factorial
    runParser parseL "{ ead x; va ans (1); va i (x); while (i>0) { va i (i-1); va ans (ans*i); }; pint (ans); }" @?= Success (toStream "" 89) (
        Seq [
            Read "x",
            Assign "ans" (Num 1),
            Assign "i" (Ident "x"),
            While (BinOp Gt (Ident "i") (Num 0)) (Seq [
                Assign "i" (BinOp Minus (Ident "i") (Num 1)),
                Assign "ans" (BinOp Mult (Ident "ans") (Ident "i"))
            ]),
            Write (Ident "ans")
        ])

    assertBool "" $ isFailure (runParser parseL "mem")
    assertBool "" $ isFailure (runParser parseL "{ if (a==b) {}; }")
    assertBool "" $ isFailure (runParser parseL "{ f (12); }")

unit_parseDef = do
  runParser parseDef "fun fact(x) { va ans (1); va i (x); while (i>0) { va i (i-1); va ans (ans*i); }; etun (ans); }" @?= Success (toStream "" 94) (
    Function "fact" ["x"] (
        Seq [
            Assign "ans" (Num 1),
            Assign "i" (Ident "x"),
            While (BinOp Gt (Ident "i") (Num 0)) (Seq [
                Assign "i" (BinOp Minus (Ident "i") (Num 1)),
                Assign "ans" (BinOp Mult (Ident "ans") (Ident "i"))
            ]),
            Return (Ident "ans")
        ])
    )



  runParser parseDef "fun f(x, y) { ead z; etun (x+y); }" @?= Success (toStream "" 34) funcF

  runParser parseDef "fun g(x) { if (x) { etun (x); } else { etun (x*13); }; }" @?= Success (toStream "" 56) funcG


-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

funcF = Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
funcG = Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))

prog =
  Program
    [ funcF, funcG ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

code = "fun f(x, y) { read z; return (x+y); } fun g(x) { if (x) { return (x); } else { return (x*13); }; } { read x; read y; print (f(x,y)); print (g(x)); }"

prog0 = 
  Program
        [Function "f" [] (Seq [Write (Num 12)])]
        (Seq [])

prog1 = 
  Program
        [Function "f" ["a", "b", "c"] (Seq [Read "a", Write (Ident "b")])]
        (Seq [Read "x", Write (Ident "x")])

unit_parseProg :: Assertion
unit_parseProg = do
    runParser parseProg "fun f() { print (12); } { }" @?= Success (toStream "" 26) prog0
    runParser parseProg "fun f(a, b, c) { read a; print (b);  }   { read x; print (x); }   " @?= Success (toStream "" 62) prog1
    runParser parseProg code @?= Success (toStream "" 137) prog

-}