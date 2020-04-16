module Test.LLang where

<<<<<<< HEAD
import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream, symbol)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang            (LAst (..), parseCondition, parseIf, parseWhile, parseAssign,
                                   parseRead, parseWrite, parseSeq, parseCommand, parseL,
                                   Configuration (..), eval, initialConf, Function (..), Program (..))
import           Expr             (Associativity (..), evaluate, parseExpr,
                                   parseNum, parseOp, toOperator, uberExpr, parseIdent)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Control.Applicative ((<|>))
import qualified Data.Map as Map

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )


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
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing


isFailure (Failure _) = True
isFailure  _          = False


unit_parseCondition :: Assertion
unit_parseCondition = do
    runParser parseCondition "(001)" @?= Success "" (Num 1)
    runParser parseCondition "  (  m  )" @?= Success "" (Ident "m")
    assertBool "" $ isFailure (runParser parseCondition "mem")

unit_parseAssign :: Assertion
unit_parseAssign = do
    runParser parseAssign "va name (0)" @?= Success "" 
        (Assign "name" (Num 0))
    runParser parseAssign "va  name (  1+2  )" @?= Success "" 
        (Assign "name" (BinOp Plus (Num 1) (Num 2)))
    runParser parseAssign "va i (i-1)" @?= Success "" 
        (Assign "i" (BinOp Minus (Ident "i") (Num 1)))
    runParser parseAssign "va ans (ans*i)" @?= Success "" 
        (Assign "ans" (BinOp Mult (Ident "ans") (Ident "i")))
    assertBool "" $ isFailure (runParser parseAssign "mem")
    assertBool "" $ isFailure (runParser parseAssign "va name 1")

unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "ead  mem" @?= Success "" 
        (Read "mem")
    assertBool "" $ isFailure (runParser parseRead "mem")
    assertBool "" $ isFailure (runParser parseRead "ead (mem)")
    assertBool "" $ isFailure (runParser parseRead "ead 12")

unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "pint  (mem)" @?= Success "" 
        (Write (Ident "mem"))
    runParser parseWrite "pint (1+2)" @?= Success "" 
        (Write (BinOp Plus (Num 1) (Num 2)))
    assertBool "" $ isFailure (runParser parseWrite "mem")
    assertBool "" $ isFailure (runParser parseWrite "pint 0")
    assertBool "" $ isFailure (runParser parseWrite "pint(1+2)")


unit_parseSeq :: Assertion
unit_parseSeq = do
    runParser parseSeq "{ead x;}" @?= Success "" 
        (Seq [Read "x"])
    runParser parseSeq "{pint (1);ead x;}" @?= Success "" 
        (Seq [Write (Num 1), Read "x"])
    runParser parseSeq "{ pint (1) ;  ead x  ;}" @?= Success "" 
        (Seq [Write (Num 1), Read "x"])
    runParser parseSeq "{   }" @?= Success "" 
        (Seq [])
    runParser parseL "{while (i>0) { va i (i-1); va ans (ans*i); };}" @?= Success "" (
        Seq [
            While (BinOp Gt (Ident "i") (Num 0)) (Seq [
                Assign "i" (BinOp Minus (Ident "i") (Num 1)),
                Assign "ans" (BinOp Mult (Ident "ans") (Ident "i"))
            ])
        ])
    runParser parseL "{ va i (i-1); va ans (ans*i); }" @?= Success ""
       (Seq [
            Assign "i" (BinOp Minus (Ident "i") (Num 1)),
            Assign "ans" (BinOp Mult (Ident "ans") (Ident "i"))
        ])
    assertBool "" $ isFailure (runParser parseSeq "mem")
    assertBool "" $ isFailure (runParser parseSeq "{ead x}")


unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "if  (1==2) {}  else  {}" @?= Success "" 
        (If (BinOp Equal (Num 1) (Num 2)) (Seq []) (Seq []))
    runParser parseIf "if  (1==2) { ead x  ; pint (x)  ; }  else { ead x;  }" @?= Success "" 
        (If (BinOp Equal (Num 1) (Num 2)) (Seq [Read "x", Write (Ident "x")]) (Seq [Read "x"]))
    assertBool "" $ isFailure (runParser parseIf "mem")
    assertBool "" $ isFailure (runParser parseIf "if () {} else {}")
    assertBool "" $ isFailure (runParser parseIf "if (1==2) {}")
    assertBool "" $ isFailure (runParser parseIf "if (1==2) {} {}")
    assertBool "" $ isFailure (runParser parseIf "if (1==2) {} else")

unit_parseWhile :: Assertion
unit_parseWhile = do
    runParser parseWhile "while  (x==1)  { ead x; }" @?= Success ""
        (While (BinOp Equal (Ident "x") (Num 1)) (Seq [Read "x"]))
    runParser parseWhile "while(x==1) {}" @?= Success ""
        (While (BinOp Equal (Ident "x") (Num 1)) (Seq []))
    runParser parseWhile "while(i>0) { va i (i-1); va ans (ans*i); }" @?= Success "" (
        While (BinOp Gt (Ident "i") (Num 0)) (Seq [
            Assign "i" (BinOp Minus (Ident "i") (Num 1)),
            Assign "ans" (BinOp Mult (Ident "ans") (Ident "i"))
        ]))
    assertBool "" $ isFailure (runParser parseWhile "mem")

unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "   { ead x; pint (x); }   " @?= Success "" (Seq [Read "x", Write (Ident "x")])
    runParser parseL "   {  read x; print (x); }   " @?= Success "" (Seq [Read "x", Write (Ident "x")])

    --factorial
    runParser parseL "{\nread x;\nvar ans (1);\nvar i (x);\nwhile (i>0) {\nvar i (i-1);\nvar ans (ans*i);\n};\nprint (ans);\n}\nrrrrrrrrrr" @?= Success "" (
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
