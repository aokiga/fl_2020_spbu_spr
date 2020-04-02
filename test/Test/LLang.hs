module Test.LLang where

import AST                 (AST (..), Operator (..))
import Combinators         (Parser (..), Result (..), runParser, symbol)
import Control.Applicative ((<|>))
import Expr                (Associativity (..), evaluate, parseExpr,
                             parseNum, parseOp, toOperator, uberExpr, parseIdent)

import LLang               (LAst (..), parseCondition, parseIf, parseWhile, parseAssign,
                                      parseRead, parseWrite, parseSeq, parseCommand, parseL)

import Test.Tasty.HUnit    (Assertion, (@?=), assertBool)


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
