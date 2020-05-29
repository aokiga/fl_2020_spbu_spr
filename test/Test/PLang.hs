module Test.PLang where
import           Combinators
import           PLang           
import           Debug.Trace      (trace)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Control.Applicative ((<|>))
import qualified Data.Map as Map

isFailure (Failure _) = True
isFailure  _          = False

unit_parseVar :: Assertion
unit_parseVar = do
    runParser parseVar "Abacaba" @?= Success (toStream "" 7) (Var "Abacaba")
    runParser parseVar "Abacaba228" @?= Success (toStream "" 10) (Var "Abacaba228")
    assertBool "" $ isFailure (runParser parseVar "abacaba")
    assertBool "" $ isFailure (runParser parseVar "123abacaba")
    assertBool "" $ isFailure (runParser parseVar "_2123sds")

unit_parseIdent :: Assertion
unit_parseIdent = do
    runParser parseIdent "abacaba" @?= Success (toStream "" 7) ("abacaba")
    runParser parseIdent "abacaba228" @?= Success (toStream "" 10) ("abacaba228")
    assertBool "" $ isFailure (runParser parseIdent "Abacaba")
    assertBool "" $ isFailure (runParser parseIdent "123abacaba")
    assertBool "" $ isFailure (runParser parseIdent "_2123sds")

unit_modifyInput :: Assertion
unit_modifyInput = do
    modifyInput "    a b ac a b a   \n \n \n " @?= ("abacaba")

unit_parseAtom :: Assertion
unit_parseAtom = do
    runParser parseAtom "mem" @?= Success (toStream "" 3) (Atom "mem" [])
    runParser parseAtom "kek(St)" @?= Success (toStream "" 7) (Atom "kek" [Var "St"])
    runParser parseAtom "zez(St,Bt)" @?= Success (toStream "" 10) (Atom "zez" [Var "St", Var "Bt"])
    runParser parseAtom "lel(St,false,Bt,true)" @?= Success (toStream "" 21) (Atom "lel" [Var "St", Atom "false" [], Var "Bt", Atom "true" []])
    runParser parseAtom "yey(St,false(St,Bt),Bt,true)" @?= Success (toStream "" 28) (Atom "yey" [Var "St", Atom "false" [Var "St", Var "Bt"], Var "Bt", Atom "true" []])
    assertBool "" $ isFailure (runParser parseAtom "Sobaka(mem)")

unit_parseRelations :: Assertion
unit_parseRelations = do
    runParser parseRelations "eval(St,var(X),U):-elem(X,St,U)." @?= Success (toStream "" 32) (Relation "eval" [
        Var "St",
        Atom "var" [Var "X"],
        Var "U"
      ] [
        Atom "elem" [
          Var "X",
          Var "St",
          Var "U"
        ]
      ])
    runParser parseRelations "eval(St,var(X),U)." @?= Success (toStream "" 18) (Relation "eval" [
        Var "St",
        Atom "var" [Var "X"],
        Var "U"
      ] [])
    assertBool "" $ isFailure (runParser parseRelations "eval(St,var(X),U):-elem(X,St,U)")
    assertBool "" $ isFailure (runParser parseRelations "Eval(St,var(X),U):-elem(X,St,U).")


unit_parseTarget :: Assertion
unit_parseTarget = do
    runParser parseTarget "?-." @?= Success (toStream "" 3) ([])
    runParser parseTarget "?-eval(St,var(X),U),elem(X,St,U)." @?= Success (toStream "" 33) ([Atom "eval" [
        Var "St",
        Atom "var" [Var "X"],
        Var "U"
      ], Atom "elem" [
          Var "X",
          Var "St",
          Var "U"
        ]
      ])
    assertBool "" $ isFailure (runParser parseTarget "eval(St,var(X),U):-elem(X,St,U).")
    assertBool "" $ isFailure (runParser parseTarget "?-eval(St,var(X),U),elem(X,St,U)")


unit_parseProg :: Assertion
unit_parseProg = do
    let code = "eval(St, conj(X, Y), U) :- eval(St, X, V), eval(St, Y, W), and(V, W, U).\n\nelem(zero, cons(H, T), H).\n?- eval(St, true, not(mem(Z))).\n\n" 

    let rel1 = Relation "eval" [Var "St", Atom "conj" [Var "X", Var "Y"], Var "U"] [ Atom "eval" [Var "St", Var "X", Var "V"], Atom "eval" [Var "St", Var "Y", Var "W"], Atom "and" [Var "V", Var "W", Var "U"] ]
    let rel2 = Relation "elem" [Atom "zero" [], Atom "cons" [Var "H", Var "T"], Var "H"] []
    let program = Program [rel1, rel2] [Atom "eval" [Var "St", Atom "true" [], Atom "not" [Atom "mem" [Var "Z"]]]]

    runParser parseProg code @?= Success (toStream "" 110) program
    runParser parseProg "true.false.true.\n?- true." @?= Success (toStream "" 23) (Program [Relation "true" [] [], Relation "false" [] [], Relation "true" [] []] [Atom "true" []])
