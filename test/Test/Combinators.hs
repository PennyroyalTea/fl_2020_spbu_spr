module Test.Combinators where

import           Combinators      (Parser, Result (..), elem', many', runParser,
                                   satisfy, sepBy1, some', symbol)
import           Test.Tasty.HUnit (Assertion, (@?=))

predErrMsg :: String
predErrMsg = "Predicate failed"

digit :: Parser String String Char
digit = satisfy (`elem` "012346789")

unit_satisfy :: Assertion
unit_satisfy = do
    runParser (satisfy (/= '1')) "1234" @?= Failure predErrMsg
    runParser (satisfy (== '1')) "1234" @?= Success "234" '1'
    runParser digit "1234" @?= Success "234" '1'
    runParser digit "blah" @?= Failure predErrMsg

unit_elem :: Assertion
unit_elem = do
    runParser elem' "1234" @?= Success "234" '1'
    runParser elem' [1,2,3,4] @?= Success [2,3,4] 1
    runParser elem' "" @?= Failure predErrMsg

unit_many :: Assertion
unit_many = do
    runParser (many' $ symbol '1') "234" @?= Success "234" ""
    runParser (many' $ symbol '1') "134" @?= Success "34" "1"
    runParser (many' $ symbol '1') "114" @?= Success "4" "11"
    runParser (many' $ symbol '1') "111" @?= Success "" "111"

unit_some :: Assertion
unit_some = do
    runParser (some' $ symbol '1') "234" @?= Failure predErrMsg
    runParser (some' $ symbol '1') "134" @?= Success "34" "1"
    runParser (some' $ symbol '1') "114" @?= Success "4" "11"
    runParser (some' $ symbol '1') "111" @?= Success "" "111"

unit_sepBy :: Assertion
unit_sepBy = do
    runParser (sepBy1 (symbol ',') digit) "" @?= Failure predErrMsg
    runParser (sepBy1 (symbol ',') digit) "1,4," @?= Success "," ['1', '4']
    runParser (sepBy1 (symbol ',') digit) "1,1,4" @?= Success "" ['1', '1', '4']
