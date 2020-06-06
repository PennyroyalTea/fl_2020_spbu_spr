module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), simplify, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent, parseStr)
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_simplify :: Assertion
unit_simplify = do
  simplify "1+0" @?= Just "1"
  simplify "x+y" @?= Just "x+y"
  simplify "1*x" @?= Just "x"
  simplify "(a+b)*c" @?= Just "(a+b)*c"
  simplify "(x+y)*(1+0)" @?= Just "x+y"
  simplify "2*2+2" @?= Just "6"