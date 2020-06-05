module Main where

import           Combinators (runParser, Result (..))
import           Expr        (simplify, parseExpr)
import           Text.Printf (printf)

fm0 :: String
fm0 = "x"

fm1 :: String
fm1 = "0+x"

fm2 :: String
fm2 = "0*x+1*y"

fm3 :: String
fm3 = "x*y*1*1*1+z*0*0*0"

run :: String -> IO ()
run input = do
    putStrLn ""
    putStrLn $ printf "Input:\t%s" input
    case runParser parseExpr input of
      Success rest tree ->
        putStrLn $ printf "Rest:\t%s\nResult:\t%s\nTree:%s\n" (show rest) (show $ simplify input) (show tree)
      Failure e -> putStrLn $ "Parsing failed:\n" ++ show e

main :: IO ()
main = do
    run fm0
    run fm1
    run fm2
    run fm3