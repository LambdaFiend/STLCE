module Main where

import Syntax
import Semantics
import Typing
import Display
import Lexer
import Parser
import Helper

main :: IO ()
main = do
  txt <- readFile "_Testes/teste.txt"
  let tok = alexScanTokens txt
  putStrLn $ show tok
  putStrLn ""
  let ast = deriveTm' $ genIndex' $ parser tok
  putStrLn $ show $ ast
  putStrLn ""
  let str = showTm' ast
  putStrLn str
  putStrLn ""
  let typ = typeOf' ast
  putStrLn $ showType typ
