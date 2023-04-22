module Main where

import Toast.AbsToast
import Toast.ErrM
import Toast.LexToast
import Toast.ParToast
import Toast.PrintToast
import Interpreter

import System.Environment ( getArgs, getProgName )


runProgram :: String -> IO ()
runProgram s = case pProgram (myLexer s) of
    Ok e -> runInterpreter e
    Left err -> putStrLn err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= runProgram
    -- fs -> mapM_ runFile fs

