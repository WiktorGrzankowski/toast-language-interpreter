module Main where

import Toast.AbsToast
import Toast.ErrM
import Toast.LexToast
import Toast.ParToast
import Toast.PrintToast
import Interpreter

import System.Environment ( getArgs, getProgName )


runFromString :: String -> IO ()
runFromString s = case pProgram (myLexer s) of
    Ok e -> runInterpreter e
    Left err -> putStrLn err

runFromFile :: FilePath -> IO ()
runFromFile file = readFile file >>= runFromString

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= runFromString
    files -> mapM_ runFromFile files

