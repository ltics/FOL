{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Lens
import System.Environment
import Core(toplevel)
import Parser(parseExpr)
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Control.Exception as E

process :: String -> IO ()
process expr = E.catch (putStrLn . show . toplevel . parseExpr $ expr)
                       (\(E.ErrorCall e) -> putStrLn e)

loop :: InputT IO ()
loop = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> (liftIO $ process input) >> loop

main :: IO ()
main = do
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then runInputT defaultSettings loop
               else putStrLn "unsupported mode"
    Nothing -> do
      input <- getContents
      process input
