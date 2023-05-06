module Main where
import Process (transpile, mdBookProcessor, latexProcessor)
import System.Environment (getArgs)

main :: IO ()
main = let p = latexProcessor in do
  [src, dst] <- getArgs
  transpile p src dst