Some markdown goes here.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .literate}
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = getArgs >>= \args -> case args of
    [n] -> print $ (fac . read) n
    _ -> do
      name <- getProgName
      hPutStrLn stderr ("usage: " ++ name ++ " N")
      exitFailure

fac :: Integer -> Integer
fac n = product [1..n]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
