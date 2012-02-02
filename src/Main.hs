module Main (main) where

import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Text.Pandoc (Pandoc(..), Block(..), readMarkdown, defaultParserState)

main :: IO ()
main = getArgs >>= \args -> case args of
  -- GHC calls unlit like so:
  --
  -- > unlit -h label Foo.lhs /tmp/somefile
  --
  -- The label is meant to be used in line pragmas, like so:
  --
  -- #line 1 "label"
  --
  -- But as Pandoc does not provide location information, we have no use for
  -- it, yet.
  ["-h", _, infile, outfile] ->
    fmap unlit (readFile infile) >>= writeFile outfile
  _ -> do
    name <- getProgName
    hPutStrLn stderr ("usage: " ++ name ++ " -h label infile outfile")
    exitFailure

unlit :: String -> String
unlit input = unlines [x | CodeBlock (_, c, _) x <- nodes, isLiterate c]
  where
    isLiterate c = "literate" `elem` c && "haskell" `elem` c
    Pandoc _ nodes = readMarkdown defaultParserState input
