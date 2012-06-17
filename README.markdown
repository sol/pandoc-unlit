# Literate Haskell support for Pandoc's markdown flavor

`pandoc-unlit` makes it convenient to write literate Haskell programs with
Pandoc's [delimited code blocks][delimited-code-blocks].  All code blocks with
classes `literate` and `haskell` are part of the final program.

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

You can load this file into `ghci` like so:

    ghci -pgmL pandoc-unlit Main.lhs

Or better yet, pipe the required flag into a `.ghci` file, and forget about it:

    echo ':set -pgmL pandoc-unlit' >> .ghci
    ghci Main.lhs

## A note about runhaskell

For `ghc` (and hence `ghci`) both, `-pgmL pandoc-unlit` and `-pgmLpandoc-unlit`
work; for `runhaskell` only the latter works.

## Some other niceties

With `pandoc-unlit` you can attach arbitrary classes to your code blocks, this
allows you to customize the generated HTML with CSS.  You can e.g. hide
boilerplate code.

But what if I want plain old literate Haskell, that works without
`pandoc-unlit`?

Pandoc has [support for literate Haskell programs] [literate-haskell-support]
that use either "bird tracks" or code surrounded by `\begin{code}` and
`\end{code}`.  And it can convert your code into that form.

    pandoc Main.lhs -t markdown+lhs    # this produces invalid markdown

<!--
And what if I need plain old markdown, without any pandoc specific extensions?
`pandoc-to-markdown` is for you.
-->

## Known limitations

The line numbers that GHC reports on errors are bogus.

[delimited-code-blocks]: http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks
[literate-haskell-support]: http://johnmacfarlane.net/pandoc/README.html#literate-haskell-support
