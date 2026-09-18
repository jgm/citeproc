{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for backend functions not covered by the file-based
-- CSL test suite (which only exercises the CslJson backend).
module Main (main) where
import Citeproc.Types (CiteprocOutput(..))
import Citeproc.Pandoc ()
import Text.Pandoc.Builder
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  let failures = [ c | c@(_, actual, expected) <- testCases
                     , actual /= expected ]
  mapM_ report failures
  printf "%d of %d unit tests passed.\n"
    (length testCases - length failures) (length testCases)
  if null failures
     then exitSuccess
     else exitFailure
 where
  report (name, actual, expected) = do
    putStrLn $ "[FAILED] " <> name
    putStrLn $ "  expected: " <> show (toList expected)
    putStrLn $ "  actual:   " <> show (toList actual)

testCases :: [(String, Inlines, Inlines)]
testCases =
  -- dropTextWhileEnd must trim from the *last* Str of a trailing
  -- nested inline, not the first:
  [ ("dropTextWhileEnd: trims last Str inside trailing nested inline",
     dropTextWhileEnd (== '.') (fromList [Emph [Str "a.", Str "b."]]),
     fromList [Emph [Str "a.", Str "b"]])
  -- a Space that doesn't match the predicate must stop the trimming:
  , ("dropTextWhileEnd: stops at non-matching Space",
     dropTextWhileEnd (== '.') (fromList [Str "etc.", Space, Str "."]),
     fromList [Str "etc.", Space, Str ""])
  -- as long as everything so far has been dropped, trimming continues
  -- past nesting boundaries:
  , ("dropTextWhileEnd: continues across nesting while dropping",
     dropTextWhileEnd (== '.') (fromList [Emph [Str "a.", Str "."], Str "."]),
     fromList [Emph [Str "a", Str ""], Str ""])
  -- trailing space trimming (the trimR use case in Citeproc.hs):
  , ("dropTextWhileEnd: drops a trailing Space",
     dropTextWhileEnd (== ' ') (fromList [Str "hi", Space]),
     fromList [Str "hi", Str ""])
  , ("dropTextWhileEnd: single Str inside trailing nested inline",
     dropTextWhileEnd (== '.') (fromList [Str "x ", Emph [Str "Title."]]),
     fromList [Str "x ", Emph [Str "Title"]])
  ]
