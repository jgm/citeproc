{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for backend functions not covered by the file-based
-- CSL test suite (which only exercises the CslJson backend's HTML
-- rendering).
module Main (main) where
import Citeproc.Types (CiteprocOutput(..))
import Citeproc.CslJson (CslJson(..), cslJsonToJson, parseCslJson)
import Citeproc.Pandoc ()
import Text.Pandoc.Builder
import Data.Aeson (Value(..), object, toJSON)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  let failures = mapMaybe check inlineCases ++ mapMaybe check jsonCases
                   ++ mapMaybe check quoteCases
  let total = length inlineCases + length jsonCases + length quoteCases
  mapM_ report failures
  printf "%d of %d unit tests passed.\n" (total - length failures) total
  if null failures
     then exitSuccess
     else exitFailure
 where
  check (name, actual, expected)
    | actual == expected = Nothing
    | otherwise          = Just (name, show expected, show actual)
  report (name, expected, actual) = do
    putStrLn $ "[FAILED] " <> name
    putStrLn $ "  expected: " <> expected
    putStrLn $ "  actual:   " <> actual

-- dropTextWhileEnd on pandoc Inlines:
inlineCases :: [(String, Inlines, Inlines)]
inlineCases =
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

-- flip-flop formatting state in cslJsonToJson's JSON output:
jsonCases :: [(String, [Value], [Value])]
jsonCases =
  [ ("cslJsonToJson: bold flip-flops in nested bold",
     jsonOf "<b>One <b>Two <b>Three</b></b></b>",
     [fmt "bold" [String "One ",
        fmt "no-bold" [String "Two ",
          fmt "bold" [String "Three"]]]])
  , ("cslJsonToJson: bold state unaffected by italic context",
     jsonOf "<i>One <b>Two</b></i>",
     [fmt "italics" [String "One ", fmt "bold" [String "Two"]]])
  , ("cslJsonToJson: italics flip-flop in nested italics",
     jsonOf "<i>One <i>Two <i>Three</i></i></i>",
     [fmt "italics" [String "One ",
        fmt "no-italics" [String "Two ",
          fmt "italics" [String "Three"]]]])
  , ("cslJsonToJson: small-caps flip-flop in nested small-caps",
     jsonOf "<span style=\"font-variant:small-caps;\">One \
            \<span style=\"font-variant:small-caps;\">Two \
            \<span style=\"font-variant:small-caps;\">Three\
            \</span></span></span>",
     [fmt "small-caps" [String "One ",
        fmt "no-small-caps" [String "Two ",
          fmt "small-caps" [String "Three"]]]])
  ]
 where
  jsonOf = cslJsonToJson . parseCslJson mempty
  fmt :: Text -> [Value] -> Value
  fmt f xs = object [("format", String f), ("contents", toJSON xs)]

-- punctuation moving (punctuation-in-quote) on the CslJson backend:
quoteCases :: [(String, CslJson Text, CslJson Text)]
quoteCases =
  -- the basic case: movable punctuation after a quoted span moves inside:
  [ ("punctuationInsideQuotes: moves comma following a quote inside",
     movePunctuationInsideQuotes
       (CslQuoted (CslText "Hi") <> CslText ", she said"),
     CslQuoted (CslText "Hi" <> CslText ",") <> CslText " she said")
  -- when the text after a quoted span starts with something unmovable,
  -- punctuation moving must still be applied *inside* the quoted span:
  , ("punctuationInsideQuotes: processes nested content of a quote",
     movePunctuationInsideQuotes
       (CslQuoted (CslQuoted (CslText "inner") <> CslText ", outer")
          <> CslText " rest"),
     CslQuoted (CslQuoted (CslText "inner" <> CslText ",") <> CslText " outer")
       <> CslText " rest")
  ]
