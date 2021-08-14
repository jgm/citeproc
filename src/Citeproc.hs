{-# LANGUAGE OverloadedStrings #-}
-- | Process citations using the formatting instructions encoded
-- in a CSL stylesheet.  The library targets version 1.0.1 of the
-- CSL spec: https://docs.citationstyles.org/en/stable/specification.html
module Citeproc
       ( module Citeproc.Types
       , module Citeproc.Style
       , module Citeproc.Locale
       , citeproc
       , Result(..)
       ) where
import Data.Bifunctor (second)
import qualified Data.Text as T
import qualified Data.Set as Set
import Citeproc.Types
import Citeproc.Style
import Citeproc.Locale
import Citeproc.Eval

-- | Process a list of 'Citation's, producing formatted citations
-- and a bibliography according to the rules of a CSL 'Style'.
-- If a 'Lang' is specified, override the style's default locale.
-- To obtain a 'Style' from an XML stylesheet, use
-- 'parseStyle' from "Citeproc.Style".
citeproc :: CiteprocOutput a
         => CiteprocOptions    -- ^ Rendering options
         -> Style a            -- ^ Parsed CSL style
         -> Maybe Lang         -- ^ Overrides default locale for style
         -> [Reference a]      -- ^ List of references (bibliographic data)
         -> [Citation a]       -- ^ List of citations to process
         -> Result a
citeproc opts style mblang refs citations =
  Result{ resultCitations = rCitations
        , resultBibliography = rBibliography
        , resultWarnings = warnings ++ noPrintedFormWarnings }
 where
  rCitations = map ( trimR
                   . localizeQuotes locale
                   . movePunct
                   . renderOutput opts
                   ) citationOs
  rBibliography = map (second
                         ( trimR
                         . localizeQuotes locale
                         . movePunct
                         . renderOutput opts{ linkCitations = False } ))
                      bibliographyOs
  locale = mergeLocales mblang style
  trimR = dropTextWhileEnd (== ' ')
  movePunct = case localePunctuationInQuote locale of
                Just True -> movePunctuationInsideQuotes
                _         -> id
  (citationOs, bibliographyOs, warnings) =
    evalStyle style mblang refs citations
  noPrintedFormWarnings = Set.toList $ mconcat $
                           zipWith npfCitation citations rCitations ++
                           map npfBibentry rBibliography
  npfBibentry (ident, out) =
    if out == mempty
       then Set.singleton $ "Bibliography entry with no printed form: " <>
                               ident
       else mempty
  npfCitation citation res =
    if res == mempty
       then Set.singleton $ "Citation with no printed form: "  <>
                                T.intercalate ","
                                (map (unItemId . citationItemId)
                                  (citationItems citation))
       else mempty
