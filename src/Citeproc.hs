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
import Data.Text (Text)
import Citeproc.Types
import Citeproc.Style
import Citeproc.Locale
import Citeproc.Eval
import Citeproc.CslJson

-- | Result of citation processing.
data Result a =
  Result
  { resultCitations     :: [a]          -- ^ List of formatted citations
                    -- corresponding to the citations given to 'citeproc'
  , resultBibliography  :: [(Text, a)]  -- ^ List of formatted bibliography
                    -- entries (if the style calls for a bibliography),
                    -- each a pair consisting of the item identifier and
                    -- the formatted entry
  , resultWarnings      :: [Text]       -- ^ Warnings from citation processing
  } deriving (Show)


-- | Process a list of 'Citation's, producing formatted citations
-- and a bibliography according to the rules of a CSL 'Style'.
-- If a 'Lang' is specified, override the style's default locale.
-- To obtain a 'Style' from an XML stylesheet, use
-- 'parseStyle' from 'Citeproc.Style'.
citeproc :: CiteprocOutput a
         => CiteprocOptions
         -> Style a
         -> Maybe Lang
         -> [Reference a]
         -> [Citation a]
         -> Result a
citeproc opts style mblang refs citations =
  Result{ resultCitations =
            map (trimR . movePunct . renderOutput opts) citationOs
        , resultBibliography =
            map (\(ident, out) ->
                  (ident, trimR . movePunct .
                    renderOutput opts{ linkCitations = False } $ out))
                  bibliographyOs
        , resultWarnings = warnings }
 where
  locale = mergeLocales mblang style
  trimR = dropTextWhileEnd (== ' ')
  movePunct = case localePunctuationInQuote locale of
                Just True -> movePunctuationInsideQuotes
                _         -> id
  (citationOs, bibliographyOs, warnings) =
    evalStyle style mblang refs citations

