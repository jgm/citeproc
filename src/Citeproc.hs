module Citeproc
       ( module Citeproc.Types
       , module Citeproc.Style
       , module Citeproc.Locale
       , module Citeproc.CslJson
       , citeproc
       , mergeLocales
       ) where
import Citeproc.Types
import Citeproc.Style
import Citeproc.Locale
import Citeproc.Eval
import Citeproc.CslJson

citeproc :: CiteprocOutput a
         => CiteprocOptions
         -> Style a
         -> Maybe Lang
         -> [Reference a]
         -> [Citation a]
         -> Result a
citeproc opts style mblang refs citations =
  Result{ resultCitations =
            map (trimR . movePunct . renderOutput opts mblang) citationOs
        , resultBibliography =
            map (\(ident, out) ->
                  (ident, trimR . movePunct .
                    renderOutput opts{ linkCitations = False } mblang $ out))
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

