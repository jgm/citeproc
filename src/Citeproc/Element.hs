{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Citeproc.Element
  ( pLocale
  , pDate
  , Attributes(..)
  , lookupAttribute
  , ElementParser
  , runElementParser
  , parseFailure
  , getChildren
  , allChildren
  , getAttributes
  , getNameAttributes
  , getFormatting
  , getTextContent
  )
where
import Citeproc.Types
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as M
import qualified Text.XML as X
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

newtype Attributes = Attributes (M.Map Text Text)
  deriving (Show, Semigroup, Monoid, Eq)

lookupAttribute :: Text -> Attributes -> Maybe Text
lookupAttribute key (Attributes kvs) = M.lookup key kvs

type ElementParser = ReaderT (M.Map X.Name Text) (Except CiteprocError)

runElementParser :: ElementParser a -> Either CiteprocError a
runElementParser p = runExcept (runReaderT p mempty)

parseFailure :: String -> ElementParser a
parseFailure s = lift $ throwE (CiteprocParseError $ T.pack s)

getChildren :: Text -> X.Element -> [X.Element]
getChildren name el = [e | X.NodeElement e <- X.elementNodes el
                         , X.nameLocalName (X.elementName e) == name]

allChildren :: X.Element -> [X.Element]
allChildren el = [e | X.NodeElement e <- X.elementNodes el]

getAttributes :: X.Element -> Attributes
getAttributes =
  Attributes . M.mapKeys X.nameLocalName . X.elementAttributes

-- Like getAttributes but incorporates inheritable attributes.
getNameAttributes :: X.Element -> ElementParser Attributes
getNameAttributes node = do
  nameattr <- ask
  let xattr = X.elementAttributes node <> nameattr
  return $ Attributes $ M.mapKeys X.nameLocalName xattr

getFormatting :: Attributes -> Formatting
getFormatting attr =
   Formatting
     { formatLang = Nothing
     , formatFontStyle =
          case lookupAttribute "font-style" attr of
            Just "italic"  -> Just ItalicFont
            Just "oblique" -> Just ObliqueFont
            Just "normal"  -> Just NormalFont
            _              -> Nothing
      , formatFontVariant =
          case lookupAttribute "font-variant" attr of
            Just "small-caps" -> Just SmallCapsVariant
            Just "normal"     -> Just NormalVariant
            _                 -> Nothing
      , formatFontWeight =
          case lookupAttribute "font-weight" attr of
            Just "bold"   -> Just BoldWeight
            Just "light"  -> Just LightWeight
            Just "normal" -> Just NormalWeight
            _             -> Nothing
      , formatTextDecoration =
          case lookupAttribute "text-decoration" attr of
            Just "underline" -> Just UnderlineDecoration
            Just "none"      -> Just NoDecoration
            _                -> Nothing
      , formatVerticalAlign =
          case lookupAttribute "vertical-align" attr of
            Just "sup"       -> Just SupAlign
            Just "sub"       -> Just SubAlign
            Just "baseline"  -> Just BaselineAlign
            _                -> Nothing
      , formatPrefix = lookupAttribute "prefix" attr
      , formatSuffix = lookupAttribute "suffix" attr
      , formatDisplay =
          case lookupAttribute "display" attr of
            Just "block"        -> Just DisplayBlock
            Just "left-margin"  -> Just DisplayLeftMargin
            Just "right-inline" -> Just DisplayRightInline
            Just "indent"       -> Just DisplayIndent
            _                   -> Nothing
      , formatTextCase =
          case lookupAttribute "text-case" attr of
            Just "lowercase"        -> Just Lowercase
            Just "uppercase"        -> Just Uppercase
            Just "capitalize-first" -> Just CapitalizeFirst
            Just "capitalize-all"   -> Just CapitalizeAll
            Just "sentence"         -> Just SentenceCase
            Just "title"            -> Just TitleCase
            _                       -> Nothing
      , formatDelimiter = lookupAttribute "delimiter" attr
      , formatStripPeriods =
          lookupAttribute "strip-periods" attr == Just "true"
      , formatQuotes =
          lookupAttribute "quotes" attr == Just "true"
      , formatAffixesInside = False -- should be true for layout only
      }


getTextContent :: X.Element -> Text
getTextContent e = mconcat [t | X.NodeContent t <- X.elementNodes e]

pLocale :: X.Element -> ElementParser Locale
pLocale node = do
  let attr = getAttributes node
  lang <- case lookupAttribute "lang" attr of
            Nothing -> return Nothing
            Just l  -> either parseFailure (return . Just) $ parseLang l
  let styleOpts = mconcat . map getAttributes $
                      getChildren "style-options" node
  let addDateElt e m =
        case e of
          Element (EDate _ dateType _ _) _ -> M.insert dateType e m
          _ -> error "pDate returned an element other than EDate"
  dateElts <- foldr addDateElt mempty <$> mapM pDate (getChildren "date" node)
  let termNodes = concatMap (getChildren "term") (getChildren "terms" node)
  terms <- foldM parseTerm mempty termNodes
  return $
    Locale
    { localeLanguage               = lang
    , localePunctuationInQuote     = (== "true") <$>
               lookupAttribute "punctuation-in-quote" styleOpts
    , localeLimitDayOrdinalsToDay1 = (== "true") <$>
               lookupAttribute "limit-day-ordinals-to-day-1" styleOpts
    , localeDate                   = dateElts
    , localeTerms                  = terms
    }

parseTerm :: M.Map Text [(Term, Text)]
          -> X.Element
          -> ElementParser (M.Map Text [(Term, Text)])
parseTerm m node = do
  let attr = getAttributes node
  name <- case lookupAttribute "name" attr of
                Just n   -> return n
                Nothing  -> parseFailure "Text node has no name attribute"
  let single = mconcat $ map getTextContent $ getChildren "single" node
  let multiple = mconcat $ map getTextContent $ getChildren "multiple" node
  let txt = getTextContent node
  let form = case lookupAttribute "form" attr of
               Just "short"      -> Short
               Just "verb"       -> Verb
               Just "verb-short" -> VerbShort
               Just "symbol"     -> Symbol
               _                 -> Long
  let gender = case lookupAttribute "gender" attr of
                 Just "masculine"  -> Just Masculine
                 Just "feminine"   -> Just Feminine
                 _                 -> Nothing
  let genderForm = case lookupAttribute "gender-form" attr of
                     Just "masculine"  -> Just Masculine
                     Just "feminine"   -> Just Feminine
                     _                 -> Nothing
  let match = case lookupAttribute "match" attr of
                     Just "last-digit"      -> Just LastDigit
                     Just "last-two-digits" -> Just LastTwoDigits
                     Just "whole-number"    -> Just WholeNumber
                     _                      -> Nothing
  let term = Term
        { termName          = name
        , termForm          = form
        , termNumber        = Nothing
        , termGender        = gender
        , termGenderForm    = genderForm
        , termMatch         = match
        }
  let addToList x Nothing   = Just [x]
      addToList x (Just xs) = Just (x:xs)
  if T.null single
     then return $ M.alter (addToList (term, txt)) (termName term) m
     else do
       let term_single = term{ termNumber = Just Singular }
       let term_plural = term{ termNumber = Just Plural }
       return $ M.alter
          (addToList (term_single, single) .
           addToList (term_plural, multiple)) (termName term) m

pDate :: X.Element -> ElementParser (Element a)
pDate node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let form = lookupAttribute "form" attr
  let var = toVariable $ fromMaybe mempty $ lookupAttribute "variable" attr
  let showDateParts = case lookupAttribute "date-parts" attr of
                        Just "year-month-day" -> Just YearMonthDay
                        Just "year-month"     -> Just YearMonth
                        Just "year"           -> Just Year
                        _                     -> Nothing

  dps <- mapM parseDatePartElement (getChildren "date-part" node)
  let dateType = case form of
                      Just "numeric" -> LocalizedNumeric
                      Just "text"    -> LocalizedText
                      _              -> NonLocalized
  return $ Element (EDate var dateType showDateParts dps) formatting

parseDatePartElement :: X.Element -> ElementParser DP
parseDatePartElement node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let name = case lookupAttribute "name" attr of
               Just "day"   -> DPDay
               Just "month" -> DPMonth
               _            -> DPYear
  let form = case lookupAttribute "form" attr of
               Just "numeric"                -> DPNumeric
               Just "numeric-leading-zeros"  -> DPNumericLeadingZeros
               Just "ordinal"                -> DPOrdinal
               Just "long"                   -> DPLong
               Just "short"                  -> DPShort
               _ | name == DPDay             -> DPNumeric
                 | otherwise                 -> DPLong
  let rangeDelim = fromMaybe "–" $ lookupAttribute "range-delimiter" attr
  return $ DP name form rangeDelim formatting

