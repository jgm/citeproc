{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Citeproc.Locale
  ( parseLocale,
    getLocale,
    getPrimaryDialect,
    lookupQuotes
  )
where
import Citeproc.Types
import Citeproc.Element (runElementParser, pLocale)
import Citeproc.Data (localeFiles)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Text.XML as X
import System.FilePath (takeExtension, dropExtension)
import qualified Data.Text as T
import Data.Default (def)
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8)
import Control.Applicative ((<|>))

-- | Parse a CSL locale definition (XML).  For information about
-- the format, see
-- <https://docs.citationstyles.org/en/stable/translating-locale-files.html>.
parseLocale :: Text -> Either CiteprocError Locale
parseLocale t =
  case X.parseText def $ TL.fromStrict t of
       Left e -> Left $ CiteprocXMLError (T.pack (show e))
       Right n -> runElementParser $ pLocale $ X.documentRoot n

primaryDialectMap :: M.Map Text (Maybe Text)
primaryDialectMap = M.fromList
  [ ("af", Just "ZA"),
    ("ar", Nothing),
    ("bg", Just "BG"),
    ("ca", Just "AD"),
    ("cs", Just "CZ"),
    ("cy", Just "GB"),
    ("da", Just "DK"),
    ("de", Just "DE"),
    ("el", Just "GR"),
    ("en", Just "US"),
    ("es", Just "ES"),
    ("et", Just "EE"),
    ("eu", Nothing),
    ("fa", Just "IR"),
    ("fi", Just "FI"),
    ("fr", Just "FR"),
    ("he", Just "IL"),
    ("hr", Just "HR"),
    ("hu", Just "HU"),
    ("id", Just "ID"),
    ("is", Just "IS"),
    ("it", Just "IT"),
    ("ja", Just "JP"),
    ("km", Just "KH"),
    ("ko", Just "KR"),
    ("la", Nothing),
    ("lt", Just "LT"),
    ("lv", Just "LV"),
    ("mn", Just "MN"),
    ("nb", Just "NO"),
    ("nl", Just "NL"),
    ("nn", Just "NO"),
    ("pl", Just "PL"),
    ("pt", Just "PT"),
    ("ro", Just "RO"),
    ("ru", Just "RU"),
    ("sk", Just "SK"),
    ("sl", Just "SI"),
    ("sr", Just "RS"),
    ("sv", Just "SE"),
    ("th", Just "TH"),
    ("tr", Just "TR"),
    ("uk", Just "UA"),
    ("vi", Just "VN"),
    ("zh", Just "CN")
    ]

-- | Retrieves the "primary dialect" corresponding to a language,
-- e.g. "lt-LT" for "lt".
getPrimaryDialect :: Lang -> Maybe Lang
getPrimaryDialect lang =
  case M.lookup (langLanguage lang) primaryDialectMap of
    Nothing       -> Nothing
    Just mbregion -> Just $ lang{ langRegion = mbregion }


locales :: M.Map Text (Either CiteprocError Locale)
locales = foldr go mempty localeFiles
  where
   go (fp, bs) m
     | takeExtension fp == ".xml"
     = let lang = T.pack $ dropExtension fp
       in M.insert lang (parseLocale $ decodeUtf8 bs) m
     | otherwise = m

-- | Retrieves the locale defined for the specified language.
-- Implements the locale fallback algorithm described in the CSL 1.0.1 spec.
getLocale :: Lang -> Either CiteprocError Locale
getLocale lang =
  let toCode l = langLanguage l <> maybe "" ("-"<>) (langRegion l)
   in case M.lookup (toCode lang) locales
          <|> (getPrimaryDialect lang >>=
               (\l -> M.lookup (toCode l) locales)) of
        Just loc -> loc
        Nothing  -> Left $ CiteprocLocaleNotFound $ renderLang lang

lookupTerm :: Locale -> Text -> Maybe Text
lookupTerm locale termname = do
  let terms = localeTerms locale
  case M.lookup termname terms of
     Just ((_,t):_) -> Just t
     _              -> Nothing

lookupQuotes :: Locale -> ((Text, Text), (Text, Text))
lookupQuotes locale = ((outerOpen, outerClose), (innerOpen, innerClose))
 where
  outerOpen = fromMaybe "\x201C" $ lookupTerm locale "open-quote"
  outerClose = fromMaybe "\x201D" $ lookupTerm locale "close-quote"
  innerOpen = fromMaybe "\x2018" $ lookupTerm locale "open-inner-quote"
  innerClose = fromMaybe "\x2019" $ lookupTerm locale "close-inner-quote"

