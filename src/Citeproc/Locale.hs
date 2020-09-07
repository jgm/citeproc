{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Citeproc.Locale
  ( parseLocale,
    getLocale,
    getPrimaryDialect
  )
where
import Citeproc.Types
import Citeproc.Element (runElementParser, pLocale)
import Citeproc.Data (localeFiles)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Text.XML as X
import System.FilePath (takeExtension, dropExtension)
import qualified Data.Text as T
import Data.Default (def)
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8)
import Control.Applicative ((<|>))

parseLocale :: Text -> Either CiteprocError Locale
parseLocale t =
  case X.parseText def $ TL.fromStrict t of
       Left e -> Left $ CiteprocXMLError (T.pack (show e))
       Right n -> runElementParser $ pLocale $ X.documentRoot n

primaryDialectMap :: M.Map Text Text
primaryDialectMap = M.fromList
  [ ("af", "af-ZA"),
    ("ar", "ar"),
    ("bg", "bg-BG"),
    ("ca", "ca-AD"),
    ("cs", "cs-CZ"),
    ("cy", "cy-GB"),
    ("da", "da-DK"),
    ("de", "de-DE"),
    ("el", "el-GR"),
    ("en", "en-US"),
    ("es", "es-ES"),
    ("et", "et-EE"),
    ("eu", "eu"),
    ("fa", "fa-IR"),
    ("fi", "fi-FI"),
    ("fr", "fr-FR"),
    ("he", "he-IL"),
    ("hr", "hr-HR"),
    ("hu", "hu-HU"),
    ("id", "id-ID"),
    ("is", "is-IS"),
    ("it", "it-IT"),
    ("ja", "ja-JP"),
    ("km", "km-KH"),
    ("ko", "ko-KR"),
    ("la", "la"),
    ("lt", "lt-LT"),
    ("lv", "lv-LV"),
    ("mn", "mn-MN"),
    ("nb", "nb-NO"),
    ("nl", "nl-NL"),
    ("nn", "nn-NO"),
    ("pl", "pl-PL"),
    ("pt", "pt-PT"),
    ("ro", "ro-RO"),
    ("ru", "ru-RU"),
    ("sk", "sk-SK"),
    ("sl", "sl-SI"),
    ("sr", "sr-RS"),
    ("sv", "sv-SE"),
    ("th", "th-TH"),
    ("tr", "tr-TR"),
    ("uk", "uk-UA"),
    ("vi", "vi-VN"),
    ("zh", "zh-CN")
    ]

getPrimaryDialect :: Lang -> Maybe Lang
getPrimaryDialect l =
  parseLang <$> M.lookup (langLanguage l) primaryDialectMap

locales :: M.Map Lang (Either CiteprocError Locale)
locales = foldr go mempty localeFiles
  where
   go (fp, bs) m
     | takeExtension fp == ".xml"
     = let lang = parseLang $ T.pack $ dropExtension fp
       in M.insert lang (parseLocale $ decodeUtf8 bs) m
     | otherwise = m

-- see locale fallback algorithm in CSL 1.0.1 spec
getLocale :: Lang -> Either CiteprocError Locale
getLocale lang =
  case M.lookup lang locales
        <|>
        (getPrimaryDialect lang >>= \lang' -> M.lookup lang' locales) of
    Just loc -> loc
    Nothing -> Left $ CiteprocLocaleNotFound $ renderLang lang

