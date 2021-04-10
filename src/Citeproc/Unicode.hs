{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Citeproc.Unicode
  ( Lang(..),
    parseLang,
    renderLang,
    lookupLang,
    toUpper,
    toLower,
    comp
  )
where
import UnicodeCollation (Lang(..), parseLang, renderLang, lookupLang)
#ifdef MIN_VERSION_text_icu
import qualified Data.Text.ICU as ICU
#else
import qualified UnicodeCollation as U
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

#ifdef MIN_VERSION_text_icu
toICULocale :: Maybe Lang -> ICU.LocaleName
toICULocale Nothing = ICU.Current
toICULocale (Just l) = ICU.Locale (T.unpack (renderLang l))
#endif

toUpper :: Maybe Lang -> Text -> Text
#ifdef MIN_VERSION_text_icu
toUpper mblang =
   ICU.toUpper (toICULocale mblang)
#else
toUpper mblang = T.toUpper .
  case langLanguage <$> mblang of
    Just "tr" -> T.map (\c -> case c of
                                'i' -> 'İ'
                                'ı' -> 'I'
                                _   -> c)
    _         -> id
#endif

toLower :: Maybe Lang -> Text -> Text
#ifdef MIN_VERSION_text_icu
toLower mblang =
   ICU.toLower (toICULocale mblang)
#else
toLower mblang = T.toLower .
  case langLanguage <$> mblang of
    Just "tr" -> T.map (\c -> case c of
                                'İ' -> 'i'
                                'I' -> 'ı'
                                _   -> c)
    _         -> id
#endif

comp :: Maybe Lang -> Text -> Text -> Ordering
#ifdef MIN_VERSION_text_icu
comp mblang = ICU.collate (ICU.collator (toICULocale mblang))
#else
comp mblang =
  let coll = U.collatorFor
               (fromMaybe (Lang "en" Nothing (Just "US") [] [] []) mblang)
   in U.collate coll
#endif
