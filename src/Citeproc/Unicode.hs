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
import Text.Collate.Lang (Lang(..), parseLang, renderLang, lookupLang)
#ifdef MIN_VERSION_text_icu
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Collate as ICUC
#else
import qualified Text.Collate as U
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
comp mblang = ICU.collate collator
 where
  collator = ICU.collatorWith (toICULocale mblang) [ICUC.AlternateHandling alt]
  alt = case mblang >>= lookup "u" . langExtensions >>= lookup "ka" of
          Just "noignore" -> ICUC.NonIgnorable
          _ -> ICUC.Shifted
#else
comp mblang = U.collate coll
 where
  lang = fromMaybe (Lang "" Nothing Nothing [] [] []) mblang
  coll = case lookup "u" (langExtensions lang) >>= lookup "ka" of
           -- default to Shifted variable weighting, unless a variable
           -- weighting is explicitly specified with the ka keyword:
           Nothing -> U.setVariableWeighting U.Shifted $ U.collatorFor lang
           Just _  -> U.collatorFor lang
#endif
