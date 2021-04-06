{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Citeproc.Unicode
  ( Lang(..),
    parseLang,
    renderLang,
    toUpper,
    toLower,
    comp
  )
where
#ifdef MIN_VERSION_text_icu
import qualified Data.Text.ICU as ICU
#else
import qualified UnicodeCollation as U
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..))

-- | A parsed IETF language tag, with language and optional variant.
-- For example, @Lang "en" (Just "US")@ corresponds to @en-US@.
data Lang = Lang{ langLanguage :: Text
                , langVariant  :: Maybe Text }
  deriving (Show, Eq, Ord)

instance ToJSON Lang where
  toJSON = toJSON . renderLang

instance FromJSON Lang where
  parseJSON = fmap parseLang . parseJSON

-- | Render a 'Lang' an an IETF language tag.
renderLang :: Lang -> Text
renderLang (Lang l Nothing)  = l
renderLang (Lang l (Just v)) = l <> "-" <> v

-- | Parse an IETF language tag.
parseLang :: Text -> Lang
parseLang t = Lang l (snd <$> T.uncons v)
  where
   (l,v) = T.break (\c -> c == '-' || c == '_') t

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
  case mblang of
    Just (Lang "tr" _) -> T.map (\c -> case c of
                                        'i' -> 'İ'
                                        'ı' -> 'I'
                                        _   -> c)
    _                  -> id
#endif

toLower :: Maybe Lang -> Text -> Text
#ifdef MIN_VERSION_text_icu
toLower mblang =
   ICU.toLower (toICULocale mblang)
#else
toLower mblang = T.toLower .
  case mblang of
    Just (Lang "tr" _) -> T.map (\c -> case c of
                                        'İ' -> 'i'
                                        'I' -> 'ı'
                                        _   -> c)
    _                  -> id
#endif

comp :: Maybe Lang -> Text -> Text -> Ordering
#ifdef MIN_VERSION_text_icu
comp mblang = ICU.collate (ICU.collator (toICULocale mblang))
#else
comp mblang = U.collate collator
 where
  collator = U.mkCollator U.collationOptions{
                          U.optCollation = maybe U.rootCollation
                               (U.localizedCollation . renderLang) mblang,
                          U.optVariableWeighting = U.Shifted,
                          U.optFrenchAccents =
                            case mblang of
                              Just (Lang "fr" _) -> True
                              _                  -> False }
#endif

