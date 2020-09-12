{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Citeproc.Types
  ( CiteprocOptions(..)
  , defaultCiteprocOptions
  , CiteprocOutput(..)
  , addFormatting
  , CiteprocError(..)
  , prettyCiteprocError
  , Result(..)
  , ItemId(..)
  , CitationItem(..)
  , CitationItemType(..)
  , Citation(..)
  , ElementType(..)
  , Element(..)
  , NumberForm(..)
  , Pluralize(..)
  , DateType(..)
  , Date(..)
  , rawDateEDTF
  , DateParts(..)
  , ShowDateParts(..)
  , DPName(..)
  , DPForm(..)
  , DP(..)
  , VariableForm(..)
  , TextType(..)
  , NameFormat(..)
  , defaultNameFormat
  , NameAsSortOrder(..)
  , NamesFormat(..)
  , NameForm(..)
  , Name(..)
  , extractParticles
  , isByzantineName
  , DelimiterPrecedes(..)
  , Condition(..)
  , Position(..)
  , Match(..)
  , Formatting(..)
  , FontStyle(..)
  , FontVariant(..)
  , FontWeight(..)
  , TextDecoration(..)
  , VerticalAlign(..)
  , DisplayStyle(..)
  , TextCase(..)
  , Attributes(..)
  , lookupAttribute
  , DemoteNonDroppingParticle(..)
  , StyleOptions(..)
  , SubsequentAuthorSubstitute(..)
  , SubsequentAuthorSubstituteRule(..)
  , SecondFieldAlign(..)
  , PageRangeFormat(..)
  , Style(..)
  , TermMatch(..)
  , TermGender(..)
  , TermNumber(..)
  , TermForm(..)
  , Term(..)
  , emptyTerm
  , SortDirection(..)
  , SortKey(..)
  , SortKeyValue(..)
  , LayoutOptions(..)
  , Collapsing(..)
  , Layout(..)
  , DisambiguationStrategy(..)
  , GivenNameDisambiguationRule(..)
  , Lang(..)
  , parseLang
  , renderLang
  , Locale(..)
  , DisambiguationData(..)
  , NameHints(..)
  , Reference(..)
  , ReferenceMap(..)
  , makeReferenceMap
  , lookupReference
  , Val(..)
  , lookupVariable
  , Output(..)
  , Tag(..)
  , outputToText
  , renderOutput
  , grouped
  , formatted
  , readAsInt
  , variableType
  , VariableType(..)
  , Abbreviations
  , lookupAbbreviation
  )
where
import qualified Data.Map as M
import qualified Data.Text.Read as TR
import qualified Data.Scientific as S
import Data.Semigroup
import Control.Monad (foldM, guard, mzero)
import Control.Applicative ((<|>))
import Data.Char (isLower, isDigit, isLetter, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (elemIndex)
import Data.Maybe
import qualified Data.Vector as V
import Data.Aeson (FromJSON (..), ToJSON (..),
                   withArray, withObject, object, Value(..),
                   (.:), (.:?), (.!=))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Coerce
import Data.Generics.Uniplate.Direct
import qualified Data.Attoparsec.Text as P
import Safe (readMay)

import Debug.Trace
import Text.Show.Pretty (ppShow)
#ifdef MIN_VERSION_text_icu
import qualified Data.Text.ICU as ICU
#else
import qualified Data.RFC5051 as RFC5051
#endif

ppTrace :: Show a => a -> a
ppTrace x = trace (ppShow x) x

newtype CiteprocOptions =
  CiteprocOptions
  { linkCitations :: Bool }
  deriving (Show, Eq)

defaultCiteprocOptions :: CiteprocOptions
defaultCiteprocOptions =
  CiteprocOptions
  { linkCitations = False }

data CiteprocError =
    CiteprocXMLError Text
  | CiteprocParseError Text
  | CiteprocLocaleNotFound Text
  deriving (Show, Eq)

prettyCiteprocError :: CiteprocError -> Text
prettyCiteprocError (CiteprocXMLError t) =
  "CiteprocXMLError: " <> t
prettyCiteprocError (CiteprocParseError t) =
  "CiteprocParseError: " <> t
prettyCiteprocError (CiteprocLocaleNotFound t) =
  "CiteprocLocaleNotFound: " <> t

class (Semigroup a, Monoid a, Show a, Eq a, Ord a) => CiteprocOutput a where
  toText                      :: a -> Text
  fromText                    :: Text -> a
  dropTextWhile               :: (Char -> Bool) -> a -> a
  dropTextWhileEnd            :: (Char -> Bool) -> a -> a
  addFontVariant              :: FontVariant -> a -> a
  addFontStyle                :: FontStyle -> a -> a
  addFontWeight               :: FontWeight -> a -> a
  addTextDecoration           :: TextDecoration -> a -> a
  addVerticalAlign            :: VerticalAlign -> a -> a
  addTextCase                 :: TextCase -> a -> a
  addDisplay                  :: DisplayStyle -> a -> a
  addQuotes                   :: a -> a
  movePunctuationInsideQuotes :: a -> a
  inNote                      :: a -> a
  mapText                     :: (Text -> Text) -> a -> a
  addHyperlink                :: Text -> a -> a

addFormatting :: CiteprocOutput a => Formatting -> a -> a
addFormatting f x =
  if T.null (toText x)  -- TODO inefficient
     then mempty
     else
       maybe id addDisplay (formatDisplay f) .
       (if affixesInside then id else addPrefix . addSuffix) .
       (if formatQuotes f then addQuotes else id) .
       maybe id addVerticalAlign (formatVerticalAlign f) .
       maybe id addTextDecoration (formatTextDecoration f) .
       maybe id addFontWeight (formatFontWeight f) .
       maybe id addFontVariant (formatFontVariant f) .
       maybe id addTextCase (formatTextCase f) .
       maybe id addFontStyle (formatFontStyle f) .
       (if affixesInside then addPrefix . addSuffix else id) .
       (if formatStripPeriods f then mapText (T.filter (/='.')) else id)
       $ x
 where
  addPrefix z = case formatPrefix f of
                  Just s   -> fromText s <> z
                  Nothing  -> z
  addSuffix z = case formatSuffix f of
                  Just s   -> mconcat $ fixPunct [z, fromText s]
                  Nothing  -> z
  affixesInside = formatAffixesInside f

data Result a =
  Result
  { resultCitations     :: [a]
  , resultBibliography  :: [(Text, a)]  -- pairs of item id and entry
  , resultWarnings      :: [Text]
  } deriving (Show)

newtype ItemId = ItemId { unItemId :: Text }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

data CitationItemType =
  AuthorOnly | SuppressAuthor | NormalCite
  deriving (Show, Eq, Ord)

data CitationItem a =
  CitationItem
  { citationItemId             :: ItemId
  , citationItemLabel          :: Maybe Text
  , citationItemLocator        :: Maybe Text
  , citationItemType           :: CitationItemType
  , citationItemPrefix         :: Maybe a
  , citationItemSuffix         :: Maybe a
  } deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (CitationItem a) where
  parseJSON = withObject "CitationItem" $ \v -> CitationItem
    <$> (v .: "id" >>= fmap ItemId . asText)
    <*> v .:? "label"
    <*> (Just <$> (v .: "locator" >>= asText) <|> pure Nothing)
    <*> (do suppressAuth <- v .:? "suppress-author"
            authorOnly <- v .:? "author-only"
            return $
              case suppressAuth of
                Just True -> SuppressAuthor
                _ -> case authorOnly of
                       Just True -> AuthorOnly
                       _ -> NormalCite)
    <*> v .:? "prefix"
    <*> v .:? "suffix"

data Citation a =
  Citation { citationId         :: Maybe Text
           , citationNoteNumber :: Maybe Int
           , citationItems      :: [CitationItem a] }
  deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (Citation a) where
 parseJSON v =
   withArray "Citation"
     (\ary ->
       case ary V.!? 0 of
         Just v' -> (withObject "Citation" $ \o
                      -> Citation <$> o .:? "citationID"
                                  <*> ((o .: "properties"
                                         >>= (.: "noteIndex"))
                                        <|> pure Nothing)
                                  <*> o .: "citationItems") v'
         Nothing -> fail "Empty array") v
   <|>
   (Citation Nothing Nothing <$> parseJSON v)

newtype Attributes = Attributes [(Text, Text)]
  deriving (Show, Semigroup, Monoid, Eq)

lookupAttribute :: Text -> Attributes -> Maybe Text
lookupAttribute key (Attributes kvs) = lookup key kvs

data Match =
    MatchAll
  | MatchAny
  | MatchNone
  deriving (Show, Eq)

data Condition =
    HasVariable Text
  | HasType Text
  | IsUncertainDate Text
  | IsNumeric Text
  | HasLocatorType Text
  | HasPosition Position
  | WouldDisambiguate
  deriving (Show, Eq)

data Position =
    FirstPosition
  | IbidWithLocator
  | Ibid
  | NearNote
  | Subsequent
  deriving (Show, Eq, Ord)

data DateType =
    LocalizedNumeric
  | LocalizedText
  | NonLocalized
  deriving (Show, Eq, Ord)

data ShowDateParts =
    YearMonthDay
  | YearMonth
  | Year
  deriving (Show, Eq)

data DPName =
    DPYear
  | DPMonth
  | DPDay
  deriving (Show, Eq, Ord)

data DPForm =
    DPNumeric
  | DPNumericLeadingZeros
  | DPOrdinal
  | DPLong
  | DPShort
  deriving (Show, Eq)

data DP =
  DP
  { dpName           :: DPName
  , dpForm           :: DPForm
  , dpRangeDelimiter :: Text
  , dpFormatting     :: Formatting
  }
  deriving (Show, Eq)

data VariableForm =
    ShortForm
  | LongForm
  deriving (Show, Eq)

data TextType =
    TextVariable VariableForm Text
  | TextMacro Text
  | TextTerm Term
  | TextValue Text
  deriving (Show, Eq)

data NumberForm =
    NumberNumeric
  | NumberOrdinal
  | NumberLongOrdinal
  | NumberRoman
  deriving (Show, Eq)

data Pluralize =
    ContextualPluralize
  | AlwaysPluralize
  | NeverPluralize
  deriving (Show, Eq)

data NamesFormat =
    NamesFormat
    { namesLabel           :: Maybe (TermForm, Pluralize, Formatting)
    , namesEtAl            :: Maybe (Text, Formatting)
    , namesName            :: Maybe (NameFormat, Formatting)
    , namesLabelBeforeName :: Bool
    } deriving (Show, Eq)

data DelimiterPrecedes =
    PrecedesContextual
  | PrecedesAfterInvertedName
  | PrecedesAlways
  | PrecedesNever
  deriving (Show, Eq)

data NameForm =
    LongName
  | ShortName
  | CountName
  deriving (Show, Eq)

data NameFormat =
  NameFormat
  { nameGivenFormatting        :: Maybe Formatting
  , nameFamilyFormatting       :: Maybe Formatting
  , nameAndStyle               :: Maybe TermForm
  , nameDelimiter              :: Text
  , nameDelimiterPrecedesEtAl  :: DelimiterPrecedes
  , nameDelimiterPrecedesLast  :: DelimiterPrecedes
  , nameEtAlMin                :: Maybe Int
  , nameEtAlUseFirst           :: Maybe Int
  , nameEtAlSubsequentUseFirst :: Maybe Int
  , nameEtAlSubsequentMin      :: Maybe Int
  , nameEtAlUseLast            :: Bool
  , nameForm                   :: NameForm
  , nameInitialize             :: Bool
  , nameInitializeWith         :: Maybe Text
  , nameAsSortOrder            :: Maybe NameAsSortOrder
  , nameSortSeparator          :: Text
  } deriving (Show, Eq)

defaultNameFormat :: NameFormat
defaultNameFormat =
  NameFormat
  { nameGivenFormatting          = Nothing
  , nameFamilyFormatting         =  Nothing
  , nameAndStyle                 = Nothing
  , nameDelimiter                = ", "
  , nameDelimiterPrecedesEtAl    = PrecedesContextual
  , nameDelimiterPrecedesLast    = PrecedesContextual
  , nameEtAlMin                  = Nothing
  , nameEtAlUseFirst             = Nothing
  , nameEtAlSubsequentUseFirst   = Nothing
  , nameEtAlSubsequentMin        = Nothing
  , nameEtAlUseLast              = False
  , nameForm                     = LongName
  , nameInitialize               = True
  , nameInitializeWith           = Nothing
  , nameAsSortOrder              = Nothing
  , nameSortSeparator            = ", "
  }

data NameAsSortOrder =
     NameAsSortOrderFirst
   | NameAsSortOrderAll
   deriving (Show, Eq)

data ElementType a =
    EText TextType
  | EDate Text DateType (Maybe ShowDateParts) [DP]
  | ENumber Text NumberForm
  | ENames [Text] NamesFormat [Element a] -- last part is substitutes if any
  | ELabel Text TermForm Pluralize
  | EGroup [Element a]
  | EChoose [(Match, [Condition], [Element a])]
    -- 'else' can be represented by a final trivial match condition
  deriving (Show, Eq)

data Formatting =
  Formatting
  { formatFontStyle      :: Maybe FontStyle
  , formatFontVariant    :: Maybe FontVariant
  , formatFontWeight     :: Maybe FontWeight
  , formatTextDecoration :: Maybe TextDecoration
  , formatVerticalAlign  :: Maybe VerticalAlign
  , formatPrefix         :: Maybe Text
  , formatSuffix         :: Maybe Text
  , formatDisplay        :: Maybe DisplayStyle
  , formatTextCase       :: Maybe TextCase
  , formatDelimiter      :: Maybe Text
  , formatStripPeriods   :: Bool
  , formatQuotes         :: Bool
  , formatAffixesInside  :: Bool  -- put affixes inside other formatting
  } deriving (Show, Eq)

defaultFormatting :: Formatting
defaultFormatting = Formatting Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing False False False

combineFormatting :: Formatting -> Formatting -> Formatting
combineFormatting
  (Formatting a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1)
  (Formatting a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2) =
     Formatting (a1 <|> a2) (b1 <|> b2) (c1 <|> c2)
                (d1 <|> d2) (e1 <|> e2) (f1 <|> f2)
                (g1 <|> g2) (h1 <|> h2) (i1 <|> i2)
                (j1 <|> j2) (k1 || k2) (l1 || l2) (m1 || m2)

instance Semigroup Formatting where
 (<>) = combineFormatting

instance Monoid Formatting where
 mempty = defaultFormatting
 mappend = (<>)

data TextCase =
     Lowercase
   | Uppercase
   | CapitalizeFirst
   | CapitalizeAll
   | SentenceCase
   | TitleCase
   deriving (Show, Eq)

data DisplayStyle =
      DisplayBlock
    | DisplayLeftMargin
    | DisplayRightInline
    | DisplayIndent
    deriving (Show, Eq)

data FontStyle =
       NormalFont
    | ItalicFont
    | ObliqueFont
    deriving (Show, Eq)

data FontVariant =
      NormalVariant
    | SmallCapsVariant
    deriving (Show, Eq)

data FontWeight =
      NormalWeight
    | BoldWeight
    | LightWeight
    deriving (Show, Eq)

data TextDecoration =
      NoDecoration
    | UnderlineDecoration
    deriving (Show, Eq)

data VerticalAlign =
      BaselineAlign
    | SupAlign
    | SubAlign
    deriving (Show, Eq)

data Element a = Element (ElementType a) Formatting
  deriving (Show, Eq)

data SortDirection =
    Ascending
  | Descending
  deriving (Show, Eq)

data SortKey a =
     SortKeyVariable SortDirection Text
   | SortKeyMacro SortDirection [Element a]
  deriving (Show, Eq)

newtype SortKeyValue =
  SortKeyValue (SortDirection, Maybe [Text])
  deriving (Show, Eq)

-- absence should sort AFTER all values
-- see sort_StatusFieldAscending.txt, sort_StatusFieldDescending.txt
instance Ord SortKeyValue where
  SortKeyValue (Ascending, _)  <= SortKeyValue (Ascending, Nothing)
    = True
  SortKeyValue (Ascending, Nothing)  <= SortKeyValue (Ascending, Just _)
    = False
  SortKeyValue (Ascending, Just t1) <= SortKeyValue (Ascending, Just t2) =
    t1 `keyLEQ` t2
  SortKeyValue (Descending, _) <= SortKeyValue (Descending, Nothing)
    = True
  SortKeyValue (Descending, Nothing) <= SortKeyValue (Descending, Just _)
    = False
  SortKeyValue (Descending, Just t1) <= SortKeyValue (Descending, Just t2) =
    t2 `keyLEQ` t1
  SortKeyValue _ <= SortKeyValue _ = False

-- We need special comparison operators to ensure that
-- á sorts before b, for example.
keyLEQ :: [Text] -> [Text] -> Bool
keyLEQ _  [] = False
keyLEQ [] _  = True
keyLEQ (x:xs) (y:ys) =
  case comp x y of
    EQ -> xs `keyLEQ` ys
    GT -> False
    LT -> True
 where
#ifdef MIN_VERSION_text_icu
  comp = ICU.collate (ICU.collator ICU.Current)
#else
  comp = RFC5051.compareUnicode
#endif

data Layout a =
  Layout
  { layoutOptions        :: LayoutOptions
  , layoutFormatting     :: Formatting
  , layoutElements       :: [Element a]
  , layoutSortKeys       :: [SortKey a]
  } deriving (Show, Eq)

data LayoutOptions =
  LayoutOptions
  { layoutCollapse               :: Maybe Collapsing
  , layoutYearSuffixDelimiter    :: Maybe Text
  , layoutAfterCollapseDelimiter :: Maybe Text
  } deriving (Show, Eq)

data Collapsing =
     CollapseCitationNumber
   | CollapseYear
   | CollapseYearSuffix
   | CollapseYearSuffixRanged
   deriving (Show, Eq)

data DisambiguationStrategy =
  DisambiguationStrategy
  { disambiguateAddNames      :: Bool
  , disambiguateAddGivenNames :: Maybe GivenNameDisambiguationRule
  , disambiguateAddYearSuffix :: Bool
  } deriving (Show, Eq, Ord)

data GivenNameDisambiguationRule =
    AllNames
  | AllNamesWithInitials
  | PrimaryName
  | PrimaryNameWithInitials
  | ByCite
  deriving (Show, Eq, Ord)

data DemoteNonDroppingParticle =
     DemoteDisplayAndSort
   | DemoteSortOnly
   | DemoteNever
   deriving (Show, Eq)

data StyleOptions =
  StyleOptions
  { styleIsNoteStyle                :: Bool
  , styleDefaultLocale              :: Maybe Lang
  , styleDemoteNonDroppingParticle  :: DemoteNonDroppingParticle
  , styleInitializeWithHyphen       :: Bool
  , stylePageRangeFormat            :: Maybe PageRangeFormat
  , stylePageRangeDelimiter         :: Maybe Text
  , styleDisambiguation             :: DisambiguationStrategy
  , styleNearNoteDistance           :: Maybe Int
  , styleCiteGroupDelimiter         :: Maybe Text
  , styleLineSpacing                :: Maybe Int
  , styleEntrySpacing               :: Maybe Int
  , styleHangingIndent              :: Bool
  , styleSecondFieldAlign           :: Maybe SecondFieldAlign
  , styleSubsequentAuthorSubstitute :: Maybe SubsequentAuthorSubstitute
  , styleUsesYearSuffixVariable     :: Bool
  } deriving (Show, Eq)

data SubsequentAuthorSubstitute =
  SubsequentAuthorSubstitute Text SubsequentAuthorSubstituteRule
  deriving (Show, Eq)

data SubsequentAuthorSubstituteRule =
      CompleteAll
    | CompleteEach
    | PartialEach
    | PartialFirst
    deriving (Show, Eq)

data SecondFieldAlign =
      SecondFieldAlignFlush
    | SecondFieldAlignMargin
    deriving (Show, Eq)

data PageRangeFormat =
    PageRangeChicago
  | PageRangeExpanded
  | PageRangeMinimal
  | PageRangeMinimalTwo
  deriving (Show, Eq, Ord)

data Style a =
  Style
  { styleCslVersion    :: (Int,Int,Int)
  , styleOptions       :: StyleOptions
  , styleCitation      :: Layout a
  , styleBibliography  :: Maybe (Layout a)
  , styleLocales       :: [Locale]
  , styleAbbreviations :: Maybe Abbreviations
  } deriving (Show, Eq)
-- Note: no macros section, because we
-- expand these after parsing the CSL.

data TermForm =
    Long
  | Short
  | Verb
  | VerbShort
  | Symbol
  deriving (Show, Ord, Eq)

data TermNumber =
    Singular
  | Plural
  deriving (Show, Ord, Eq)

data TermGender =
    Masculine
  | Feminine
  deriving (Show, Ord, Eq)

data TermMatch =
    LastDigit
  | LastTwoDigits
  | WholeNumber
  deriving (Show, Ord, Eq)

data Term =
  Term
  { termName          :: Text
  , termForm          :: TermForm
  , termNumber        :: Maybe TermNumber
  , termGender        :: Maybe TermGender
  , termGenderForm    :: Maybe TermGender
  , termMatch         :: Maybe TermMatch
  } deriving (Show, Eq)

emptyTerm :: Term
emptyTerm = Term mempty Long Nothing Nothing Nothing Nothing

instance Ord Term where
   (<=)(Term name1 form1 num1 gen1 gf1 match1)
       (Term name2 form2 num2 gen2 gf2 match2) =
     name1 == name2 &&
     form1 == form2 &&
     (isNothing num1   || isNothing num2   || num1 == num2) &&
     (isNothing gen1   || isNothing gen2   || gen1 == gen2) &&
     (isNothing gf1    || isNothing gf2    || gf1  == gf2 ) &&
     (isNothing match1 || isNothing match2 || match1 == match2)

data Lang = Lang{ langLanguage :: Text
                , langVariant  :: Maybe Text }
  deriving (Show, Eq, Ord)

renderLang :: Lang -> Text
renderLang (Lang l Nothing)  = l
renderLang (Lang l (Just v)) = l <> "-" <> v

parseLang :: Text -> Lang
parseLang t = Lang l (snd <$> T.uncons v)
  where
   (l,v) = T.break (\c -> c == '-' || c == '_') t

data Locale =
  Locale
  { localeLanguage               :: Maybe Lang
  , localePunctuationInQuote     :: Maybe Bool
  , localeLimitDayOrdinalsToDay1 :: Maybe Bool
  , localeDate                   :: M.Map DateType (Element Text)
  , localeTerms                  :: M.Map Text [(Term, Text)]
  }
  deriving (Show, Eq)

-- in x <> y, x values take precedence
instance Semigroup Locale where
 Locale lang1 pq1 ldo1 date1 ts1 <>
   Locale lang2 pq2 ldo2 date2 ts2 =
   Locale (lang1 <|> lang2)
          (pq1 <|> pq2)
          (ldo1 <|> ldo2)
          (date1 <> date2)
          (M.unionWith (<>) ts1 ts2)

instance Monoid Locale where
 mempty = Locale Nothing Nothing Nothing mempty mempty
 mappend = (<>)

data Reference a =
  Reference{ referenceId             :: ItemId
           , referenceType           :: Text
           , referenceDisambiguation :: Maybe DisambiguationData
           , referenceVariables      :: M.Map Text (Val a)
           } deriving (Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Reference a) where
  toJSON r = toJSON $
               M.insert "id" (TextVal $ coerce (referenceId r)) $
               M.insert "type" (TextVal $ referenceType r) $
               referenceVariables r

data DisambiguationData =
  DisambiguationData
  { disambYearSuffix  :: Maybe Int
  , disambNameMap     :: M.Map Name NameHints
  , disambEtAlNames   :: Maybe Int
  , disambCondition   :: Bool
  } deriving (Show)

data NameHints =
    AddInitials
  | AddGivenName
  | AddInitialsIfPrimary
  | AddGivenNameIfPrimary
  deriving (Show)

instance (Eq a, FromJSON a)  => FromJSON (Reference a) where
  parseJSON v = parseJSON v >>= parseReference

lookupVariable :: CiteprocOutput a => Text -> Reference a -> Maybe (Val a)
lookupVariable "id" r =
  case referenceId r of
    ItemId "" -> Nothing
    ItemId t  -> Just (TextVal t)
lookupVariable "type" r =
  case referenceType r of
    "" -> Nothing
    t  -> Just (TextVal t)
lookupVariable "page-first" r =  -- compute "page-first" if not set
  M.lookup "page-first" (referenceVariables r) <|>
    case M.lookup "pages" (referenceVariables r) of
      Nothing           -> Nothing
      Just (NumVal n)   -> Just (NumVal n)
      Just (TextVal t)  -> NumVal <$> readMay (T.unpack (takeDigits t))
      Just (FancyVal x) -> NumVal <$> readMay (T.unpack
                                           (takeDigits $ toText x))
      _                 -> Nothing
 where
  takeDigits = T.takeWhile isDigit
lookupVariable v r = M.lookup v $ referenceVariables r

parseReference :: FromJSON a
               => M.Map Text Value -> Parser (Reference a)
parseReference rawmap =
  foldM go (Reference mempty mempty Nothing mempty) (M.toList rawmap)
 where
  go (Reference i t d m) (k, v)
    | k == "id"   = do
        id' <- ItemId <$> readString v
        return $ Reference id' t d m
    | k == "type" = do
        type' <- readString v
        return $ Reference i type' d m
    | k == "journalAbbreviation" || k == "shortTitle" = -- legacy citeproc-js
      go (Reference i t d m) ("container-title-short", v)
    | k == "note" = do
        t' <- parseJSON v
        let (kvs, rest) = parseNote t'
         in (if T.null rest
                then id
                else \(Reference i' t'' d' m') ->
                       Reference i' t'' d' (M.insert "note" (TextVal rest) m'))
             <$> foldM go (Reference i t d m) (consolidateNameVariables kvs)
    | otherwise   = Reference i t d <$>
        case variableType k of
          StringVariable -> do
            v' <- FancyVal <$> parseJSON v <|> TextVal <$> readString v
            return $ M.insert k v' m
          NumberVariable -> do
            v' <- case v of
                    String{} -> parseJSON v
                    Number{} -> T.pack . show <$> (parseJSON v :: Parser Int)
                    _        -> typeMismatch "String or Number" v
            return $ M.insert k (TextVal v') m
          DateVariable -> do
            v' <- parseJSON v
            return $ M.insert k (DateVal v') m
          NameVariable -> do
            v' <- parseJSON v
            return $ M.insert k (NamesVal v') m
          UnknownVariable -> do -- treat as string variable if possible
            case v of
              String{}  -> (\x -> M.insert k x m) <$>
                    (FancyVal <$> parseJSON v <|> TextVal <$> readString v)
              Number{}  -> (\x -> M.insert k (TextVal x) m) <$> readString v
              _         -> return m -- silently ignore
  readString v =
    case v of
       String{} -> parseJSON v
       Number{} -> T.pack . show <$> (parseJSON v :: Parser Int)
       _        -> typeMismatch "String or Number" v

-- name variables are cumulative and should be packed into an array
consolidateNameVariables :: [(Text, Text)] -> [(Text, Value)]
consolidateNameVariables [] = []
consolidateNameVariables ((k,v):kvs)
  = case variableType k of
      NameVariable
        -> (k, Array
                 (V.fromList ([String t | (k',t) <- kvs, k' == k]))) :
            consolidateNameVariables (filter ((/= k) . fst) kvs)
      _ -> (k, String v) : consolidateNameVariables kvs

parseNote :: Text
          -> ([(Text, Text)], Text)
parseNote t =
  either (const ([],t)) id $
    P.parseOnly ((,) <$> P.many' pNoteField <*> P.takeText) t
 where
  pNoteField = pBracedField <|> pLineField
  pLineField = do
    name <- pVarname
    _ <- P.char ':'
    val <- P.takeWhile (/='\n')
    () <$ P.char '\n' <|> P.endOfInput
    return (name, T.strip val)
  pBracedField = do
    _ <- P.string "{:"
    name <- pVarname
    _ <- P.char ':'
    val <- P.takeWhile (/='}')
    _ <- P.char '}'
    return (name, T.strip val)
  pVarname = P.takeWhile1 (\c -> isLetter c || c == '-')

data VariableType =
    DateVariable
  | NameVariable
  | NumberVariable
  | StringVariable
  | UnknownVariable
  deriving (Show, Eq)

variableType :: Text -> VariableType
variableType "accessed" = DateVariable
variableType "available-date" = DateVariable
variableType "container" = DateVariable
variableType "event-date" = DateVariable
variableType "issued" = DateVariable
variableType "original-date" = DateVariable
variableType "submitted" = DateVariable
variableType "author" = NameVariable
variableType "chair" = NameVariable
variableType "collection-editor" = NameVariable
variableType "composer" = NameVariable
variableType "compiler" = NameVariable
variableType "container-author" = NameVariable
variableType "contributor" = NameVariable
variableType "curator" = NameVariable
variableType "director" = NameVariable
variableType "editor" = NameVariable
variableType "editor-translator" = NameVariable
variableType "editorial-director" = NameVariable
variableType "executive-producer" = NameVariable
variableType "guest" = NameVariable
variableType "host" = NameVariable
variableType "illustrator" = NameVariable
variableType "interviewer" = NameVariable
variableType "narrator" = NameVariable
variableType "original-author" = NameVariable
variableType "organizer" = NameVariable
variableType "performer" = NameVariable
variableType "producer" = NameVariable
variableType "recipient" = NameVariable
variableType "reviewed-author" = NameVariable
variableType "script-writer" = NameVariable
variableType "series-creator" = NameVariable
variableType "translator" = NameVariable
variableType "chapter-number" = NumberVariable
variableType "citation-number" = NumberVariable
variableType "collection-number" = NumberVariable
variableType "edition" = NumberVariable
variableType "first-reference-note-number" = NumberVariable
variableType "issue" = NumberVariable
variableType "locator" = NumberVariable
variableType "number" = NumberVariable
variableType "number-of-pages" = NumberVariable
variableType "number-of-volumes" = NumberVariable
variableType "page" = NumberVariable
variableType "page-first" = NumberVariable
variableType "part-number" = NumberVariable
variableType "printing-number" = NumberVariable
variableType "section" = NumberVariable
variableType "supplement-number" = NumberVariable
variableType "version" = NumberVariable
variableType "volume" = NumberVariable
variableType "abstract" = StringVariable
variableType "annote" = StringVariable
variableType "archive" = StringVariable
variableType "archive_collection"  = StringVariable
variableType "archive_location" = StringVariable
variableType "archive-place" = StringVariable
variableType "authority" = StringVariable
variableType "call-number" = StringVariable
variableType "citation-key" = StringVariable
variableType "citation-label" = StringVariable
variableType "collection-title" = StringVariable
variableType "container-title" = StringVariable
variableType "container-title-short" = StringVariable
variableType "dimensions" = StringVariable
variableType "division" = StringVariable
variableType "DOI" = StringVariable
variableType "event" = StringVariable
variableType "event-place" = StringVariable
variableType "event-title" = StringVariable --(new name for "event" to avoid confusion with new "event" type) 
variableType "genre" = StringVariable
variableType "ISBN" = StringVariable
variableType "ISSN" = StringVariable
variableType "jurisdiction" = StringVariable
variableType "keyword" = StringVariable
variableType "language" = StringVariable
variableType "license" = StringVariable
variableType "medium" = StringVariable
variableType "note" = StringVariable
variableType "original-publisher" = StringVariable
variableType "original-publisher-place" = StringVariable
variableType "original-title" = StringVariable
variableType "part-title" = StringVariable
variableType "PMID" = StringVariable
variableType "PMCID" = StringVariable
variableType "publisher" = StringVariable
variableType "publisher-place" = StringVariable
variableType "references" = StringVariable
variableType "reviewed-genre" = StringVariable
variableType "reviewed-title" = StringVariable
variableType "scale" = StringVariable
variableType "source" = StringVariable
variableType "status" = StringVariable
variableType "title" = StringVariable
variableType "title-short" = StringVariable
variableType "URL" = StringVariable
variableType "volume-title" = StringVariable
variableType "year-suffix" = StringVariable
variableType _ = UnknownVariable

newtype (ReferenceMap a) =
  ReferenceMap { unReferenceMap :: M.Map ItemId (Reference a) }
  deriving (Show)

makeReferenceMap :: [Reference a] -> ReferenceMap a
makeReferenceMap refs =
  ReferenceMap (M.fromList (map (\r -> (referenceId r, r)) refs))

lookupReference :: ItemId -> ReferenceMap a -> Maybe (Reference a)
lookupReference ident (ReferenceMap m) = M.lookup ident m

data Val a =
    TextVal Text
  | FancyVal a
  | NumVal  Int
  | NamesVal [Name]
  | DateVal Date
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Val a) where
  toJSON (TextVal t) = toJSON t
  toJSON (FancyVal x) = toJSON x
  toJSON (NumVal n) = toJSON n
  toJSON (NamesVal ns) = toJSON ns
  toJSON (DateVal d) = toJSON d

data Name =
  Name
  { nameFamily              :: Maybe Text
  , nameGiven               :: Maybe Text
  , nameDroppingParticle    :: Maybe Text
  , nameNonDroppingParticle :: Maybe Text
  , nameSuffix              :: Maybe Text
  , nameCommaSuffix         :: Bool
  , nameStaticOrdering      :: Bool
  , nameLiteral             :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON Name where
  toJSON n =
    object $
      maybe id (\x -> (("family", toJSON x):)) (nameFamily n) .
      maybe id (\x -> (("given", toJSON x):)) (nameGiven n) .
      maybe id (\x -> (("dropping-particle", toJSON x):))
         (nameDroppingParticle n) .
      maybe id (\x -> (("non-dropping-particle", toJSON x):))
         (nameNonDroppingParticle n) .
      maybe id (\x -> (("suffix", toJSON x):)) (nameSuffix n) .
      (if nameCommaSuffix n
          then (("comma-suffix", toJSON True):)
          else id) .
      (if nameStaticOrdering n
          then (("static-ordering", toJSON True):)
          else id) .
      maybe id (\x -> (("literal", toJSON x):)) (nameLiteral n) $
      []

instance FromJSON Name where
  parseJSON (String t) = parseCheaterName t
  parseJSON x =
    extractParticles <$>
     (withObject "Name" $ \v -> Name
      <$> v .:? "family"
      <*> v .:? "given"
      <*> v .:? "dropping-particle"
      <*> v .:? "non-dropping-particle"
      <*> v .:? "suffix"
      <*> (v .:? "comma-suffix" >>= maybe (return False) asBool)
      <*> (v .:? "static-ordering" >>= maybe (return False) asBool)
      <*> v .:? "literal"
     ) x

-- "lowercase elements before the family name are treated as “non-dropping”
-- particles, and lowercase elements following the given name as “dropping”
-- particles"
extractParticles :: Name -> Name
extractParticles =
  extractNonDroppingParticle . extractDroppingParticle . extractSuffix
 where
  extractSuffix name =
    case nameSuffix name of
      Nothing ->
        case nameGiven name of
          Nothing -> name
          Just t
            -- in CSL JSON you can put double quotes around something
            -- to make it a unit (not subject to splitting).
            | "\"" `T.isPrefixOf` t
            , "\"" `T.isSuffixOf` t
            -> name { nameGiven = Just $ T.drop 1 $ T.dropEnd 1 t }
           | otherwise->
            let (a,b) = T.break (==',') t
             in if T.null a || T.null b
                   then name
                   else
                     if T.take 2 b == ",!"
                        then name{ nameGiven  = Just a
                                 , nameSuffix = Just $ T.strip $ T.drop 2 b
                                 , nameCommaSuffix = True }
                        else name{ nameGiven  = Just a
                                 , nameSuffix = Just $ T.strip $ T.drop 1 b }
      Just _  -> name
  extractNonDroppingParticle name =
    case nameNonDroppingParticle name of
      Nothing ->
        case nameFamily name of
          Nothing -> name
          Just t
            | "\"" `T.isPrefixOf` t
            , "\"" `T.isSuffixOf` t
              -> name { nameFamily = Just $ T.drop 1 $ T.dropEnd 1 t }
            | otherwise ->
              let isParticlePunct c = c == '\'' || c == '’' || c == '-' ||
                                       c == '\x2013' in
              case span (T.all isLower) (T.words t) of
                ([],_)
                    -> case T.split isParticlePunct t of
                         [x,y] | T.all isLower x ->
                              name{ nameFamily = Just y
                                  , nameNonDroppingParticle = Just $ x <>
                                      T.take 1
                                      (T.dropWhile (not . isParticlePunct) t) }
                         _ -> name
                (_,[])  -> name
                (as,bs) -> name{ nameFamily = Just (T.unwords bs)
                               , nameNonDroppingParticle = Just (T.unwords as) }
      Just _  -> name
  extractDroppingParticle name =
    case nameDroppingParticle name of
      Just _  -> name
      Nothing ->
        case nameGiven name of
          Nothing -> name
          Just t  ->
            case break (T.all isLower) (T.words t) of
              (_,[])  -> name
              ([],_)  -> name
              (as,bs)
                | all (T.all isLower) bs
                      -> name{ nameGiven = Just (T.unwords as)
                             , nameDroppingParticle = Just (T.unwords bs) }
                | otherwise -> name

-- cheater syntax for name: used in parsing note:
--  editor: Thompson || Hunter S.
parseCheaterName :: Text -> Parser Name
parseCheaterName t = do
  let (family, given) = case T.splitOn "||" t of
                           (f:g:_) -> (Just (T.strip f), Just (T.strip g))
                           [f]     -> (Just (T.strip f), Nothing)
                           []      -> (Nothing, Nothing)
  return $ extractParticles $
      Name
      { nameFamily              = family
      , nameGiven               = given
      , nameDroppingParticle    = Nothing
      , nameNonDroppingParticle = Nothing
      , nameSuffix              = Nothing
      , nameCommaSuffix         = False
      , nameStaticOrdering      = False
      , nameLiteral             = if isNothing family && isNothing given
                                     then Just t
                                     else Nothing
      }

isByzantineName :: Name -> Bool
isByzantineName name = maybe False isByzantine (nameFamily name)

-- detect latin/cyrillic names
-- see src/load.js ROMANESQUE_REGEX in citeproc-js:
-- /[-0-9a-zA-Z\u0e01-\u0e5b\u00c0-\u017f\u0370-\u03ff\u0400-\u052f\u0590-\u05d4\u05d6-\u05ff\u1f00-\u1fff\u0600-\u06ff\u200c\u200d\u200e\u0218\u0219\u021a\u021b\u202a-\u202e]/
isByzantineChar :: Char -> Bool
isByzantineChar c = c == '-' ||
                (c >= '0' && c <= '9') ||
                (c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                (c >= '\x0e01' && c <= '\x0e5b') ||
                (c >= '\x00c0' && c <= '\x017f') ||
                (c >= '\x0370' && c <= '\x03ff') ||
                (c >= '\x0400' && c <= '\x052f') ||
                (c >= '\x0590' && c <= '\x05d4') ||
                (c >= '\x05d6' && c <= '\x05ff') ||
                (c >= '\x1f00' && c <= '\x1fff') ||
                (c >= '\x0600' && c <= '\x06ff') ||
                (c >= '\x200c' && c <= '\x200e') ||
                (c >= '\x2018' && c <= '\x2019') ||
                (c >= '\x021a' && c <= '\x021b') ||
                (c >= '\x202a' && c <= '\x202e')

isByzantine :: Text -> Bool
isByzantine = T.any isByzantineChar

asBool :: Value -> Parser Bool
asBool (String t) = return $ t == "true"
asBool (Bool b)   = return b
asBool (Number n) = return $ n == 1
asBool x          = typeMismatch "Bool" x

asText :: Value -> Parser Text
asText (String t)   = return t
asText (Number n)   = return $ case S.floatingOrInteger n of
                                 Left r -> T.pack (show (r :: Double))
                                 Right i -> T.pack (show (i :: Int))
asText x            = typeMismatch "String" x

asInt :: Value -> Parser Int
asInt (String t) =
  case readAsInt t of
    Just x  -> return x
    Nothing -> fail "not a number"
asInt v@Number{} = parseJSON v
asInt v = typeMismatch "Number" v

data Date =
  Date
  { dateParts     :: [DateParts]
  , dateCirca     :: Bool
  , dateSeason    :: Maybe Int
  , dateLiteral   :: Maybe Text
  } deriving (Show, Eq, Ord)

instance ToJSON Date where
  toJSON d =
    object $
      (if dateCirca d then (("circa", toJSON True):) else id) .
      (case dateSeason d of
        Just s -> (("season", toJSON s):)
        Nothing -> id) .
      (case dateLiteral d of
        Just l -> (("literal", toJSON l):)
        Nothing -> id) $
      [ ("date-parts", toJSON (dateParts d)) ]

instance FromJSON Date where
 parseJSON (String t) = rawDate t  -- cheater dates
 parseJSON x = withObject "Date" (\v ->
   (v.: "raw" >>= rawDate)
   <|>
   (Date <$> v .:? "date-parts" .!= []
         <*> ((v .: "circa" >>= asBool) <|> pure False)
         <*> ((v .: "season" >>= fmap Just . asInt) <|> pure Nothing)
         <*> v .:? "literal")) x

newtype DateParts = DateParts [Int]
  deriving (Show, Eq, Ord, ToJSON)

instance FromJSON DateParts where
  parseJSON v =
    DateParts <$> (parseJSON v >>= mapM asInt . removeEmptyStrings)

rawDate :: Text -> Parser Date
rawDate t = case rawDateEDTF t <|> rawDateOld t of
              Just d  -> return d
              Nothing -> return $ Date { dateParts = []
                                       , dateCirca = False
                                       , dateSeason = Nothing
                                       , dateLiteral = Just t }

rawDateEDTF :: Text -> Maybe Date
rawDateEDTF = rawDateISO . handleRanges
 where
  handleRanges t =
    case T.split (=='/') t of
         -- 199u EDTF format for a range
         [x] | T.any (== 'u') x ->
               T.map (\c -> if c == 'u' then '0' else c) x
               <> "/" <>
               T.map (\c -> if c == 'u' then '9' else c) x
         [x, "open"] -> x <> "/"    -- EDTF
         [x, "unknown"] -> x <> "/" -- EDTF
         _  -> t

rawDateISO :: Text -> Maybe Date
rawDateISO raw = do
  let ranges = map T.strip $ T.split (=='/') raw
  let circa = any ("~" `T.isSuffixOf`) ranges
  let isSpecial '~' = True
      isSpecial '?' = True
      isSpecial '%' = True
      isSpecial 'T' = True
      isSpecial _   = False
  let dparts t = do
        (hasY, t') <- if T.take 1 t == "y"
                         then return (True, T.drop 1 t)
                         else return (False, t)
        (isNeg, t'') <- if T.take 1 t' == "-"
                           then return (True, T.drop 1 t')
                           else return (False, t')
        let t''' = T.takeWhile (not . isSpecial) t''
        case T.split (=='-') t''' of
          [""]         -> return $ DateParts []
          [y', m', d'] -> do
            guard $ T.length y' == 4 || hasY && T.length y' >= 4
            guard $ T.length m' == 2
            guard $ T.length d' == 2
            y <- (if isNeg
                     then (\x -> (x * (-1)) - 1) -- 0 = 1 BC
                     else id) <$> readAsInt y'
            m <- readAsInt m'
            d <- readAsInt d'
            return $ DateParts [y, m, d]
          [y', m'] -> do
            guard $ T.length y' == 4 || hasY && T.length y' >= 4
            guard $ T.length m' == 2
            y <- (if isNeg
                     then (\x -> (x * (-1)) - 1) -- 0 = 1 BC
                     else id) <$> readAsInt y'
            m <- readAsInt m'
            return $ DateParts [y, m]
          [y'] -> do
            guard $ T.length y' == 4 || hasY && T.length y' >= 4
            y <- (if isNeg
                     then (\x -> (x * (-1)) - 1) -- 0 = 1 BC
                     else id) <$> readAsInt y'
            return $ DateParts [y]
          _ -> mzero
  dps <- mapM dparts ranges
  return $ Date
    { dateParts     = dps
    , dateCirca     = circa
    , dateSeason    = Nothing
    , dateLiteral   = Nothing
    }


rawDateOld :: Text -> Maybe Date
rawDateOld raw = do
  let months   = ["jan","feb","mar","apr","may","jun","jul","aug",
                  "sep","oct","nov","dec"]
  let seasons  = ["spr","sum","fal","win"]
  let ranges = T.split (=='-') raw
  let readTextMonth t = do
        let key = T.toLower $ T.take 3 t
        case elemIndex key months of
             Just n  -> return (n+1)
             Nothing -> case elemIndex key seasons of
                          Just n -> return (n+13)
                          Nothing -> fail "Improper month"
  let dparts t =
        case T.split (\c -> c == ' ' || c == '/' || c == ',') $ T.strip t of
          [m', d', y'] -> do
            y <- readAsInt y'
            m <- readAsInt m' <|> readTextMonth m'
            d <- readAsInt d'
            return $ DateParts [y, m, d]
          [m', y']     -> do
            y <- readAsInt y'
            m <- readAsInt m' <|> readTextMonth m'
            return $ DateParts [y, m]
          [y']         -> do
            y <- readAsInt y'
            return $ DateParts [y]
          []           -> return $ DateParts []
          _            -> mzero
  dps <- mapM dparts ranges
  return $ Date
    { dateParts     = dps
    , dateCirca     = False
    , dateSeason    = Nothing
    , dateLiteral   = Nothing
    }



removeEmptyStrings :: [Value] -> [Value]
removeEmptyStrings = filter (not . isEmptyString)
  where
   isEmptyString (String t) = T.null t
   isEmptyString _ = False

data Output a =
    Formatted Formatting [Output a]
  | InNote (Output a)
  | Literal a
  | Tagged Tag (Output a)
  | NullOutput
  deriving (Show, Eq)

instance Uniplate (Output a) where
  uniplate (Formatted f xs) = plate Formatted |- f ||* xs
  uniplate (InNote x)       = plate InNote |* x
  uniplate (Literal x)      = plate Literal |- x
  uniplate (Tagged t x)     = plate Tagged |- t |* x
  uniplate NullOutput       = plate NullOutput

instance Biplate (Output a) (Output a) where
  biplate = plateSelf

data Tag =
      TagTerm
    | TagCitationNumber Int
    | TagItem CitationItemType ItemId
    | TagName Name
    | TagNames Text NamesFormat [Name]
    | TagDate Date
    | TagYearSuffix Int
  deriving (Show, Eq)

outputToText :: CiteprocOutput a => Output a -> Text
outputToText NullOutput = mempty
outputToText (Literal x ) = toText x
outputToText (Tagged _ x) = outputToText x
outputToText (Formatted _ xs) = T.unwords $ map outputToText xs
outputToText (InNote x)   = outputToText x

renderOutput :: CiteprocOutput a => CiteprocOptions -> Output a -> a
renderOutput _ NullOutput = mempty
renderOutput _ (Literal x) = x
renderOutput opts (Tagged (TagItem _ ident) x)
  | linkCitations opts
  = addHyperlink ("#ref-" <> unItemId ident) $ renderOutput opts x
renderOutput opts (Tagged _ x) = renderOutput opts x
renderOutput opts (Formatted formatting xs) =
  addFormatting formatting . mconcat . fixPunct .
    (case formatDelimiter formatting of
       Just d  -> addDelimiters (fromText d)
       Nothing -> id) . filter (/= mempty) $ map (renderOutput opts) xs
renderOutput opts (InNote x) = inNote $
  dropTextWhile isSpace $
  dropTextWhile (\c -> c == ',' || c == ';' || c == '.' || c == ':') $
  renderOutput opts x

addDelimiters :: CiteprocOutput a => a -> [a] -> [a]
addDelimiters delim =
  foldr addDelim []
 where
  addDelim x []     = [x]
  addDelim x (a:as) = case T.uncons (toText a) of
                         Just (c,_)
                           | c == ',' || c == ';' || c == '.' -> x : a : as
                         _ -> x : delim : a : as

fixPunct :: CiteprocOutput a => [a] -> [a]
fixPunct (x:y:zs) =
  case (T.takeEnd 1 (toText x), T.take 1 (toText y)) of
    (":", ":") -> x : fixPunct (dropTextWhile (==':') y : zs)
    (";", ":") -> x : fixPunct (dropTextWhile (==':') y : zs)
    ("!", ":") -> x : fixPunct (dropTextWhile (==':') y : zs)
    ("?", ":") -> x : fixPunct (dropTextWhile (==':') y : zs)
    (":", ".") -> x : fixPunct (dropTextWhile (=='.') y : zs)
    (".", ".") -> x : fixPunct (dropTextWhile (=='.') y : zs)
    (";", ".") -> x : fixPunct (dropTextWhile (=='.') y : zs)
    ("!", ".") -> x : fixPunct (dropTextWhile (=='.') y : zs)
    ("?", ".") -> x : fixPunct (dropTextWhile (=='.') y : zs)
    (";", ";") -> x : fixPunct (dropTextWhile (==';') y : zs)
    (",", ";") -> x : fixPunct (dropTextWhile (==';') y : zs)
    (":", "!") -> dropTextWhileEnd (==':') x : fixPunct (y : zs)
    (";", "!") -> dropTextWhileEnd (==';') x : fixPunct (y : zs)
    ("!", "!") -> dropTextWhileEnd (=='!') x : fixPunct (y : zs)
    (":", "?") -> dropTextWhileEnd (==':') x : fixPunct (y : zs)
    (";", "?") -> dropTextWhileEnd (==';') x : fixPunct (y : zs)
    ("?", "?") -> dropTextWhileEnd (=='?') x : fixPunct (y : zs)
    (",", ",") -> x : fixPunct (dropTextWhile (==',') y : zs)
    (" ", " ") -> dropTextWhileEnd (==' ') x : fixPunct (y : zs)
    (" ", ",") -> dropTextWhileEnd (==' ') x : fixPunct (y : zs)
    (" ", ".") -> dropTextWhileEnd (==' ') x : fixPunct (y : zs)
    _ -> x : fixPunct (y : zs)
fixPunct zs = zs

grouped :: [Output a] -> Output a
grouped = formatted mempty

formatted :: Formatting -> [Output a] -> Output a
formatted formatting = grouped' . filter (not . isNullOutput)
 where
  isNullOutput NullOutput = True
  isNullOutput _          = False
  grouped' []  = NullOutput
  grouped' [x] | formatting == mempty = x
  grouped' xs  = Formatted formatting xs

readAsInt :: Text -> Maybe Int
readAsInt t =
  case TR.decimal t of
      Right (x,t') | T.null t' -> Just x
      _                        -> Nothing

newtype Abbreviations =
  Abbreviations (M.Map Text (M.Map Text Text))
  deriving (Show, Eq, Ord)

instance FromJSON Abbreviations where
  parseJSON (Object v)   =
    Abbreviations <$>
      (parseJSON (Object v) >>= maybe
         (fail "abbreviations lacks a default key")
         return . M.lookup ("default" :: Text))
  parseJSON _            = fail "Could not read abbreviations"

lookupAbbreviation :: CiteprocOutput a
                   => Text -> Val a -> Abbreviations -> Maybe (Val a)
lookupAbbreviation var val (Abbreviations abbrevmap) = do
  abbrvs <- M.lookup (if variableType var == NumberVariable
                         then "number"
                         else var) abbrevmap
  case val of
    TextVal t  -> maybe mzero (return . TextVal) $ M.lookup t abbrvs
    FancyVal x -> return $ FancyVal
                         $ mapText (\t -> fromMaybe t $ M.lookup t abbrvs) x
    NumVal n   -> maybe mzero (return . TextVal)
                         $ M.lookup (T.pack (show n)) abbrvs
    _          -> mzero

