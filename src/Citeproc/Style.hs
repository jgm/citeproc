{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Citeproc.Style
  ( parseStyle
  , mergeLocales
  )
where
import Citeproc.Types
import Citeproc.Locale
import Citeproc.Element
import Data.Text (Text)
import Control.Monad (foldM)
import Control.Applicative ((<|>))
import qualified Text.XML as X
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Default (def)
import qualified Data.Text.Lazy as TL

-- | Merge the locale specified by the first parameter, if any,
-- with the default locale of the style and locale definitions
-- in the style.  The locale specified by the first parameter
-- overrides the style's defaults when there is a conflict.
mergeLocales :: Maybe Lang -> Style a -> Locale
mergeLocales mblang style =
  mconcat stylelocales <> deflocale -- left-biased union
 where
  getUSLocale = case getLocale (Lang "en" Nothing (Just"US") [] [] []) of
                  Right l -> l
                  Left _  -> mempty
  lang = fromMaybe (Lang "en" Nothing (Just"US") [] [] []) $
              mblang <|> styleDefaultLocale (styleOptions style)
  deflocale = case getLocale lang of
                 Right l -> l
                 Left _  -> getUSLocale
  primlang = getPrimaryDialect lang
  stylelocales =  -- exact match to lang gets precedence
                 [l | l <- styleLocales style
                    , localeLanguage l == Just lang] ++
                 -- then match to primary dialect, if different
                 [l | primlang /= Just lang
                    , l <- styleLocales style
                    , localeLanguage l == primlang] ++
                 -- then match to the two letter language
                 [l | l <- styleLocales style
                    , (langRegion <$> localeLanguage l) == Just Nothing
                    , (langLanguage <$> localeLanguage l) ==
                      Just (langLanguage lang)] ++
                 -- then locale with no lang
                 [l | l <- styleLocales style
                    , isNothing (localeLanguage l)]



-- | Parse an XML stylesheet into a 'Style'.  The first parameter
-- is a function that retrieves the text of the independent parent
-- of a dependent style, given a URL.  (This might make an HTTP
-- request or retrieve the style locally.)  If you aren't using
-- dependent styles, you may use `(\_ -> return mempty)`.
parseStyle :: Monad m
           => (Text -> m Text) -- ^ Function that takes a URL and retrieves
                               -- text of independent parent
           -> Text             -- ^ Contents of XML stylesheet
           -> m (Either CiteprocError (Style a))
parseStyle getIndependentParent t =
  -- first, see if it's a dependent or independent style
  case X.parseText def (TL.fromStrict t) of
    Left e  -> return $ Left $ CiteprocXMLError (T.pack (show e))
    Right n -> do
      let attr = getAttributes $ X.documentRoot n
      let defaultLocale =
            case lookupAttribute "default-locale" attr of
              Nothing  -> Nothing
              Just l   -> either (const Nothing) Just $ parseLang l
      let links = concatMap (getChildren "link") $ getChildren "info"
                    (X.documentRoot n)
      case [getAttributes l
              | l <- links
              , lookupAttribute "rel" (getAttributes l) ==
                  Just "independent-parent" ] of
        [] -> return $
               runElementParser $ pStyle defaultLocale $ X.documentRoot n
        (lattr:_) ->
          case lookupAttribute "href" lattr of
            Nothing -> return $ Left $ CiteprocXMLError
                          "No href attribute on link to parent style"
            Just url -> do -- get parent style
              parentTxt <- getIndependentParent url
              case X.parseText def (TL.fromStrict parentTxt) of
                Left e -> return $ Left $ CiteprocXMLError (T.pack (show e))
                Right n' -> return $
                 runElementParser $ pStyle defaultLocale $ X.documentRoot n'

pStyle :: Maybe Lang -> X.Element -> ElementParser (Style a)
pStyle defaultLocale node = do
  let attr = getAttributes node
  let nameformat = getInheritableNameFormat attr
  macros <- M.fromList <$> mapM pMacro (getChildren "macro" node)
  (cattr, citations)
      <- case getChildren "citation" node of
                 [n] -> (getAttributes n,) <$> pLayout n
                 []  -> parseFailure "No citation element present"
                 _   -> parseFailure "More than one citation element present"
  (battr, bibliography) <- case getChildren "bibliography" node of
                    [n] -> (\z -> (getAttributes n, Just z))
                              <$> pLayout n
                    []  -> return (mempty, Nothing)
                    _   -> parseFailure
                             "More than one bibliography element present"

  let disambiguateGivenNameRule =
        case lookupAttribute "givenname-disambiguation-rule" cattr of
          Just "all-names" -> AllNames
          Just "all-names-with-initials" -> AllNamesWithInitials
          Just "primary-name" -> PrimaryName
          Just "primary-name-with-initials" -> PrimaryNameWithInitials
          _ -> ByCite

  let disambigStrategy =
        DisambiguationStrategy
        { disambiguateAddNames =
            lookupAttribute "disambiguate-add-names" cattr == Just "true"
        , disambiguateAddGivenNames =
            case lookupAttribute "disambiguate-add-givenname" cattr of
              Just "true" -> Just disambiguateGivenNameRule
              _           -> Nothing
        , disambiguateAddYearSuffix =
           lookupAttribute "disambiguate-add-year-suffix" cattr ==
             Just "true"
        }

  let hasYearSuffixVariable (Element e f) =
        case e of
          EText (TextVariable _ "year-suffix") -> True
          EText (TextMacro macroname)
            | Just es' <- M.lookup macroname macros ->
                 any hasYearSuffixVariable es'
          EGroup _ es -> any hasYearSuffixVariable es
          EChoose [] -> False
          EChoose ((_,_,es):conds) -> any hasYearSuffixVariable es ||
              hasYearSuffixVariable (Element (EChoose conds) f)
          _ -> False
  let usesYearSuffixVariable =
        any hasYearSuffixVariable $
          layoutElements citations ++ maybe [] layoutElements bibliography

  let sOpts = StyleOptions
               { styleIsNoteStyle =
                   case lookupAttribute "class" attr of
                     Just "note" -> True
                     Nothing     -> True
                     _           -> False
               , styleDefaultLocale = defaultLocale
               , styleDemoteNonDroppingParticle =
                   case lookupAttribute "demote-non-dropping-particle" attr of
                     Just "never"     -> DemoteNever
                     Just "sort-only" -> DemoteSortOnly
                     _                -> DemoteDisplayAndSort
               , styleInitializeWithHyphen =
                 maybe True (== "true") $
                   lookupAttribute "initialize-with-hyphen" attr
               , stylePageRangeFormat =
                   case lookupAttribute "page-range-format" attr of
                     Just "chicago"     -> Just PageRangeChicago15
                     -- chicago is an alias for chicago-15, but this
                     -- will change to chicago-16 in v1.1
                     Just "chicago-15"  -> Just PageRangeChicago15
                     Just "chicago-16"  -> Just PageRangeChicago16
                     Just "expanded"    -> Just PageRangeExpanded
                     Just "minimal"     -> Just PageRangeMinimal
                     Just "minimal-two" -> Just PageRangeMinimalTwo
                     _                  -> Nothing
               , stylePageRangeDelimiter =
                   lookupAttribute "page-range-delimiter" attr
               , styleDisambiguation = disambigStrategy
               , styleNearNoteDistance =
                   lookupAttribute "near-note-distance" attr >>= readAsInt
               , styleCiteGroupDelimiter =
                   lookupAttribute "cite-group-delimiter" cattr <|>
                   (", " <$ lookupAttribute "collapse" cattr)
               , styleLineSpacing =
                   lookupAttribute "line-spacing" battr >>= readAsInt
               , styleEntrySpacing =
                   lookupAttribute "entry-spacing" battr >>= readAsInt
               , styleHangingIndent =
                   lookupAttribute "hanging-indent" battr == Just "true"
               , styleSecondFieldAlign =
                   case lookupAttribute "second-field-align" battr of
                     Just "flush" -> Just SecondFieldAlignFlush
                     Just "margin" -> Just SecondFieldAlignMargin
                     _ -> Nothing
               , styleSubsequentAuthorSubstitute =
                   case lookupAttribute "subsequent-author-substitute"
                        battr of
                     Nothing -> Nothing
                     Just t  -> Just $
                       SubsequentAuthorSubstitute t
                       $ case lookupAttribute
                           "subsequent-author-substitute-rule" battr of
                             Just "complete-each" -> CompleteEach
                             Just "partial-each" -> PartialEach
                             Just "partial-first" -> PartialFirst
                             _  -> CompleteAll
               , styleUsesYearSuffixVariable = usesYearSuffixVariable
               , styleNameFormat = nameformat -- TODO
               }
  locales <- mapM pLocale (getChildren "locale" node)
  let cslVersion = case lookupAttribute "version" attr of
                     Nothing -> (0,0,0)
                     Just t  ->
                       case map readAsInt (T.splitOn "." t) of
                         (Just x : Just y : Just z :_) -> (x,y,z)
                         (Just x : Just y : _)         -> (x,y,0)
                         (Just x : _)                  -> (x,0,0)
                         _                             -> (0,0,0)
  return $ Style
           { styleCslVersion     = cslVersion
           , styleOptions        = sOpts
           , styleCitation       = citations
           , styleBibliography   = bibliography
           , styleLocales        = locales
           , styleAbbreviations  = Nothing
           , styleMacros         = macros
           }

pElement :: X.Element -> ElementParser (Element a)
pElement node =
  case X.nameLocalName (X.elementName node) of
    "date"   -> pDate node
    "text"   -> pText node
    "group"  -> pGroup node
    "choose" -> pChoose node
    "number" -> pNumber node
    "label"  -> pLabel node
    "names"  -> pNames node
    name     -> parseFailure $ "unknown element " <> show name

pChoose :: X.Element -> ElementParser (Element a)
pChoose node = do
  ifNodes <- mapM parseIf $ getChildren "if" node
  elseIfNodes <- mapM parseIf $ getChildren "else-if" node
  elseNodes <- mapM parseIf $ getChildren "else" node
  let parts = ifNodes ++ elseIfNodes ++ elseNodes
  return $ Element (EChoose parts) mempty

parseIf :: X.Element -> ElementParser (Match, [Condition], [Element a])
parseIf node = do
  let attr = getAttributes node
  let match = case lookupAttribute "match" attr of
                Just "any"   -> MatchAny
                Just "none"  -> MatchNone
                _            -> MatchAll
  let conditions =
        (case lookupAttribute "disambiguate" attr of
           Just "true" -> (WouldDisambiguate :)
           _           -> id) .
        (case lookupAttribute "is-numeric" attr of
           Just t  -> \xs -> foldr ((:) . IsNumeric) xs (splitVars t)
           _       -> id) .
        (case lookupAttribute "is-uncertain-date" attr of
           Just t  -> \xs -> foldr ((:) . IsUncertainDate) xs (splitVars t)
           _       -> id) .
        (case lookupAttribute "locator" attr of
           Just t  -> \xs -> foldr ((:) . HasLocatorType) xs (splitVars t)
           _       -> id) .
        (case lookupAttribute "position" attr of
           Just t  -> \xs ->
             foldr (\case
                       "first"      -> (HasPosition FirstPosition :)
                       "ibid"       -> (HasPosition Ibid :)
                       "ibid-with-locator"
                                    -> (HasPosition IbidWithLocator :)
                       "subsequent" -> (HasPosition Subsequent :)
                       "near-note"  -> (HasPosition NearNote :)
                       _            -> id)
             xs (splitVars t)
           _       -> id) .
        (case lookupAttribute "type" attr of
           Just t  -> (HasType (T.words $ T.strip t) :)
           _       -> id) .
        (case lookupAttribute "variable" attr of
           Just t  -> \xs -> foldr ((:) . HasVariable) xs (splitVars t)
           _       -> id) $ []
  elts <- mapM pElement $ allChildren node
  return (match, conditions, elts)

pNumber :: X.Element -> ElementParser (Element a)
pNumber node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let variable = lookupAttribute "variable" attr
  let numform = case lookupAttribute "form" attr of
                  Just "ordinal"      -> NumberOrdinal
                  Just "long-ordinal" -> NumberLongOrdinal
                  Just "roman"        -> NumberRoman
                  _                   -> NumberNumeric
  case variable of
    Nothing  -> parseFailure "number element without required variable attribute"
    Just var -> return $ Element (ENumber (toVariable var) numform)
                         formatting

pLabel :: X.Element -> ElementParser (Element a)
pLabel node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let variable = toVariable $ fromMaybe "" $ lookupAttribute "variable" attr
  let labelform = case lookupAttribute "form" attr of
                    Just "short"      -> Short
                    Just "verb"       -> Verb
                    Just "verb-short" -> VerbShort
                    Just "symbol"     -> Symbol
                    _                 -> Long
  let pluralize = case lookupAttribute "plural" attr of
                    Just "always" -> AlwaysPluralize
                    Just "never"  -> NeverPluralize
                    _             -> ContextualPluralize
  return $ Element (ELabel variable labelform pluralize) formatting

pNames :: X.Element -> ElementParser (Element a)
pNames node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let variables = maybe [] splitVars $ lookupAttribute "variable" attr
  let pChild (nf,subst) n =
       case X.nameLocalName (X.elementName n) of
         "label"      -> do
           e <- pLabel n
           case e of
             Element (ELabel _ labelform pluralize) f ->
               return ( nf{ namesLabel = Just (labelform, pluralize, f)
                          , namesLabelBeforeName =
                              isNothing (namesName nf) }
                      , subst )
             _ -> parseFailure "pLabel returned something other than ELabel"
         "substitute" -> do
           els <- mapM pElement $ allChildren n
           return ( nf, els )
         "et-al"      -> do
           res <- pEtAl n
           return ( nf{ namesEtAl = Just res }, subst )
         "name"       -> do
           res <- pName n
           return ( nf{ namesName = Just res }, subst )
         name -> parseFailure $ "element " <> show name <>
                        " not a valid child of names"
  (nameformat, subst) <-
      foldM pChild (NamesFormat Nothing Nothing Nothing False, [])
                   (allChildren node)
  return $ Element (ENames variables nameformat subst) formatting

pEtAl :: X.Element -> ElementParser (Text, Formatting)
pEtAl node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let term = fromMaybe "et-al" $ lookupAttribute "term" attr
  return (term, formatting)


pName :: X.Element -> ElementParser (NameFormat, Formatting)
pName node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let nameParts = map getAttributes $ getChildren "name-part" node
  let nameformat = NameFormat
         { nameGivenFormatting        =
             case [nattr
                   | nattr <- nameParts
                   , lookupAttribute "name" nattr == Just "given" ] of
                (nattr:_) -> Just $ getFormatting nattr
                _     -> Nothing
         , nameFamilyFormatting       =
             case [nattr
                   | nattr <- nameParts
                   , lookupAttribute "name" nattr == Just "family" ] of
                (nattr:_) -> Just $ getFormatting nattr
                _     -> Nothing
         , nameAndStyle           =
             case lookupAttribute "and" attr of
               Just "text"   -> Just Long
               Just "symbol" -> Just Symbol
               _             -> Nothing
         , nameDelimiter              =
             lookupAttribute "delimiter" attr
         , nameDelimiterPrecedesEtAl  =
             case lookupAttribute "delimiter-precedes-et-al" attr of
               Just "after-inverted-name" -> Just PrecedesAfterInvertedName
               Just "always"              -> Just PrecedesAlways
               Just "never"               -> Just PrecedesNever
               Just "contextual"          -> Just PrecedesContextual
               _                          -> Nothing
         , nameDelimiterPrecedesLast  =
             case lookupAttribute "delimiter-precedes-last" attr of
               Just "after-inverted-name" -> Just PrecedesAfterInvertedName
               Just "always"              -> Just PrecedesAlways
               Just "never"               -> Just PrecedesNever
               Just "contextual"          -> Just PrecedesContextual
               _                          -> Nothing
         , nameEtAlMin                =
            lookupAttribute "et-al-min" attr >>= readAsInt
         , nameEtAlUseFirst           =
            lookupAttribute "et-al-use-first" attr >>= readAsInt
         , nameEtAlSubsequentUseFirst =
             lookupAttribute "et-al-subsequent-use-first" attr >>= readAsInt
         , nameEtAlSubsequentMin      =
             lookupAttribute "et-al-subsequent-min" attr >>= readAsInt
         , nameEtAlUseLast            =
             case lookupAttribute "et-al-use-last" attr of
               Just "true" -> Just True
               Just "false" -> Just False
               _           -> Nothing
         , nameForm                   =
             case lookupAttribute "form" attr of
               Just "short"  -> Just ShortName
               Just "count"  -> Just CountName
               Just "long"   -> Just LongName
               _             -> Nothing
         , nameInitialize             =
             case lookupAttribute "initialize" attr of
               Just "false" -> Just False
               Just "true" -> Just True
               _            -> Nothing
         , nameInitializeWith         =
             lookupAttribute "initialize-with" attr
         , nameAsSortOrder            =
             case lookupAttribute "name-as-sort-order" attr of
               Just "all"   -> Just NameAsSortOrderAll
               Just "first" -> Just NameAsSortOrderFirst
               _            -> Nothing
         , nameSortSeparator          =
             lookupAttribute "sort-separator" attr
         }
  return (nameformat, formatting)

pGroup :: X.Element -> ElementParser (Element a)
pGroup node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  es <- mapM pElement $ allChildren node
  return $ Element (EGroup False es) formatting

pText :: X.Element -> ElementParser (Element a)
pText node = do
  let attr = getAttributes node
  let formatting = getFormatting attr
  let varform = case lookupAttribute "form" attr of
                  Just "short" -> ShortForm
                  _            -> LongForm
  let termform = case lookupAttribute "form" attr of
                   Just "short"      -> Short
                   Just "verb"       -> Verb
                   Just "verb-short" -> VerbShort
                   Just "symbol"     -> Symbol
                   _                 -> Long
  let termnumber = case lookupAttribute "plural" attr of
                     Just "true"   -> Just Plural
                     Just "false"  -> Just Singular
                     _             -> Nothing
  elt <- case lookupAttribute "variable" attr of
           Just var -> return $ EText (TextVariable varform (toVariable var))
           Nothing ->
             case lookupAttribute "macro" attr of
               Just macroname -> return $ EText (TextMacro macroname)
               Nothing ->
                 case lookupAttribute "term" attr of
                   Just termname ->
                     return $ EText (TextTerm
                       Term { termName = termname
                            , termForm = termform
                            , termNumber = termnumber
                            , termGender = Nothing
                            , termGenderForm = Nothing
                            , termMatch = Nothing
                            })
                   Nothing ->
                     case lookupAttribute "value" attr of
                       Just val ->
                         return $ EText (TextValue val)
                       Nothing ->
                         parseFailure "text element lacks needed attribute"
  return $ Element elt formatting

pMacro :: X.Element -> ElementParser (Text, [Element a])
pMacro node = do
  name <- case lookupAttribute "name" (getAttributes node) of
            Just t  -> return t
            Nothing -> parseFailure "macro element missing name attribute"
  elts <- mapM pElement (allChildren node)
  return (name, elts)

getInheritableNameFormat :: Attributes -> NameFormat
getInheritableNameFormat attr =
  NameFormat
         { nameGivenFormatting = Nothing
         , nameFamilyFormatting = Nothing
         , nameAndStyle =
             case lookupAttribute "and" attr of
               Just "text"   -> Just Long
               Just "symbol" -> Just Symbol
               _             -> Nothing
         , nameDelimiter =
             lookupAttribute "name-delimiter" attr <|>
             lookupAttribute "names-delimiter" attr
         , nameDelimiterPrecedesEtAl  =
             case lookupAttribute "delimiter-precedes-et-al" attr of
               Just "after-inverted-name" -> Just PrecedesAfterInvertedName
               Just "always"              -> Just PrecedesAlways
               Just "never"               -> Just PrecedesNever
               Just "contextual"          -> Just PrecedesContextual
               _                          -> Nothing
         , nameDelimiterPrecedesLast  =
             case lookupAttribute "delimiter-precedes-last" attr of
               Just "after-inverted-name" -> Just PrecedesAfterInvertedName
               Just "always"              -> Just PrecedesAlways
               Just "never"               -> Just PrecedesNever
               Just "contextual"          -> Just PrecedesContextual
               _                          -> Nothing
         , nameEtAlMin                =
           lookupAttribute "et-al-min" attr >>= readAsInt
         , nameEtAlUseFirst           =
            lookupAttribute "et-al-use-first" attr >>= readAsInt
         , nameEtAlSubsequentUseFirst =
             lookupAttribute "et-al-subsequent-use-first" attr >>= readAsInt
         , nameEtAlSubsequentMin      =
             lookupAttribute "et-al-subsequent-min" attr >>= readAsInt
         , nameEtAlUseLast            =
             case lookupAttribute "et-al-use-last" attr of
               Just "true" -> Just True
               Just "false" -> Just False
               _           -> Nothing
         , nameForm                   =
             case lookupAttribute "name-form" attr of
               Just "short"  -> Just ShortName
               Just "count"  -> Just CountName
               Just "long"   -> Just LongName
               _             -> Nothing
         , nameInitialize             =
             case lookupAttribute "initialize" attr of
               Just "false" -> Just False
               Just "true" -> Just True
               _            -> Nothing
         , nameInitializeWith         =
             lookupAttribute "initialize-with" attr
         , nameAsSortOrder            =
             case lookupAttribute "name-as-sort-order" attr of
               Just "all"   -> Just NameAsSortOrderAll
               Just "first" -> Just NameAsSortOrderFirst
               _            -> Nothing
         , nameSortSeparator          =
             lookupAttribute "sort-separator" attr
         }

pLayout :: X.Element -> ElementParser (Layout a)
pLayout node = do
  let attr = getAttributes node
  let nameformat = getInheritableNameFormat attr
  let layouts = getChildren "layout" node
  -- In case there are multiple layouts (as CSL-M allows), we raise an error
  let elname = T.unpack $ X.nameLocalName $ X.elementName node
  layout <- case layouts of
              [] -> parseFailure $ "No layout element present in " <> elname
              [l] -> return l
              (_:_) -> parseFailure $ "Multiple layout elements present in " <> elname
  let formatting = getFormatting . getAttributes $ layout
  let sorts   = getChildren "sort" node
  elements <- mapM pElement $ allChildren layout
  let opts = LayoutOptions
             { layoutCollapse =
                 case lookupAttribute "collapse" attr of
                   Just "citation-number" -> Just CollapseCitationNumber
                   Just "year"            -> Just CollapseYear
                   Just "year-suffix"     -> Just CollapseYearSuffix
                   Just "year-suffix-ranged"
                                          -> Just CollapseYearSuffixRanged
                   _                      -> Nothing
             , layoutYearSuffixDelimiter =
                 lookupAttribute "year-suffix-delimiter" attr <|>
                   -- technically the spec doesn't say this, but
                   -- this seems to be what the test suites want?:
                   lookupAttribute "cite-group-delimiter" attr <|>
                   formatDelimiter formatting
             , layoutAfterCollapseDelimiter =
                 lookupAttribute "after-collapse-delimiter" attr <|>
                   formatDelimiter formatting
             , layoutNameFormat = nameformat
             }
  sortKeys <- mapM pSortKey (concatMap (getChildren "key") sorts)
  return $ Layout { layoutOptions  = opts
                  , layoutFormatting = formatting{
                                         formatAffixesInside = True }
                  , layoutElements = elements
                  , layoutSortKeys = sortKeys
                  }

pSortKey :: X.Element -> ElementParser (SortKey a)
pSortKey node = do
  let attr@(Attributes attr') = getAttributes node
  let direction = case lookupAttribute "sort" attr of
                    Just "descending" -> Descending
                    _                 -> Ascending
  -- The attributes names-min, names-use-first, and names-use-last may
  -- be used to override the values of the corresponding
  -- et-al-min/et-al-subsequent-min,
  -- et-al-use-first/et-al-subsequent-use-first and et-al-use-last
  -- attributes, and affect all names generated via macros called by
  -- cs:key.
  let keyChange "names-min" = "et-al-min"
      keyChange "names-use-first" = "et-al-use-first"
      keyChange "names-use-last" = "et-al-use-last"
      keyChange "names-subsequent-min" = "et-al-subsequent-min"
      keyChange "names-subsequent-use-first" = "et-al-subsequent-use-first"
      keyChange x = x
  let nameformat = getInheritableNameFormat
                     (Attributes (M.mapKeys keyChange attr'))
  case lookupAttribute "macro" attr of
      Just macroname -> return $ SortKeyMacro direction nameformat macroname
      Nothing   -> return $ SortKeyVariable direction
                     (toVariable $ fromMaybe mempty $
                       lookupAttribute "variable" attr)

splitVars :: Text -> [Variable]
splitVars = map toVariable . T.words . T.strip
