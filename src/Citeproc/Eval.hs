{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Citeproc.Eval
  ( evalStyle )
where
import Citeproc.Types
import Citeproc.Style (mergeLocales)
import qualified Citeproc.Unicode as Unicode
import Control.Monad.Trans.RWS.CPS
import Data.Containers.ListUtils (nubOrdOn, nubOrd)
import Safe (headMay, headDef, lastMay, initSafe, tailSafe, maximumMay)
import Data.Maybe
import Control.Monad (foldM, foldM_, zipWithM, when, unless)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Coerce (coerce)
import Data.List (find, intersperse, sortBy, sortOn, groupBy, foldl', transpose,
                  sort, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isDigit, isUpper, isLower, isLetter,
                  ord, chr)
import Text.Printf (printf)
import Control.Applicative
import Data.Generics.Uniplate.Operations (universe, transform)

-- import Debug.Trace (trace)
--
-- traceShowIdLabeled :: Show a => String -> a -> a
-- traceShowIdLabeled label x =
--   trace (label ++ ": " ++ show x) x
--
-- import Text.Show.Pretty (ppShow)
-- ppTrace :: Show a => a -> a
-- ppTrace x = trace (ppShow x) x

data Context a =
  Context
  { contextLocale              :: Locale
  , contextCollate             :: [SortKeyValue] -> [SortKeyValue] -> Ordering
  , contextAbbreviations       :: Maybe Abbreviations
  , contextStyleOptions        :: StyleOptions
  , contextLocator             :: Maybe Text
  , contextLabel               :: Maybe Text
  , contextPosition            :: [Position]
  , contextInSubstitute        :: Bool
  , contextInSortKey           :: Bool
  , contextInBibliography      :: Bool
  , contextSubstituteNamesForm :: Maybe NamesFormat
  }

-- used internally for group elements, which
-- are skipped if (a) the group calls a variable
-- but (b) all of the variables called are empty.
data VarCount =
  VarCount
  { variablesAccessed :: Int
  , variablesNonempty :: Int
  } deriving (Show)

data EvalState a =
  EvalState
  { stateVarCount       :: VarCount
  , stateLastCitedMap   :: M.Map ItemId (Int, Maybe Int, Int,
                                          Bool, Maybe Text, Maybe Text)
                        -- (citegroup, noteNum, posInGroup,
                        --      aloneInCitation, label, locator)
  , stateNoteMap        :: M.Map Int (Set.Set ItemId) -- ids cited in note
  , stateRefMap         :: ReferenceMap a
  , stateReference      :: Reference a
  , stateUsedYearSuffix :: Bool
  , stateUsedIdentifier :: Bool
  -- ^ tracks whether an identifier (DOI,PMCID,PMID,URL) has yet been used
  , stateUsedTitle      :: Bool
  -- ^ tracks whether the item title has yet been used
  } deriving (Show)


type Eval a = RWS (Context a) (Set.Set Text) (EvalState a)

updateVarCount :: Int -> Int -> Eval a ()
updateVarCount total' nonempty' =
  modify $ \st ->
    let VarCount{ variablesAccessed = total
                , variablesNonempty = nonempty } = stateVarCount st
     in st{ stateVarCount =
              VarCount { variablesAccessed = total + total',
                         variablesNonempty = nonempty + nonempty' } }

evalStyle  :: CiteprocOutput a
           => Style a          -- ^ Parsed CSL style.
           -> Maybe Lang       -- ^ Override style default locale.
           -> [Reference a]    -- ^ List of references (bibliographic data).
           -> [Citation a]     -- ^ List of citations.
           -> ([Output a], [(Text, Output a)], [Text])
                       -- ^ (citations, (id, bibentry) pairs, warnings)
evalStyle style mblang refs' citations =
  (citationOs, bibliographyOs, Set.toList warnings)
 where
  (refs, refmap) = makeReferenceMap refs'

  ((citationOs, bibliographyOs), warnings) = evalRWS go
     Context
      { contextLocale              = mergeLocales mblang style
      , contextCollate             = \xs ys ->
                                       compSortKeyValues (Unicode.comp mblang)
                                       xs ys
      , contextAbbreviations       = styleAbbreviations style
      , contextStyleOptions        = styleOptions style
      , contextLocator             = Nothing
      , contextLabel               = Nothing
      , contextPosition            = []
      , contextInSubstitute        = False
      , contextInSortKey           = False
      , contextInBibliography      = False
      , contextSubstituteNamesForm = Nothing
      }
      EvalState
      { stateVarCount = VarCount 0 0
      , stateLastCitedMap = mempty
      , stateNoteMap = mempty
      , stateRefMap = refmap
      , stateReference = Reference mempty mempty Nothing mempty
      , stateUsedYearSuffix = False
      , stateUsedIdentifier = False
      , stateUsedTitle = False
      }

  assignCitationNumbers sortedIds =
    modify $ \st ->
              st{ stateRefMap = ReferenceMap $ foldl'
                     (\m (citeId, num) ->
                         M.adjust (\ref ->
                           ref{ referenceVariables =
                                 M.insert "citation-number"
                                    (NumVal num) .
                                 M.alter (addIfMissing (citationLabel ref))
                                    "citation-label"
                                 $ referenceVariables ref
                              }) citeId m)
                     (unReferenceMap (stateRefMap st))
                     (zip sortedIds [1..]) }

  addIfMissing x Nothing  = Just x
  addIfMissing _ (Just x) = Just x

  go = do
      -- list of citationItemIds that are actually cited
      let citationOrder = M.fromList $ reverse $ zip
            (concatMap (map citationItemId . citationItems) citations)
            [(1 :: Int)..]
      let citeIds = M.keysSet citationOrder
      let sortedCiteIds = sortOn
              (fromMaybe maxBound . (`M.lookup` citationOrder))
              (map referenceId refs)
      let layoutOpts = layoutOptions $ styleCitation style
      let mbcgDelim =
            case styleCiteGroupDelimiter (styleOptions style) of
              Just x -> Just x
              Nothing
                -- grouping is activated whenever there is
                -- collapsing; this is the default
                -- cite-group-delimiter
                | isJust (layoutCollapse layoutOpts) -> Just ", "
                | otherwise -> Nothing

      assignCitationNumbers sortedCiteIds
      -- sorting of bibliography, insertion of citation-number
      collate <- asks contextCollate

      (bibCitations, bibSortKeyMap) <-
        case styleBibliography style of
          Nothing -> return ([], mempty)
          Just biblayout -> do
            bibSortKeyMap <- M.fromList
                      <$> mapM
                          ((\citeId ->
                             (citeId,) <$> evalSortKeys biblayout citeId)
                             . referenceId)
                          refs
            let sortedIds =
                  if null (layoutSortKeys biblayout)
                     then sortedCiteIds
                     else sortBy
                       (\x y -> collate
                                  (fromMaybe [] $ M.lookup x bibSortKeyMap)
                                  (fromMaybe [] $ M.lookup y bibSortKeyMap))
                            (map referenceId refs)
            assignCitationNumbers $
              case layoutSortKeys biblayout of
                (SortKeyVariable Descending "citation-number":_)
                  -> reverse sortedIds
                (SortKeyMacro Descending
                  (Element (ENumber "citation-number" _) _:_) : _)
                  -> reverse sortedIds
                (SortKeyMacro Descending
                  (Element (EText (TextVariable _ "citation-number")) _:_): _)
                  -> reverse sortedIds
                _ -> sortedIds
            let bibCitations = map (\ident ->
                  Citation (Just $ unItemId ident) Nothing
                   [CitationItem ident Nothing Nothing
                      NormalCite Nothing Nothing]) sortedIds
            return (bibCitations, bibSortKeyMap)
      -- styling of citations
      sortKeyMap <-
        foldM (\m citeId -> do
                  sk <- evalSortKeys (styleCitation style) citeId
                  return $ M.insert citeId sk m)
               M.empty
               citeIds
      -- We can't just sort all the citations, because
      -- this can make a hash out of prefixes and suffixes.
      -- See e.g. pandoc-citeproc issue #292.
      -- we need to first break into groups so that any
      -- suffix ends a group and any prefix begins a group;
      -- then sort the groups; then combine again:
      let canGroup i1 i2
           =   isNothing (citationItemSuffix i1) &&
               isNothing (citationItemPrefix i2)
      let sortCitationItems citation' =
            citation'{ citationItems =
                          concatMap
                           (sortBy
                             (\item1 item2 ->
                               collate
                                (fromMaybe [] $ M.lookup
                                   (citationItemId item1) sortKeyMap)
                                (fromMaybe [] $ M.lookup
                                   (citationItemId item2) sortKeyMap)))
                        $ groupBy canGroup
                        $ citationItems citation' }
      let citCitations = map sortCitationItems citations
      cs <- disambiguateCitations style bibSortKeyMap citCitations
      let cs' = case mbcgDelim of
                   Nothing -> cs
                   Just citeGroupDelim -> map
                      (groupAndCollapseCitations citeGroupDelim
                       (layoutYearSuffixDelimiter layoutOpts)
                       (layoutAfterCollapseDelimiter layoutOpts)
                       (layoutCollapse layoutOpts))
                      cs

      let removeIfEqual x y
           | x == y    = NullOutput
           | otherwise = y
      let removeNamesIfSuppressAuthor
           (Tagged (TagItem SuppressAuthor cid') x)
             = let y = getAuthors x
                in Tagged (TagItem SuppressAuthor cid')
                     (transform (removeIfEqual y) x)
          removeNamesIfSuppressAuthor x = x

      -- we need to do this after disambiguation and collapsing
      let handleSuppressAuthors = transform removeNamesIfSuppressAuthor

      let isNoteCitation = styleIsNoteStyle (styleOptions style)

      -- if we have an author-only citation at the beginning
      -- separate it out:
      let handleAuthorOnly formattedCit =
            case formattedCit of
              Formatted f
                (x@(Tagged (TagItem AuthorOnly _) _):xs)
                  | isNoteCitation
                    -> formatted mempty
                        (x : [InNote (formatted f xs) | not (null xs)])
                  | otherwise
                    -> formatted mempty
                        (x :
                         if null xs
                            then []
                            else [Literal (fromText " "),
                                  formatted f xs])
              Formatted f
                (Formatted f'
                  (x@(Tagged (TagItem AuthorOnly _) _):xs) : ys)
                  | isNoteCitation
                    -> formatted mempty
                        (x :
                         if null xs && null ys
                            then []
                            else [InNote (formatted f
                                           (formatted f' xs : ys))])
                  | otherwise
                    -> Formatted mempty
                        (x :
                         if null xs && null ys
                            then []
                            else [Literal (fromText " "),
                                  formatted f (formatted f' xs : ys)])
              _ | isNoteCitation -> InNote formattedCit
                | otherwise      -> formattedCit

      let cs'' = map (handleSuppressAuthors . handleAuthorOnly) cs'

      -- styling of bibliography (this needs to go here to take account
      -- of year suffixes added in disambiguation)
      bs <- case styleBibliography style of
               Just biblayout
                 -> local (\context ->
                             context{ contextInBibliography = True }) $
                    mapM (evalLayout biblayout) (zip [1..] bibCitations)
                    >>= \bs ->
                      case styleSubsequentAuthorSubstitute
                            (styleOptions style) of
                        Nothing -> return bs
                        Just subs -> subsequentAuthorSubstitutes subs bs
               Nothing -> return []
      return (cs'', case styleBibliography style of
                     Nothing -> []
                     Just _  ->
                       zip (map (fromMaybe "" . citationId) bibCitations) bs)

subsequentAuthorSubstitutes :: CiteprocOutput a
                            => SubsequentAuthorSubstitute
                            -> [Output a]
                            -> Eval a [Output a]
subsequentAuthorSubstitutes (SubsequentAuthorSubstitute t rule) =
  return . groupCitesByNames
 where
  groupCitesByNames [] = []
  groupCitesByNames (x:xs) =
    let xnames = fromMaybe ([],NullOutput) $ getNames x
        samenames = replaceMatch rule (fromText t) xnames xs
        rest = drop (length samenames) xs
    in  (x : samenames) ++ groupCitesByNames rest
  getNames (Formatted _ (x:_)) =
    case [(ns,r) | (Tagged (TagNames _ _ ns) r) <- universe x] of
      ((ns,r) : _) -> Just (ns,r)
      []           -> Nothing
  getNames _ = Nothing

replaceMatch :: CiteprocOutput a
             => SubsequentAuthorSubstituteRule
             -> a
             -> ([Name], Output a)
             -> [Output a]
             -> [Output a]
replaceMatch _ _ _ [] = []
replaceMatch rule replacement (names, raw) (z:zs) =
  case go z of
    Nothing -> []
    Just z' -> z' : replaceMatch rule replacement (names, raw) zs
 where
  go (Tagged t@TagItem{} y) =
    Tagged t <$> go y
  go (Formatted f (y:ys)) =
    Formatted f . (: ys) <$> go y
  go y@(Tagged (TagNames _ _ ns) r) =
    case (if null names then CompleteAll else rule) of
        CompleteAll ->
          if ns == names && (not (null names) || r == raw)
             then Just $ replaceAll y
             else Nothing
        CompleteEach ->
          if ns == names
             then Just $ transform replaceEach y
             else Nothing
        PartialEach ->
          case numberOfMatches ns names of
            num | num >= 1 -> Just $ transform (replaceFirst num) y
            _ -> Nothing
        PartialFirst ->
          case numberOfMatches ns names of
            num | num >= (1 :: Int) -> Just $ transform (replaceFirst 1) y
            _ -> Nothing
  go _ = Nothing
  replaceAll (Tagged (TagNames t' nf ns') x)
     = Tagged (TagNames t' nf ns') $
       -- removeName will leave label "ed."
       -- which we want, but it will also leave the substituted
       -- title when there is no name, which we do not want.
       -- So, if ns' is null, then we just remove everything.
       if null ns'
          then Literal replacement
          else
            case transform removeName x of
              Formatted f' xs -> Formatted f' (Literal replacement : xs)
              _               -> Literal replacement
  replaceAll x = x
  removeName (Tagged (TagName _) _) = NullOutput
  removeName x = x
  replaceEach (Tagged (TagName n) _)
    | n `elem` names
     = Tagged (TagName n) (Literal replacement)
  replaceEach x = x
  replaceFirst num x@(Tagged (TagNames _ _ ns') _)
    -- a kludge to get this to type-check!
    | True = foldr (transform . replaceName) x $ take num ns'
    | False = Literal replacement
  replaceFirst _num x = x
  replaceName name (Tagged (TagName n) _)
    | n == name = Tagged (TagName n) (Literal replacement)
  replaceName _ x = x
  numberOfMatches (a:as) (b:bs)
    | a == b    = 1 + numberOfMatches as bs
    | otherwise = 0
  numberOfMatches _ _ = 0

--
-- Disambiguation
--

data DisambData =
  DisambData
  { ddItem       :: ItemId
  , ddNames      :: [Name]
  , ddDates      :: [Date]
  , ddRendered   :: Text
  } deriving (Eq, Ord, Show)

disambiguateCitations :: forall a . CiteprocOutput a
                      => Style a
                      -> M.Map ItemId [SortKeyValue]
                      -> [Citation a]
                      -> Eval a [Output a]
disambiguateCitations style bibSortKeyMap citations = do
  refs <- unReferenceMap <$> gets stateRefMap
  let refIds = M.keys refs
  let citeIds = concatMap (map citationItemId . citationItems) citations
  let citeIdsSet = Set.fromList citeIds
  let ghostItems = [ ident
                   | ident <- refIds
                   , not (ident `Set.member` citeIdsSet)]

  -- for purposes of disambiguation, we remove prefixes and
  -- suffixes and locators, and we convert author-in-text to normal citation.
  let removeAffix item = item{ citationItemLabel = Nothing
                             , citationItemLocator = Nothing
                             , citationItemPrefix = Nothing
                             , citationItemSuffix = Nothing }
  let cleanCitation (Citation a b (i1:i2:is))
       | citationItemType i1 == AuthorOnly
       , citationItemType i2 == SuppressAuthor
        = Citation a b
            (map removeAffix (i2{ citationItemType = NormalCite }:is))
      cleanCitation (Citation a b is)
        = Citation a b (map removeAffix is)

  -- note that citations must go first, and order must be preserved:
  -- we use a "basic item" that strips off prefixes, suffixes, locators
  let citations' = map cleanCitation citations ++
                   [Citation Nothing Nothing (map basicItem ghostItems)]
  allCites <- renderCitations citations'

  mblang <- asks (localeLanguage . contextLocale)
  styleOpts <- asks contextStyleOptions
  let strategy = styleDisambiguation styleOpts
  let allNameGroups = [ns | Tagged (TagNames _ _ ns) _ <-
                              concatMap universe allCites]
  let allNames = nubOrd $ concat allNameGroups
  let primaryNames = nubOrd $ concatMap (take 1) allNameGroups
  allCites' <-
    case disambiguateAddGivenNames strategy of
         Nothing     -> return allCites
         Just ByCite -> return allCites -- do this later
         Just rule   -> do -- disambiguate names, not just citations
           let relevantNames =
                 case rule of
                   PrimaryNameWithInitials -> primaryNames
                   PrimaryName -> primaryNames
                   _ -> allNames
           let familyNames = nubOrd $ mapMaybe nameFamily relevantNames
           let grps = map (\name ->
                             [v | v <- relevantNames
                                , nameFamily v == Just name])
                          familyNames
           let toHint names name =
                  if any (initialsMatch mblang name) (filter (/= name) names)
                     then
                       case rule of
                         AllNamesWithInitials    -> Nothing
                         PrimaryNameWithInitials -> Nothing
                         PrimaryName             -> Just AddGivenNameIfPrimary
                         _                       -> Just AddGivenName
                     else
                       case rule of
                         PrimaryNameWithInitials -> Just AddInitialsIfPrimary
                         PrimaryName             -> Just AddInitialsIfPrimary
                         _                       -> Just AddInitials
           let namesMap = mconcat $ map
                  (\names -> if length names > 1
                                 then foldr
                                    (\name ->
                                        case toHint names name of
                                          Just x -> M.insert name x
                                          Nothing -> id)
                                    mempty
                                    names
                                 else mempty) grps
           -- use this same names map for every citation
           modify $ \st ->
              st{ stateRefMap = ReferenceMap $
                   foldr
                     (M.adjust (alterReferenceDisambiguation
                       (\d -> d{ disambNameMap = namesMap })))
                     (unReferenceMap $ stateRefMap st)
                     refIds }
           -- redo citations
           renderCitations citations'

  case getAmbiguities allCites' of
    []          -> return ()
    ambiguities -> analyzeAmbiguities mblang strategy citations' ambiguities
  renderCitations citations

 where

  renderCitations :: [Citation a] -> Eval a [Output a]
  renderCitations cs =
    withRWST (\ctx st -> (ctx,
                          st { stateLastCitedMap = mempty
                             , stateNoteMap = mempty })) $
     mapM (evalLayout (styleCitation style)) (zip [1..] cs)

  refreshAmbiguities :: [Citation a] -> Eval a [[DisambData]]
  refreshAmbiguities = fmap getAmbiguities . renderCitations

  analyzeAmbiguities :: Maybe Lang
                     -> DisambiguationStrategy
                     -> [Citation a]
                     -> [[DisambData]]
                     -> Eval a ()
  analyzeAmbiguities mblang strategy cs ambiguities = do
    -- add names to et al.
    return ambiguities
      >>= (\as ->
           (if not (null as) && disambiguateAddNames strategy
               then do
                 mapM_ (tryAddNames mblang (disambiguateAddGivenNames strategy)) as
                 refreshAmbiguities cs
               else
                 return as))
      >>= (\as ->
           (case disambiguateAddGivenNames strategy of
                  Just ByCite | not (null as) -> do
                     mapM_ (tryAddGivenNames mblang) as
                     refreshAmbiguities cs
                  _           -> return as))
      >>= (\as ->
           (if not (null as) && disambiguateAddYearSuffix strategy
               then do
                 addYearSuffixes bibSortKeyMap as
                 refreshAmbiguities cs
               else return as))
      >>= mapM_ tryDisambiguateCondition

basicItem :: ItemId -> CitationItem a
basicItem iid = CitationItem
  { citationItemId      = iid
  , citationItemLabel   = Nothing
  , citationItemLocator = Nothing
  , citationItemType    = NormalCite
  , citationItemPrefix  = Nothing
  , citationItemSuffix  = Nothing
  }

isDisambiguated :: Maybe Lang
                -> Maybe GivenNameDisambiguationRule
                -> Int -- et al min
                -> [DisambData]
                -> DisambData
                -> Bool
isDisambiguated mblang mbrule etAlMin xs x =
  all (\y -> x == y || disambiguatedName y /= disambiguatedName x) xs
 where
  disambiguatedName = nameParts . take etAlMin . ddNames
  nameParts =
    case mbrule of
      Just AllNames -> id
      Just AllNamesWithInitials ->
           map (\name -> name{ nameGiven = initialize mblang True False ""
                                            <$> nameGiven name })
      Just PrimaryName ->
        \case
          [] -> []
          (z:zs) -> z : map (\name -> name{ nameGiven = Nothing }) zs
      Just PrimaryNameWithInitials ->
        \case
          [] -> []
          (z:zs) -> z{ nameGiven =
                     initialize mblang True False "" <$> nameGiven z } :
                     map (\name -> name{ nameGiven = Nothing }) zs
      Just ByCite -> id -- hints will be added later
      _ -> map (\name -> name{ nameGiven = Nothing })

tryAddNames :: Maybe Lang
            -> Maybe GivenNameDisambiguationRule
            -> [DisambData]
            -> Eval a ()
tryAddNames mblang mbrule bs =
                     (case mbrule of
                          Just ByCite -> bs <$ tryAddGivenNames mblang bs
                          _ -> return bs) >>= go 1
                        -- if ByCite, we want to make sure that
                        -- tryAddGivenNames is still applied, as
                        -- calculation of "add names" assumes this.
 where
   maxnames = fromMaybe 0 . maximumMay . map (length . ddNames)
   go n as
     | n > maxnames as = return ()
     | otherwise = do
         let ds = filter (isDisambiguated mblang mbrule n as) as
         if null ds
            then go (n + 1) as
            else do
              modify $ \st ->
                st{ stateRefMap = ReferenceMap
                      $ foldr (setEtAlNames (Just n) . ddItem)
                        (unReferenceMap $ stateRefMap st) as }
              go (n + 1) (as \\ ds)

tryAddGivenNames :: Maybe Lang
                 -> [DisambData]
                 -> Eval a ()
tryAddGivenNames mblang as = do
  let correspondingNames =
         map (zip (map ddItem as)) $ transpose $ map ddNames as
      go [] _ = return []
      go (as' :: [DisambData]) (ns :: [(ItemId, Name)]) = do
        hintedIds <- Set.fromList . catMaybes <$>
                        mapM (addNameHint mblang (map snd ns)) ns
        return $ filter (\x -> ddItem x `Set.notMember` hintedIds) as'
  foldM_ go as correspondingNames

addYearSuffixes :: M.Map ItemId [SortKeyValue]
                -> [[DisambData]]
                -> Eval a ()
addYearSuffixes bibSortKeyMap' as = do
  let allitems = concat as
  collate <- asks contextCollate
  let companions a =
        sortBy
        (\item1 item2 ->
          collate
            (fromMaybe [] $ M.lookup (ddItem item1) bibSortKeyMap')
            (fromMaybe [] $ M.lookup (ddItem item2) bibSortKeyMap'))
        (concat [ x | x <- as, a `elem` x ])
  let groups = Set.map companions $ Set.fromList allitems
  let addYearSuffix item suff =
        modify $ \st ->
          st{ stateRefMap = ReferenceMap
               $ setYearSuffix suff item
               $ unReferenceMap
               $ stateRefMap st }
  mapM_ (\xs -> zipWithM addYearSuffix (map ddItem xs) [1..]) groups

tryDisambiguateCondition :: [DisambData] -> Eval a ()
tryDisambiguateCondition as =
  case as of
    [] -> return ()
    xs -> modify $ \st ->
            st{ stateRefMap = ReferenceMap
                $ foldr (setDisambCondition True . ddItem)
                  (unReferenceMap (stateRefMap st))
                  xs }

alterReferenceDisambiguation :: (DisambiguationData -> DisambiguationData)
                             -> Reference a
                             -> Reference a
alterReferenceDisambiguation f r =
      r{ referenceDisambiguation = f <$>
           case referenceDisambiguation r of
             Nothing -> Just
               DisambiguationData
                 { disambYearSuffix  = Nothing
                 , disambNameMap     = mempty
                 , disambEtAlNames   = Nothing
                 , disambCondition   = False
               }
             Just x  -> Just x }

initialsMatch :: Maybe Lang -> Name -> Name -> Bool
initialsMatch mblang x y =
  case (nameGiven x, nameGiven y) of
    (Just x', Just y') ->
      initialize mblang True False "" x' ==
        initialize mblang True False "" y'
    _ -> False

addNameHint :: Maybe Lang -> [Name] -> (ItemId, Name) -> Eval a (Maybe ItemId)
addNameHint mblang names (item, name) = do
  let familyMatches = [n | n <- names
                         , n /= name
                         , nameFamily n == nameFamily name]
  case familyMatches of
    [] -> return Nothing
    _  -> do
      let hint = if any (initialsMatch mblang name) familyMatches
                    then AddGivenName
                    else AddInitials
      modify $ \st ->
        st{ stateRefMap = ReferenceMap
            $ setNameHint hint name item
            $ unReferenceMap (stateRefMap st) }
      return $ Just item

setNameHint :: NameHints -> Name -> ItemId
            -> M.Map ItemId (Reference a) -> M.Map ItemId (Reference a)
setNameHint hint name = M.adjust
       (alterReferenceDisambiguation
         (\d -> d{ disambNameMap =
                     M.insert name hint
                     (disambNameMap d) }))

setEtAlNames :: Maybe Int -> ItemId
             -> M.Map ItemId (Reference a) -> M.Map ItemId (Reference a)
setEtAlNames x = M.adjust
       (alterReferenceDisambiguation
         (\d -> d{ disambEtAlNames = x }))

setYearSuffix :: Int -> ItemId
              -> M.Map ItemId (Reference a) -> M.Map ItemId (Reference a)
setYearSuffix x = M.adjust
       (alterReferenceDisambiguation
         (\d -> d{ disambYearSuffix = Just x }))

setDisambCondition :: Bool -> ItemId
                   -> M.Map ItemId (Reference a) -> M.Map ItemId (Reference a)
setDisambCondition x = M.adjust
       (alterReferenceDisambiguation
         (\d -> d{ disambCondition = x }))

getAmbiguities :: CiteprocOutput a => [Output a] -> [[DisambData]]
getAmbiguities =
        mapMaybe
           (\zs ->
               case zs of
                 []     -> Nothing
                 [_]    -> Nothing
                 (z:_) ->
                   case ddRendered z of
                     "" -> Nothing
                     _  -> case nubOrdOn ddItem zs of
                             ys@(_:_:_) -> Just ys -- > 1 ambiguous entry
                             _          -> Nothing)
      . groupBy (\x y -> ddRendered x == ddRendered y)
      . sortOn ddRendered
      . map toDisambData
      . extractTagItems

extractTagItems :: [Output a] -> [(ItemId, Output a)]
extractTagItems xs =
  [(iid, x) | Tagged (TagItem NormalCite iid) x <- concatMap universe xs
            , not (hasIbid x)]
 where -- we don't want two "ibid" entries to be treated as ambiguous.
  hasIbid x = not $ null [ trm | Tagged (TagTerm trm) _ <- universe x
                               , termName trm == "ibid" ]


toDisambData :: CiteprocOutput a => (ItemId, Output a) -> DisambData
toDisambData (iid, x) =
  let xs = universe x
      ns' = getNames xs
      ds' = getDates xs
      t   = outputToText x
   in DisambData { ddItem = iid
                 , ddNames = ns'
                 , ddDates = ds'
                 , ddRendered = t }
 where
  getNames :: [Output a] -> [Name]
  getNames (Tagged (TagNames _ _ ns) _ : xs)
                  = ns ++ getNames xs
  getNames (_ : xs)   = getNames xs
  getNames []         = []

  getDates :: [Output a] -> [Date]
  getDates (Tagged (TagDate d) _ : xs)
                  = d : getDates xs
  getDates (_ : xs)   = getDates xs
  getDates []         = []


--
-- Grouping and collapsing
--

groupAndCollapseCitations :: forall a . CiteprocOutput a
                          => Text
                          -> Maybe Text
                          -> Maybe Text
                          -> Maybe Collapsing
                          -> Output a
                          -> Output a
groupAndCollapseCitations citeGroupDelim yearSuffixDelim afterCollapseDelim
  collapsing (Formatted f xs) =
   case collapsing of
      Just CollapseCitationNumber ->
        Formatted f{ formatDelimiter = Nothing } $
            foldr collapseRange []
                  (groupSuccessive isAdjacentCitationNumber xs)
      Just collapseType ->
          Formatted f{ formatDelimiter = Nothing } $
            foldr (collapseGroup collapseType) [] (groupWith sameNames xs)
      Nothing ->
          Formatted f $
             map (Formatted mempty{ formatDelimiter = Just citeGroupDelim })
                 (groupWith sameNames xs)
 where
  --   Note that we cannot assume we've sorted by name,
  --   so we can't just use Data.ListgroupBy.  We also
  --   take care not to move anything past a prefix or suffix.
  groupWith :: (Output a -> Output a -> Bool)
            -> [Output a]
            -> [[Output a]]
  groupWith _ [] = []
  groupWith isMatched (z:zs)
   | hasSuffix z = [z] : groupWith isMatched zs
   | otherwise =  -- we allow a prefix on first item in collapsed group
    case span hasNoPrefixOrSuffix zs of
      ([],ys) -> [z] : groupWith isMatched ys
      (ws,ys) ->
        (z : filter (isMatched z) ws) :
          groupWith isMatched (filter (not . isMatched z) ws ++ ys)

  hasNoPrefixOrSuffix :: Output a -> Bool
  hasNoPrefixOrSuffix x = not (hasPrefix x) && not (hasSuffix x)

  hasPrefix :: Output a -> Bool
  hasPrefix x = not $ null [y | y@(Tagged TagPrefix _) <- universe x]

  hasSuffix :: Output a -> Bool
  hasSuffix x = not $ null [y | y@(Tagged TagSuffix _) <- universe x]

  collapseRange :: [Output a] -> [Output a] -> [Output a]
  collapseRange ys zs
    | length ys >= 3
    , Just yhead <- headMay ys
    , Just ylast <- lastMay ys
      = Formatted mempty{ formatDelimiter = Just $ T.singleton enDash }
                  [yhead, ylast] :
                  if null zs
                     then []
                     else maybe NullOutput literal afterCollapseDelim : zs
  collapseRange ys zs =
    Formatted mempty{ formatDelimiter = formatDelimiter f } ys :
      if null zs
         then []
         else maybe NullOutput literal (formatDelimiter f) : zs

  collapseGroup :: Collapsing -> [Output a] -> [Output a] -> [Output a]
  collapseGroup _ [] zs = zs
  collapseGroup collapseType (y:ys) zs =
    let ys' = y : map (transform removeNames) ys
        ws = collapseYearSuffix collapseType ys'
        noCollapse = ws == y:ys
        noYearSuffixCollapse = ws == ys'
        hasLocator u = not $ null [x | x@(Tagged TagLocator _) <- universe u]
        anyHasLocator = any hasLocator ws
        -- https://github.com/citation-style-language/test-suite/issues/36 :
        flippedAfterCollapseDelim = collapseType == CollapseYear
        addCGDelim u [] = [u]
        addCGDelim u us =
          Formatted mempty{ formatSuffix =
                              if noCollapse || noYearSuffixCollapse &&
                                 not (flippedAfterCollapseDelim &&
                                      anyHasLocator)
                                 then Just citeGroupDelim
                                 else afterCollapseDelim <|>
                                      formatDelimiter f } [u] : us
     in Formatted mempty{ formatDelimiter = Nothing
                        , formatSuffix =
                            if null zs
                               then Nothing
                               else if noCollapse &&
                                          not flippedAfterCollapseDelim
                                       then formatDelimiter f
                                       else afterCollapseDelim <|>
                                            formatDelimiter f }
                               (foldr addCGDelim [] ws) : zs
  collapseRanges = map rangifyGroup . groupSuccessive isSuccessive
  isSuccessive x y
    = case ([c | Tagged (TagYearSuffix c) _ <- universe x],
            [d | Tagged (TagYearSuffix d) _ <- universe y]) of
        ([c],[d]) -> d == c + 1
        _   -> False

  rangifyGroup :: [Output a] -> Output a
  rangifyGroup zs
    | length zs >= 3
    , Just zhead <- headMay zs
    , Just zlast <- lastMay zs
    = Formatted mempty{ formatDelimiter = Just (T.singleton enDash) }
                [zhead, zlast]
  rangifyGroup [z] = z
  rangifyGroup zs = Formatted mempty{ formatDelimiter = yearSuffixDelim
                                    } zs

  yearSuffixGroup :: Bool -> [Output a] -> Output a
  yearSuffixGroup _ [x] = x
  yearSuffixGroup useRanges zs  =
    Formatted mempty{ formatDelimiter = yearSuffixDelim }
      $ if useRanges then collapseRanges zs else zs

  collapseYearSuffix :: Collapsing -> [Output a] -> [Output a]
  collapseYearSuffix CollapseYearSuffix zs =
    reverse $ yearSuffixGroup False cur : items
   where
     (cur, items) = foldl' (goYearSuffix False) ([], []) zs
  collapseYearSuffix CollapseYearSuffixRanged zs =
    reverse $ yearSuffixGroup True cur : items
   where
     (cur, items) = foldl' (goYearSuffix True) ([], []) zs
  collapseYearSuffix _ zs = zs

  getDates :: Output a -> [Date]
  getDates x = [d | Tagged (TagDate d) _ <- universe x]

  getYears :: Output a -> [[Maybe Int]]
  getYears x = [map (\case
                        DateParts (y:_) -> Just y
                        _               -> Nothing) (dateParts d)
                | d <- getDates x
                , isNothing (dateLiteral d)]

  goYearSuffix :: Bool -> ([Output a], [Output a]) -> Output a
               -> ([Output a], [Output a])
  goYearSuffix useRanges (cur, items) item =
    case cur of
      []     -> ([item], items)
      (z:zs)
        | getYears z == getYears item
          -> (z:zs ++ [x | x@(Tagged (TagYearSuffix _) _) <- universe item],
              items)
        | otherwise -> ([item], yearSuffixGroup useRanges (z:zs) : items)

  isAdjacentCitationNumber :: Output a -> Output a -> Bool
  isAdjacentCitationNumber
     (Tagged (TagItem _ _)
       (Formatted _f1 [Tagged (TagCitationNumber n1) _xs1]))
     (Tagged (TagItem _ _)
       (Formatted _f2 [Tagged (TagCitationNumber n2) _xs2]))
    = n2 == n1 + 1
  isAdjacentCitationNumber
     (Tagged (TagItem _ _) (Tagged (TagCitationNumber n1) _xs1))
     (Tagged (TagItem _ _) (Tagged (TagCitationNumber n2) _xs2))
    = n2 == n1 + 1
  isAdjacentCitationNumber _ _ = False

  sameNames :: Output a -> Output a -> Bool
  sameNames x y =
    case (extractTagged x, extractTagged y) of
      (Just (Tagged (TagNames t1 _nf1 ns1) ws1),
       Just (Tagged (TagNames t2 _nf2 ns2) ws2))
        -> t1 == t2 && (if ns1 == ns2
                           then not (null ns1) || ws1 == ws2
                           else ws1 == ws2)
      -- case where it's just a date with no name or anything;
      -- we treat this as same name e.g. (1955a,b)
      (Just (Tagged TagDate{} _), Just (Tagged TagDate{} _))
        -> True
          -- case where title is substituted
      _ -> False

  extractTagged :: Output a -> Maybe (Output a)
  extractTagged x =
    let items = [y | y@(Tagged (TagItem ty _) _) <- universe x
                   , ty /= AuthorOnly]
        names = [y | y@(Tagged TagNames{} _) <- concatMap universe items]
        dates = [y | y@(Tagged TagDate{} _) <- concatMap universe items]
    in  if null items
           then Nothing
           else listToMaybe names <|> listToMaybe dates

groupAndCollapseCitations _ _ _ _ x = x

takeSeq :: Show a => (a -> a -> Bool) -> [a] -> ([a], [a])
takeSeq isAdjacent (x1 : x2 : rest)
  | isAdjacent x1 x2 = (x1:ys, zs)
  where (ys, zs) = takeSeq isAdjacent (x2:rest)
takeSeq _ (y:ys) = ([y], ys)
takeSeq _ []     = ([], [])

groupSuccessive :: Show a => (a -> a -> Bool) -> [a] -> [[a]]
groupSuccessive isAdjacent zs =
  case takeSeq isAdjacent zs of
    ([],_)  -> []
    (xs,ys) -> xs : groupSuccessive isAdjacent ys

--
-- Sorting
--

evalSortKeys :: CiteprocOutput a
             => Layout a
             -> ItemId
             -> Eval a [SortKeyValue]
evalSortKeys layout citeId =
  withRWST (\ctx st -> (ctx{ contextInSortKey = True }, st)) $
    mapM (evalSortKey citeId) (layoutSortKeys layout)

evalSortKey :: CiteprocOutput a
            => ItemId
            -> SortKey a
            -> Eval a SortKeyValue
evalSortKey citeId (SortKeyMacro sortdir elts) = do
  refmap <- gets stateRefMap
  case lookupReference citeId refmap of
    Nothing  -> return $ SortKeyValue sortdir Nothing
    Just ref -> do
        k <- normalizeSortKey . toText .
              renderOutput defaultCiteprocOptions . grouped
              <$> withRWS newContext (mconcat <$> mapM eElement elts)
        return $ SortKeyValue sortdir (Just k)
     where
      newContext oldContext s =
        (oldContext, s{ stateReference = ref })
evalSortKey citeId (SortKeyVariable sortdir var) = do
  refmap <- gets stateRefMap
  SortKeyValue sortdir <$>
    case lookupReference citeId refmap >>= lookupVariable var of
      Nothing           -> return Nothing
      Just (TextVal t)  -> return $ Just $ normalizeSortKey t
      Just (NumVal  i)  -> return $ Just [T.pack $ printf "%09d" i]
      Just (FancyVal x) -> return $ Just $ normalizeSortKey $ toText x
      Just (NamesVal ns) ->
        Just . normalizeSortKey . mconcat . intersperse "," . map T.unwords
             <$> mapM getNamePartSortOrder ns
      Just (DateVal d)  -> return $ Just [T.toLower $ dateToText d]

-- Note: we do a case-insensitive sort (using toCaseFold):
normalizeSortKey :: Text -> [Text]
normalizeSortKey = filter (not . T.null) . T.split isWordSep . T.toCaseFold
 where
  isWordSep c = isSpace c || c == '\'' || c == '’' || c == ',' ||
                c == 'ʾ' || c == 'ʿ' -- ayn/hamza in transliterated arabic

-- absence should sort AFTER all values
-- see sort_StatusFieldAscending.txt, sort_StatusFieldDescending.txt
compSortKeyValue :: (Text -> Text -> Ordering)
                 -> SortKeyValue
                 -> SortKeyValue
                 -> Ordering
compSortKeyValue collate sk1 sk2 =
  case (sk1, sk2) of
    (SortKeyValue _ Nothing, SortKeyValue _ Nothing) -> EQ
    (SortKeyValue _ Nothing, SortKeyValue _ (Just _)) -> GT
    (SortKeyValue _ (Just _), SortKeyValue _ Nothing) -> LT
    (SortKeyValue Ascending (Just t1), SortKeyValue Ascending (Just t2)) ->
      collateKey t1 t2
    (SortKeyValue Descending (Just t1), SortKeyValue Descending (Just t2))->
      collateKey t2 t1
    _ -> EQ
 where
  collateKey :: [Text] -> [Text] -> Ordering
  collateKey [] [] = EQ
  collateKey [] (_:_) = LT
  collateKey (_:_) [] = GT
  collateKey (x:xs) (y:ys) =
    case collate x y of
      EQ -> collateKey xs ys
      GT -> GT
      LT -> LT

compSortKeyValues :: (Text -> Text -> Ordering)
                  -> [SortKeyValue]
                  -> [SortKeyValue]
                  -> Ordering
compSortKeyValues _ [] [] = EQ
compSortKeyValues _ [] (_:_) = LT
compSortKeyValues _ (_:_) [] = GT
compSortKeyValues collate (x:xs) (y:ys) =
  case compSortKeyValue collate x y of
    EQ -> compSortKeyValues collate xs ys
    GT -> GT
    LT -> LT

-- Note!  This prints negative (BC) dates as N(999,999,999 + y)
-- and positive (AD) dates as Py so they sort properly.  (Note that
-- our unicode sorting ignores punctuation, so we use a letter
-- rather than -.) Do not use out of context of sort keys.
dateToText :: Date -> Text
dateToText = mconcat . map (T.pack . go . coerce) . dateParts
 where
  go :: [Int] -> String
  go [] = ""
  go [y] = toYear y <> "0000"
  go [y,m] = toYear y <> printf "%02d" m <> "00"
  go (y:m:d:_) = toYear y <> printf "%02d" m <> printf "%02d" d
  toYear :: Int -> String
  toYear y
    | y < 0     = printf "N%09d" (999999999 + y)
    | otherwise = printf "P%09d" y


evalLayout :: CiteprocOutput a
            => Layout a
            -> (Int, Citation a)
            -> Eval a (Output a)
evalLayout layout (citationGroupNumber, citation) = do
  -- this is a hack to ensure that "ibid" detection will work
  -- correctly in a citation starting with an author-only:
  -- the combination AuthorOnly [SuppressAuthor] should not
  -- count against a later Ibid citation.
  let positionsInCitation =
        case citationItems citation of
          (c:_) | citationItemType c == AuthorOnly -> [0..]
          _ -> [1..]

  items <- mapM evalItem' (zip positionsInCitation (citationItems citation))

  -- see display_SecondFieldAlignMigratePunctuation.txt
  let moveSuffixInsideDisplay zs =
        case (lastMay zs, formatSuffix formatting) of
          (Just (Tagged (TagItem ct id') (Formatted f ys)), Just _) ->
            (\ys' -> initSafe zs ++
                      [Tagged (TagItem ct id') (Formatted f ys')]) <$>
                        moveSuffixInsideDisplay ys
          (Just (Formatted f ys), Just suff)
            | isJust (formatDisplay f) ->
                Just $ initSafe zs ++
                     [Formatted f{ formatSuffix = Just
                          (fromMaybe "" (formatSuffix f) <> suff) } ys]
            | otherwise -> (\ys' -> initSafe zs ++ [Formatted f ys']) <$>
                             moveSuffixInsideDisplay ys
          _ -> Nothing
  return $
    case moveSuffixInsideDisplay items of
      Nothing     -> formatted formatting items
      Just items' -> formatted formatting{ formatSuffix = Nothing } items'
 where
  formatting = layoutFormatting layout

  secondFieldAlign (x:xs) =
    formatted mempty{ formatDisplay = Just DisplayLeftMargin } [x]
    : [formatted mempty{ formatDisplay = Just DisplayRightInline } xs]
  secondFieldAlign [] = []

  evalItem' (positionInCitation :: Int, item) = do
    isBibliography  <- asks contextInBibliography

    styleOpts <- asks contextStyleOptions
    let isNote = styleIsNoteStyle styleOpts
    position <- getPosition citationGroupNumber (citationNoteNumber citation)
                     item positionInCitation

    xs <- evalItem layout (position, item)
    -- we only update the map in the citations section
    unless isBibliography $ do
      updateRefMap citationGroupNumber citation item
      updateLastCitedMap citationGroupNumber positionInCitation citation item

    return $
          maybe id (\pref x -> Tagged TagPrefix (grouped [Literal pref, x]))
                (citationItemPrefix item)
        . maybe id (\suff x -> Tagged TagSuffix (grouped [x, Literal suff]))
                   (citationItemSuffix item)
        . (\x -> case x of
                   NullOutput -> x
                   _          -> Tagged (TagItem (citationItemType item)
                                                  (citationItemId item)) x)
        . formatted mempty
        . (if citationItemType item == AuthorOnly
              then (:[]) . getAuthors . formatted mempty
              else id)
        . (case citationItemPrefix item of
             Just t | isNote
                    , ". " `T.isSuffixOf` toText t
                    , T.count " " (toText t) > 1 -- exclude single word
                                 -> capitalizeInitialTerm
             _                   -> id)
        . (if isBibliography
              then
                case styleSecondFieldAlign styleOpts of
                  Just SecondFieldAlignFlush  -> secondFieldAlign
                  Just SecondFieldAlignMargin -> secondFieldAlign -- TODO?
                  Nothing -> id
              else id)
        $ xs

evalItem :: CiteprocOutput a
         => Layout a -> ([Position], CitationItem a) -> Eval a [Output a]
evalItem layout (position, item) = do
  refmap <- gets stateRefMap

  let addLangToFormatting lang (Formatted f xs) =
        Formatted f{ formatLang = Just lang } xs
      addLangToFormatting _ x = x

  case lookupReference (citationItemId item) refmap of
    Just ref -> withRWST
      (\ctx st ->
       (ctx{ contextLocator = citationItemLocator item
           , contextLabel = citationItemLabel item
           , contextPosition = position
           },
        st{ stateReference = ref
          , stateUsedYearSuffix = False
          , stateUsedIdentifier = False
          , stateUsedTitle = False
          }))
        $ do xs <- mconcat <$> mapM eElement (layoutElements layout)

             -- find identifiers that can be used to hyperlink the title
             let mbident =
                    foldl (<|>) Nothing
                      [ IdentDOI   <$> (valToText =<< lookupVariable "DOI" ref)
                      , IdentPMCID <$> (valToText =<< lookupVariable "PMCID" ref)
                      , IdentPMID  <$> (valToText =<< lookupVariable "PMID" ref)
                      , IdentURL   <$> (valToText =<< lookupVariable "URL" ref)
                      ]
             let mburl = identifierToURL <$> mbident

             -- hyperlink any titles in the output
             let linkTitle url (Tagged TagTitle x) = Linked url [Tagged TagTitle x]
                 linkTitle _ x = x

             usedLink  <- gets stateUsedIdentifier
             usedTitle <- gets stateUsedTitle
             inBiblio  <- asks contextInBibliography

             -- when no links were rendered for a bibliography item, hyperlink
             -- the title, if it exists, otherwise hyperlink the whole item
             let xs' =
                   if usedLink || not inBiblio
                     then xs
                   else case mburl of
                         Nothing  -> xs
                         Just url -> if usedTitle
                                        -- hyperlink the title
                                        then fmap (transform (linkTitle url)) xs
                                        -- hyperlink the entire bib item
                                        else [Linked url xs]

             let mblang = lookupVariable "language" ref
                          >>= valToText
                          >>= either (const Nothing) Just . parseLang
             return $
               case mblang of
                 Nothing   -> xs'
                 Just lang -> map
                     (transform (addLangToFormatting lang)) xs'
    Nothing -> do
      warn $ "citation " <> unItemId (citationItemId item) <>
             " not found"
      return [Literal $ addFontWeight BoldWeight
         $ fromText $ unItemId (citationItemId item) <> "?"]



updateRefMap :: Int -> Citation a -> CitationItem a -> Eval a ()
updateRefMap citationGroupNumber citation item = do
  isNote <- asks (styleIsNoteStyle . contextStyleOptions)
  lastCitedMap <- gets stateLastCitedMap
  let notenum = NumVal $ fromMaybe citationGroupNumber (citationNoteNumber citation)
  -- keep track of how many citations in each note.
  -- two citations from the same note don't count as "alone"
  -- for ibid purposes. See citation-style-language/documentation#121
  case citationNoteNumber citation of
    Nothing -> return ()
    Just n  -> modify $ \st ->
                   st{ stateNoteMap = M.alter
                        (maybe
                          (Just (Set.singleton (citationItemId item)))
                          (Just . Set.insert (citationItemId item)))
                        n
                        (stateNoteMap st) }
  case M.lookup (citationItemId item) lastCitedMap of
    Nothing | isNote -> -- first citation
      modify $ \st ->
        st{ stateRefMap = ReferenceMap $
                M.adjust (\ref -> ref{ referenceVariables =
                  M.insert "first-reference-note-number" notenum
                             (referenceVariables ref)})
                  (citationItemId item)
               (unReferenceMap $ stateRefMap st) }
    _  -> return ()

updateLastCitedMap :: Int -> Int -> Citation a -> CitationItem a -> Eval a ()
updateLastCitedMap citationGroupNumber positionInCitation citation item = do
  unless (citationItemType item == AuthorOnly) $
    modify $ \st ->
      st{ stateLastCitedMap =
        M.insert (citationItemId item)
          (citationGroupNumber, citationNoteNumber citation,
           positionInCitation,
           (case citationItems citation of
              [_]   -> True
              [x,y] -> citationItemId x == citationItemId y
                      && citationItemType x == AuthorOnly
                      && citationItemType y == SuppressAuthor
              _     -> False),
           citationItemLabel item,
           citationItemLocator item)
        $ stateLastCitedMap st }


-- | The first-occurring element tagged as Names will be
-- treated as the "author"; generally that is the author
-- but it might be the editor if it's an edited volume.
getAuthors :: Output a -> Output a
getAuthors x =
  headDef NullOutput [y | y@(Tagged TagNames{} _) <- universe x]

removeNames :: Output a -> Output a
removeNames (Tagged TagNames{} _) = NullOutput
removeNames x = x

capitalizeInitialTerm :: [Output a] -> [Output a]
capitalizeInitialTerm [] = []
capitalizeInitialTerm (z:zs) = go z : zs
 where
  go (Tagged (TagTerm t) x) =
    Tagged (TagTerm t)
      (formatted mempty{ formatTextCase = Just CapitalizeFirst } [x])
  go (Formatted f xs) = Formatted f (capitalizeInitialTerm xs)
  go (Tagged tg x) = Tagged tg (go x)
  go x = x

getPosition :: Int -> Maybe Int -> CitationItem a -> Int -> Eval a [Position]
getPosition groupNum mbNoteNum item posInGroup = do
  inBibliography <- asks contextInBibliography
  if inBibliography
     then return []
     else getPosition'
 where
  getPosition' = do
    lastCitedMap <- gets stateLastCitedMap
    noteMap <- gets stateNoteMap
    case M.lookup (citationItemId item) lastCitedMap of
      Nothing -> return [FirstPosition]
      Just (prevGroupNum, mbPrevNoteNum,
             prevPosInGroup, prevAloneInGroup,
             prevLabel, prevLoc) -> do
        isNote <- asks (styleIsNoteStyle . contextStyleOptions)
        nearNoteDistance <- fromMaybe 5 <$>
                asks (styleNearNoteDistance . contextStyleOptions)
        let noteNum = fromMaybe groupNum mbNoteNum
        let prevNoteNum = fromMaybe prevGroupNum mbPrevNoteNum
        let prevAloneInNote =
              case M.lookup prevNoteNum noteMap of
                Nothing -> True
                Just s  -> Set.size s <= 1
        let prevAlone = prevAloneInGroup && prevAloneInNote
        return $
          (if isNote && noteNum - prevNoteNum < nearNoteDistance
              then (NearNote :)
              else id) .
          (if (groupNum == prevGroupNum &&
               posInGroup == prevPosInGroup + 1) ||
              (groupNum == prevGroupNum + 1 &&
                (((-) <$> mbNoteNum <*> mbPrevNoteNum) <= Just 1) &&
               posInGroup == 1 &&
               prevAlone)
               then case (prevLoc, citationItemLocator item) of
                      (Nothing, Just _)
                        -> (IbidWithLocator :) . (Ibid :)
                      (Nothing, Nothing) -> (Ibid :)
                      (Just _, Nothing)   -> id
                      (Just l1, Just l2)
                        | l1 == l2
                        , citationItemLabel item == prevLabel
                          -> (Ibid :)
                        | otherwise
                          -> (IbidWithLocator :) . (Ibid :)
               else id)
          $ [Subsequent]

eElement :: CiteprocOutput a => Element a -> Eval a [Output a]
eElement (Element etype formatting) =
  case etype of
    EText textType ->
      (:[]) <$> withFormatting formatting (eText textType)
    ENumber var nform ->
      (:[]) <$> withFormatting formatting (eNumber var nform)
    EGroup isMacro els ->
      (:[]) <$> eGroup isMacro formatting els
    EChoose chooseParts -> eChoose chooseParts
    ELabel var termform pluralize ->
      (:[]) <$> eLabel var termform pluralize formatting
    EDate var dateType mbShowDateParts dps ->
      (:[]) <$> eDate var dateType mbShowDateParts dps formatting
    ENames vars namesFormat subst ->
      (:[]) <$> eNames vars namesFormat subst formatting

withFormatting :: CiteprocOutput a
               => Formatting -> Eval a (Output a) -> Eval a (Output a)
withFormatting (Formatting Nothing Nothing Nothing Nothing Nothing Nothing
                           Nothing Nothing Nothing Nothing Nothing
                           False False False) p
                          = p
withFormatting formatting p = do
  -- Title case conversion only affects English-language items.
  lang <- asks (localeLanguage . contextLocale)
  ref <- gets stateReference
  let reflang = case M.lookup "language" (referenceVariables ref) of
                  Just (TextVal t)  ->
                    either (const Nothing) Just $ parseLang t
                  Just (FancyVal x) ->
                    either (const Nothing) Just $ parseLang $ toText x
                  _                 -> Nothing
  let mainLangIsEn Nothing = False
      mainLangIsEn (Just l) = langLanguage l == "en"
  let isEnglish = case reflang of
                    Just l  -> mainLangIsEn (Just l)
                    Nothing -> mainLangIsEn lang
  let formatting' = if formatTextCase formatting == Just TitleCase &&
                       not isEnglish
                       then formatting{ formatTextCase = Nothing }
                       else formatting
  res <- p
  return $ formatted formatting' [res]

lookupTerm :: Term -> Eval a [(Term, Text)]
lookupTerm term = do
  terms <- asks (localeTerms . contextLocale)
  case M.lookup (termName term) terms of
     Just ts -> return $ [(term',t)
                         | (term',t) <- ts
                         , term <= term'
                         ]
     Nothing -> return []

lookupTerm' :: CiteprocOutput a => Term -> Eval a (Output a)
lookupTerm' term = lookupTerm term >>= f
 where
   f []  =
     --  “verb-short” first falls back to “verb”, “symbol”
     --  first falls back to “short”, and “verb” and “short”
     --  both fall back to “long”.
     case termForm term of
       VerbShort -> lookupTerm' term{ termForm = Verb }
       Symbol    -> lookupTerm' term{ termForm = Short }
       Verb      -> lookupTerm' term{ termForm = Long }
       Short     -> lookupTerm' term{ termForm = Long }
       _         -> return NullOutput
   f xs  = case xs of
             []        -> return NullOutput
             ((_,t):_) -> return $
                            if T.null t
                               then NullOutput
                               else Literal $ fromText t

pageRange :: CiteprocOutput a => Text -> Eval a (Output a)
pageRange x = do
  pageDelim <- lookupTerm'
                  emptyTerm{ termName = "page-range-delimiter" }
  mbPageRangeFormat <- asks (stylePageRangeFormat . contextStyleOptions)
  let ranges = map T.strip $ T.groupBy
               (\c d -> not (c == ',' || c == '&' || d == ',' || d == '&'))
               x
  return $ formatted mempty{ formatDelimiter = Just " " }
         $ map (formatPageRange mbPageRangeFormat
            (case pageDelim of
               NullOutput -> literal $ T.singleton enDash
               delim      -> delim)) ranges

enDash :: Char
enDash = '\x2013'

formatPageRange :: CiteprocOutput a
                => Maybe PageRangeFormat
                -> Output a
                -> Text
                -> Output a
formatPageRange _ _ "&" = literal "&"
formatPageRange _ _ "," = literal ","
formatPageRange mbPageRangeFormat delim t =
  let isDash '-' = True
      isDash '\x2013' = True
      isDash _ = False
      rangeParts = if "\\-" `T.isInfixOf` t
                      then [T.replace "\\-" "-" t]
                      else map T.strip $ T.split isDash t
      inRange pref xs
        | T.null pref = grouped (intersperse delim (map literal xs))
        | otherwise = grouped
            (literal pref : intersperse delim (map literal xs))
      changedDigits xs ys =
        length $ filter not $ zipWith (==) (xs ++ repeat ' ') ys
      minimal threshold pref x y =
        case T.commonPrefixes x y of
             Just (_comm, _erstx, resty) ->
                 if T.length resty < threshold && T.length y >= threshold
                    then inRange pref [x, T.takeEnd threshold y]
                    else inRange pref [x, resty]
             Nothing -> inRange pref [x, y]
   in case rangeParts of
        []     -> NullOutput
        [w]    -> literal w
        [w,v]
          | Nothing <- mbPageRangeFormat -> inRange mempty [w,v]
          | Just fmt <- mbPageRangeFormat -> do
            let wPrefix = T.dropWhileEnd isDigit w
            let vPrefix = T.dropWhileEnd isDigit v
            if wPrefix == vPrefix
               then do
                 let pref = wPrefix
                 let x = T.drop (T.length wPrefix) w
                 let y = T.drop (T.length vPrefix) v
                 let xlen = T.length x
                 let ylen = T.length y
                 let y'   = if ylen < xlen
                               then T.take (xlen - ylen) x <> y
                               else y
                 case fmt of
                   PageRangeChicago
                       | xlen < 3  -> inRange pref [x, y']
                       | "00" `T.isSuffixOf` x -> inRange pref [x, y']
                       | T.take 1 (T.takeEnd 2 x) == "0"
                         -> minimal 1 pref x y'
                       | xlen == 4
                       , changedDigits (T.unpack x) (T.unpack y') >= 3
                         -> inRange pref [x, y']
                       | otherwise -> minimal 2 pref x y'
                   PageRangeExpanded ->
                       inRange mempty [pref <> x, pref <> y']
                   PageRangeMinimal -> minimal 1 pref x y'
                   PageRangeMinimalTwo -> minimal 2 pref x y'
               else inRange mempty [w,v]
        _ -> literal t

eText :: CiteprocOutput a => TextType -> Eval a (Output a)
eText (TextVariable varForm v) = do
  ref <- gets stateReference
  -- Note: we do book keeping on how many variables
  -- have been accessed and how many are nonempty,
  -- in order to properly handle the group element,
  -- which is implicitly conditional.
  case v of
    "id"   -> do
      updateVarCount 1 1
      return $ Literal $ fromText $ coerce $ referenceId ref
    "type" -> do
      updateVarCount 1 1
      return $ Literal $ fromText  $ referenceType ref
    "locator" -> do
        let handleAmpersands (Just t) | T.any (=='&') t = do
              ts <- lookupTerm emptyTerm { termName = "and"
                                         , termForm = Symbol }
              case ts of
                (_,x):_ -> return (Just $ T.replace "&" x t)
                []      -> return (Just t)
            handleAmpersands x = return x

        mbv <- asks contextLocator >>= handleAmpersands
        mbl <- asks contextLabel
        case mbv of
          Just x | isNothing mbl || mbl == Just "page" -> do
                      updateVarCount 1 1
                      Tagged TagLocator <$> pageRange x
                 | otherwise -> do
                      updateVarCount 1 1
                      return $ Tagged TagLocator $
                                formatPageRange Nothing
                                (literal $ T.singleton enDash) x
          Nothing -> NullOutput <$ updateVarCount 1 0

    "year-suffix" -> do
        disamb <- gets (referenceDisambiguation . stateReference)
        case disamb >>= disambYearSuffix of
          Just x ->
            -- we don't update var count here; this doesn't
            -- count as a variable
            return $ Tagged (TagYearSuffix x)
                            (Literal (fromText (showYearSuffix x)))
          Nothing -> return NullOutput

    "citation-number" -> do
        mbv <- askVariable v
        case mbv of
          Just (NumVal x)  -> return $
                              Tagged (TagCitationNumber x) $
                              Literal $ fromText (T.pack (show x))
          _ -> do
            warn $ "citation-number not defined for " <>
                      coerce (referenceId ref)
            return NullOutput

    "citation-label" -> do  -- these need year suffix too
        mbv <- askVariable v
        mbsuff <- getYearSuffix
        case mbv of
          Just (TextVal t)  -> return $
                                Tagged TagCitationLabel $
                                  grouped $
                                  Literal (fromText t)
                                  : maybeToList mbsuff
          Just (FancyVal x) -> return $
                                 Tagged TagCitationLabel $
                                  grouped $
                                  Literal x
                                  : maybeToList mbsuff
          _ -> do
            warn $ "citation-label of unknown type for " <>
                      coerce (referenceId ref)
            return NullOutput

    "DOI"   -> handleIdent fixShortDOI IdentDOI
    "PMCID" -> handleIdent id IdentPMCID
    "PMID"  -> handleIdent id IdentPMID
    "URL"   -> handleIdent id IdentURL

    _ -> do
        mbv <- if varForm == ShortForm
                  then do
                    mbval <- (<|>) <$> askVariable (v <> "-short")
                                   <*> askVariable v
                    case mbval of
                      Nothing -> return Nothing
                      Just val -> do
                        mbAbbrevs <- asks contextAbbreviations
                        return $ Just $ fromMaybe val
                               $ mbAbbrevs >>= lookupAbbreviation v val
                  else askVariable v
        res <- case mbv of
                 Just (TextVal x)
                   | v == "page" -> pageRange x
                   | otherwise   -> return $ Literal $ fromText x
                 Just (FancyVal x)
                   | v == "page" -> pageRange (toText x)
                   | otherwise   -> return $ Literal x
                 Just (NumVal x) -> return $ Literal
                                           $ fromText (T.pack (show x))
                 _ -> return NullOutput
        handleSubst
        if v == "title" && res /= NullOutput
            then do
              modify (\st -> st { stateUsedTitle = True })
              -- tag title so we can hyperlink it later
              return $ Tagged TagTitle res
            else return res
    where
      handleIdent :: CiteprocOutput b => (Text -> Text) -> (Text -> Identifier) -> Eval b (Output b)
      handleIdent f identConstr = do
        mbv <- askVariable v
        handleSubst
        case f <$> (valToText =<< mbv) of
          Nothing -> return NullOutput
          Just t  -> do
            -- create link and remember that we've done so far
            modify (\st -> st { stateUsedIdentifier = True })
            let url = identifierToURL (identConstr t)
            return $ Linked url [Literal $ fromText t]
      handleSubst :: Eval a ()
      handleSubst = do inSubst <- asks contextInSubstitute
                       when inSubst $
                         modify $ \st -> -- delete variable so it isn't used again...
                           st{ stateReference =
                                 let Reference id' type' d' m' = stateReference st
                                  in Reference id' type' d' (M.delete v m') }
eText (TextMacro name) = do
  warn $ "encountered unexpanded macro " <> name
  return NullOutput
eText (TextValue t) = return $ Literal $ fromText t
eText (TextTerm term) = do
  t' <- lookupTerm' term
  t'' <- if termName term == "no date"
            then do
              mbsuff <- getYearSuffix
              case mbsuff of
                Nothing  -> return t'
                Just suff
                  | termForm term == Long
                    -> return $ grouped [t', Literal (fromText " "), suff]
                  | otherwise -> return $ grouped [t', suff]
            else return t'
  return $ Tagged (TagTerm term) t''


-- Numbers with prefixes or suffixes are never ordinalized
-- or rendered in roman numerals (e.g. “2E” remains “2E).
-- Numbers without affixes are individually transformed
-- (“2, 3” can become “2nd, 3rd”, “second, third” or “ii, iii”).
-- So, first we split on punctuation and spaces:
splitNums :: Text -> [Val a]
splitNums = map go . T.groupBy sameClass
 where
  go t = case readAsInt t of
           Just i  -> NumVal i
           Nothing -> TextVal $ if t == "-"
                                   then T.singleton enDash
                                   else t
  sameClass c d = (isSepPunct c || isSpace c) ==
                  (isSepPunct d || isSpace d)

--- punctuation that separates pages in a range
isSepPunct :: Char -> Bool
isSepPunct ',' = True
isSepPunct ';' = True
isSepPunct '-' = True
isSepPunct '\x2013' = True
isSepPunct _   = False

eLabel :: CiteprocOutput a
       => Variable
       -> TermForm
       -> Pluralize
       -> Formatting
       -> Eval a (Output a)
eLabel var termform pluralize formatting = do
  ref <- gets stateReference
  let getTerm :: CiteprocOutput a
              => Text -> Val a -> Eval a (Output a)
      getTerm termname x = do
        let determinePlural t
             | var == "number-of-volumes"
             , t /= "1" && t /= "0"      = Plural
             | "\\-" `T.isInfixOf` t     = Singular
             | length (splitNums t) > 1  = Plural
               -- see label_CollapsedPageNumberPluralDetection.txt
             | otherwise                 = Singular
        let number = case pluralize of
                         AlwaysPluralize     -> Plural
                         NeverPluralize      -> Singular
                         ContextualPluralize ->
                          case x of
                            TextVal t   -> determinePlural t
                            FancyVal w  -> determinePlural (toText w)
                            NamesVal ns -> if length ns > 1
                                              then Plural
                                              else Singular
                            _ -> Singular
        let term = emptyTerm{ termName = termname
                            , termForm = termform
                            , termNumber = Just number }
        lookupTerm' term
  locator <- asks contextLocator
  label <- asks contextLabel
  let var' = if var == "editortranslator" then "editor" else var
  term' <- case (var, locator, label) of
             ("locator", Just loc, Just lab) -> getTerm lab (TextVal loc)
             ("locator", Just loc, Nothing)
                | beginsWithSpace loc -> return NullOutput
                | ". " `T.isPrefixOf` T.dropWhile isLetter loc
                                         -> return NullOutput
                | otherwise              -> getTerm "page" (TextVal loc)
             ("page", Just loc, _) ->
               getTerm "page" (TextVal loc)
             _ -> case lookupVariable var' ref of
                         Nothing -> return NullOutput
                         Just x  -> getTerm (fromVariable var) x

  return $
    case formatSuffix formatting of
      Just suff
        | "." `T.isPrefixOf` suff
          -> case term' of
               Literal x
                 | "." `T.isSuffixOf` toText x
                 , not (formatStripPeriods formatting)
                 -> formatted
                     formatting{ formatSuffix =
                        if T.length suff <= 1
                           then Nothing
                           else Just (T.drop 1 suff) }
                     [term']
               _ -> formatted formatting [term']
      _ -> formatted formatting [term']

eDate :: CiteprocOutput a
       => Variable
       -> DateType
       -> Maybe ShowDateParts
       -> [DP]
       -> Formatting
       -> Eval a (Output a)
eDate var dateType mbShowDateParts dps formatting
  | var == mempty = do
    warn "skipping date element with no variable attribute set"
    return NullOutput
  | otherwise = do
    datevar <- askVariable var
    localeDateElt <- M.lookup dateType <$> asks (localeDate . contextLocale)
    let addOverride newdps olddp accum =
          case find ((== dpName olddp) . dpName) newdps of
            Just x  -> x{ dpFormatting =
                            dpFormatting olddp <> dpFormatting x } : accum
            Nothing -> olddp : accum
    let useDatePart dp =
          case mbShowDateParts of
            Just Year      -> dpName dp == DPYear
            Just YearMonth -> dpName dp == DPYear || dpName dp == DPMonth
            _              -> True
    let (dps', formatting') =
          case localeDateElt of
            Just (Element (EDate _ _ _ edps) f)
              -> (filter useDatePart $ foldr (addOverride dps) [] edps,
                   formatting <> f)
            _ -> (filter useDatePart dps, formatting)
    case datevar of
      Nothing ->
        -- warn $ "date element for empty variable " <> var
        return NullOutput
      Just (DateVal date) ->
        case dateLiteral date of
          Just t -> return $ formatted formatting' [Literal $ fromText t]
          Nothing -> do
            let dateparts = case dateSeason date of
                              Just i  ->
                                case dateParts date of
                                  [DateParts [y]] ->
                                    [DateParts [y, 12 + i]] -- pseudo-mo
                                  xs    -> xs
                              Nothing -> dateParts date
            xs <- formatDateParts dps'
                    $ case dateparts of
                        [] -> (DateParts [], Nothing)
                        [d] -> (d, Nothing)
                        (d:e:_) -> (d, Just e)
            when (all (== NullOutput) xs) $
              -- back off on the claim to nonemptiness
              -- when the only match are in date parts that
              -- we aren't printing; see
              -- group_SuppressTermWhenNoOutputFromPartialDate.txt
              updateVarCount 0 (-1)

            yearSuffix <- getYearSuffix
            return $ Tagged (TagDate date) $ formatted formatting'
                      (xs ++ maybeToList yearSuffix)
      Just _ -> do
        warn $ "date element for variable with non-date value " <>
                fromVariable var
        return NullOutput


getYearSuffix :: CiteprocOutput a => Eval a (Maybe (Output a))
getYearSuffix = do
  disamb <- gets (referenceDisambiguation . stateReference)
  sopts <- asks contextStyleOptions
  -- we only want year suffix on first occurence of year
  -- in a reference:
  usedYearSuffix <- gets stateUsedYearSuffix
  case disamb >>= disambYearSuffix of
    Just c
      | not (styleUsesYearSuffixVariable sopts)
      , not usedYearSuffix
      -> do
        modify $ \st -> st{ stateUsedYearSuffix = True }
        return $ Just $ Tagged (TagYearSuffix c)
                         (Literal (fromText (showYearSuffix c)))
      | otherwise -> return Nothing
    Nothing  -> return Nothing



formatDateParts :: CiteprocOutput a
          => [DP] -> (DateParts, Maybe DateParts) -> Eval a [Output a]
formatDateParts dpSpecs (date, mbNextDate) = do
  let (yr,mo,da) = bindDateParts date
  case mbNextDate of
    Nothing -> mapM (eDP (yr,mo,da)) dpSpecs
    Just nextDate -> do
      let (nextyr,nextmo,nextda) = bindDateParts nextDate
      let isOpenRange = nextyr == Just 0 &&
                        isNothing nextmo &&
                        isNothing nextda
      -- figure out where the range goes:
      -- first to differ out of the items selected by dpSpecs, in order y->m->d
      let dpToNs DPYear  = (yr, nextyr)
          dpToNs DPMonth = (mo, nextmo)
          dpToNs DPDay   = (da, nextda)
      let areSame = takeWhile (uncurry (==) . dpToNs) $
                      sort $ map dpName dpSpecs
      let (sames1, rest) = span (\dp -> dpName dp `elem` areSame) dpSpecs
      let (diffs, sames2) = span (\dp -> dpName dp `notElem` areSame) rest
      let cleanup = filter (/= NullOutput)
      sames1' <- cleanup <$> mapM (eDP (yr,mo,da)) sames1
      diffsLeft' <- cleanup <$> mapM (eDP (yr,mo,da)) diffs
      diffsRight' <- cleanup <$> mapM (eDP (nextyr,nextmo,nextda)) diffs
      sames2' <- cleanup <$> mapM (eDP (yr,mo,da)) sames2
      let rangeDelim = case sortOn dpName diffs of
                              []     -> Nothing
                              (dp:_) -> Just $ dpRangeDelimiter dp
      let toRange xs ys =
            case lastMay xs of
              Just xlast ->
                   initSafe xs ++
                     [Formatted mempty{ formatDelimiter = rangeDelim }
                     [xlast, headDef (Literal mempty) ys]] ++
                   tailSafe ys
              _ -> xs ++ ys

      return $
        if isOpenRange
           then [Formatted mempty{ formatSuffix = rangeDelim }
                    (removeLastSuffix $ sames1' ++ diffsLeft')]
           else removeLastSuffix $
                 sames1' ++
                 toRange (removeLastSuffix diffsLeft')
                         (removeFirstPrefix diffsRight') ++
                 sames2'

removeFirstPrefix :: [Output a] -> [Output a]
removeFirstPrefix (Formatted f xs : rest) =
  Formatted f{ formatPrefix = Nothing } xs : rest
removeFirstPrefix xs = xs

removeLastSuffix :: [Output a] -> [Output a]
removeLastSuffix [] = []
removeLastSuffix [Formatted f xs] =
  [Formatted f{ formatSuffix = Nothing } xs ]
removeLastSuffix (x:xs) = x : removeLastSuffix xs

eDP :: CiteprocOutput a
    => (Maybe Int, Maybe Int, Maybe Int) ->  DP -> Eval a (Output a)
eDP (yr,mo,da) dp = do
  let mbn = case dpName dp of
               DPDay   -> da
               DPMonth -> mo
               DPYear  -> yr
  case mbn of
    Nothing -> return NullOutput
    Just 0 | dpName dp == DPYear
            -> return $ Literal mempty -- open date range
    Just n  -> do
      let litStr = return . Literal . fromText . T.pack
      suffix <- case dpName dp of
                  DPYear
                    | n < 0
                      -> (:[]) <$> lookupTerm' emptyTerm{ termName = "bc" }
                    | n > 0
                    , n < 1000
                      -> (:[]) <$> lookupTerm' emptyTerm{ termName = "ad" }
                    | otherwise -> return []
                  _ -> return []
      let n' = case dpName dp of
                 DPYear -> abs n
                 _      -> n
      formatted (dpFormatting dp) . (:suffix) <$>
          case dpForm dp of
            DPNumeric             -> litStr (show n')
            DPNumericLeadingZeros -> litStr (printf "%02d" n')
            DPOrdinal             -> do
              locale <- asks contextLocale
              if localeLimitDayOrdinalsToDay1 locale == Just True && n' /= 1
                 then litStr (show n')
                 else evalNumber NumberOrdinal Nothing (NumVal n')
            form -> do
              let termForMonth s = emptyTerm{ termName = T.pack s
                                            , termForm = if form == DPShort
                                                            then Short
                                                            else Long }

              case dpName dp of
                DPMonth | n <= 0 -> return NullOutput
                        | n <= 12 ->
                  lookupTerm' $ termForMonth (printf "month-%02d" n)
                        | n <= 16 -> -- season pseudo-month
                  lookupTerm' $ termForMonth (printf "season-%02d" (n - 12))
                        | n <= 20 -> -- season pseudo-month
                  lookupTerm' $ termForMonth (printf "season-%02d" (n - 16))
                        | otherwise -> -- season pseudo-month
                  lookupTerm' $ termForMonth (printf "season-%02d" (n - 20))
                _                 -> litStr (show n')


bindDateParts :: DateParts -> (Maybe Int, Maybe Int, Maybe Int)
bindDateParts date =
      case date of
        DateParts (y:m:d:_) -> (Just y,Just m,Just d)
        DateParts [y,m]     -> (Just y,Just m,Nothing)
        DateParts [y]       -> (Just y,Nothing,Nothing)
        _                   -> (Nothing,Nothing,Nothing)

eNames :: CiteprocOutput a
        => [Variable]
        -> NamesFormat
        -> [Element a]
        -> Formatting
        -> Eval a (Output a)
eNames vars namesFormat' subst formatting = do
  substituteNamesForm <- asks contextSubstituteNamesForm
  inSortKey <- asks contextInSortKey
  let namesFormat =
        case substituteNamesForm of
          Nothing -> namesFormat'
          Just subs ->
            NamesFormat
            { namesLabel           =
                if inSortKey -- see test/csl/sort_DropNameLabelInSort.txt
                   -- though this doesn't seem to be in the spec
                   then Nothing
                   else namesLabel namesFormat' <|> namesLabel subs
            , namesEtAl            = namesEtAl namesFormat' <|>
                                       namesEtAl subs
            , namesName            = namesName namesFormat' <|>
                                       namesName subs
            , namesLabelBeforeName =
                if isJust (namesName namesFormat') &&
                   isJust (namesLabel namesFormat')
                   then namesLabelBeforeName namesFormat'
                   else namesLabelBeforeName subs
            }

  vars' <- if "editor" `elem` vars && "translator" `elem` vars
              then do
                ed <- askVariable "editor"
                tr <- askVariable "translator"
                let termform =
                      case namesLabel namesFormat of
                        Just (termform', _, _) -> termform'
                        _ -> Long
                mbterm <- lookupTerm'
                            emptyTerm{ termName = "editortranslator"
                                     , termForm = termform }
                if ed == tr && mbterm /= NullOutput
                   then return $ "editortranslator" :
                        [v | v <- vars
                           , v /= "editor"
                           , v /= "translator"]
                   else return vars
              else return vars
  inSubstitute <- asks contextInSubstitute
  let (nameFormat, nameFormatting') =
        fromMaybe (defaultNameFormat, mempty) (namesName namesFormat)
  let nameFormatting = nameFormatting' <>
                       formatting{ formatPrefix = Nothing
                                 , formatSuffix = Nothing
                                 , formatDelimiter = Nothing }
  rawContribs <- mapM (\var -> (var,) <$>
                       askVariable
                       (if var == "editortranslator"
                           then "editor"
                           else var)) vars'
  if all (isNothing . snd) rawContribs
     then
       case subst of
         els@(_:_) | not inSubstitute -> do
           res <- withRWST
                  (\ctx st -> (ctx{ contextInSubstitute = True
                                  , contextSubstituteNamesForm =
                                      Just namesFormat },
                               st)) $ eSubstitute els
           return $
             case res of
               (Tagged TagNames{} _:_) -> formatted formatting res
               -- important to have title (or whatever) tagged as
               -- substituting for Names, for purposes of
               -- disambiguation:
               _ -> formatted formatting
                    [Tagged (TagNames "" namesFormat []) $ grouped res]
         _ -> return NullOutput
     else do
        xs <- mapM (formatNames namesFormat nameFormat nameFormatting)
               rawContribs
        when inSubstitute $
          modify $ \st ->  -- delete variables so they aren't used again...
              st{ stateReference =
                  let Reference id' type' d' m' = stateReference st
                   in Reference id' type' d' (foldr M.delete m'
                                         [v | (v, Just _) <- rawContribs ])}

        return $
          case nameForm nameFormat of
             CountName -> Literal $ fromText $ T.pack $ show $ length
               [name
                 | Tagged (TagName name) _ <- concatMap universe xs]
             _ -> formatted mempty{ formatPrefix = formatPrefix formatting
                                  , formatSuffix = formatSuffix formatting
                                  , formatDelimiter =
                                    formatDelimiter formatting } xs

eSubstitute :: CiteprocOutput a
            => [Element a]
            -> Eval a [Output a]
eSubstitute els =
  case els of
    [] -> return []
    (e:es) -> do
      res <- eElement e
      case filter (/= NullOutput) res of
        [] -> eSubstitute es
        xs -> return xs

formatNames :: CiteprocOutput a
            => NamesFormat
            -> NameFormat
            -> Formatting
            -> (Variable, Maybe (Val a))
            -> Eval a (Output a)
formatNames namesFormat nameFormat formatting (var, Just (NamesVal names)) =
  do
  isSubsequent <- (Subsequent `elem`) <$> asks contextPosition
  isInBibliography <- asks contextInBibliography
  let (etAlMin, etAlUseFirst) =
        if not isInBibliography && isSubsequent
           then (nameEtAlSubsequentMin nameFormat <|> nameEtAlMin nameFormat,
                 nameEtAlSubsequentUseFirst nameFormat <|>
                    nameEtAlUseFirst nameFormat)
           else (nameEtAlMin nameFormat, nameEtAlUseFirst nameFormat)
  inSortKey <- asks contextInSortKey
  disamb <- gets (referenceDisambiguation . stateReference)
  names' <- zipWithM (formatName nameFormat formatting) [1..] names
  let delim' = fromMaybe (nameDelimiter nameFormat) $
                 formatDelimiter formatting
  let delim = case (beginsWithSpace <$> formatSuffix formatting,
                    endsWithSpace <$> formatPrefix formatting) of
                    (Just True, Just True) -> T.strip delim'
                    (Just True, _)         -> T.stripStart delim'
                    (_, Just True)         -> T.stripEnd delim'
                    _                      -> delim'
  let numnames = length names'
  label <- case namesLabel namesFormat of
             Just (termform, pluralize, lf) | not inSortKey ->
               (:[]) <$> eLabel var termform pluralize lf
             _ -> return []
  mbAndTerm <- case nameAndStyle nameFormat of
                  Just Symbol -> do
                    ts <- lookupTerm emptyTerm { termName = "and"
                                               , termForm = Symbol }
                    case ts of
                      (_,x):_ -> return $ Just x
                      []      -> return $ Just "&"
                  Just _ -> fmap snd . listToMaybe <$>
                              lookupTerm emptyTerm { termName = "and"
                                                   , termForm = Long }
                  Nothing -> return Nothing
  let finalNameIsOthers = (lastMay names >>= nameLiteral) == Just "others"
        -- bibtex conversions often have this, and we want to render it "et al"
  let etAlUseLast = nameEtAlUseLast nameFormat
  let etAlThreshold = case etAlMin of
                        Just x | numnames >= x
                          -> case (disamb >>= disambEtAlNames, etAlUseFirst) of
                               (Just n, Just m) -> Just (max m n)
                               (_, y) -> y
                               | numnames < x
                               , finalNameIsOthers -> Just (numnames - 1)
                        _ -> Nothing
  let beforeLastDelim =
        case mbAndTerm of
          Nothing -> delim
          Just _ ->
             case nameDelimiterPrecedesLast nameFormat of
                PrecedesContextual
                  | numnames > 2          -> delim
                  | otherwise             -> ""
                PrecedesAfterInvertedName
                  -> case nameAsSortOrder nameFormat of
                       Just NameAsSortOrderAll -> delim
                       Just NameAsSortOrderFirst
                         | numnames < 3        -> delim
                       _                       -> ""
                PrecedesAlways            -> delim
                PrecedesNever             -> ""
  let andPreSpace = case beforeLastDelim of
        "" -> case formatSuffix formatting of
                Just t | endsWithSpace t -> ""
                _ -> " "
        t | endsWithSpace t -> ""
        _  -> " "
  let andPostSpace = case formatPrefix formatting of
                       Just t | beginsWithSpace t -> ""
                       _ -> " "
  let mbAndDelim = case mbAndTerm of
                         Nothing -> Nothing
                         Just t  -> Just (andPreSpace <> t <> andPostSpace)
  let etAlPreSpace = case formatSuffix formatting of
                       Just t | endsWithSpace t -> ""
                       _ -> " "
  let beforeEtAl =
        case nameDelimiterPrecedesEtAl nameFormat of
            PrecedesContextual
              | numnames > 2
              , etAlThreshold > Just 1 -> delim
              | otherwise              -> etAlPreSpace
            PrecedesAfterInvertedName
                  -> case nameAsSortOrder nameFormat of
                       Just NameAsSortOrderAll  -> delim
                       Just NameAsSortOrderFirst
                         | etAlThreshold < Just 2 -> delim
                       _                          -> etAlPreSpace
            PrecedesAlways            -> delim
            PrecedesNever             -> etAlPreSpace
  etAl <- case namesEtAl namesFormat of
                Just (term, f) -> withFormatting f{
                    formatPrefix = removeDoubleSpaces <$>
                      Just beforeEtAl <> formatPrefix f } $
                 lookupTerm' emptyTerm{ termName = term }
                Nothing
                  | etAlUseLast ->
                    return $
                      Formatted mempty{ formatPrefix = Just beforeEtAl }
                        [literal "\x2026 "] -- ellipses
                  | otherwise   ->
                      Formatted mempty{ formatPrefix = Just beforeEtAl }
                      . (:[]) <$> lookupTerm' emptyTerm{ termName = "et-al" }
  let addNameAndDelim name idx
       | etAlThreshold == Just 0 = NullOutput
       | idx == 1    = name
       | idx == numnames
       , etAlUseLast
       , maybe False (idx - 1 >=) etAlThreshold
         = name
       | maybe False (idx - 1 >) etAlThreshold = NullOutput
       | maybe False (idx - 1 ==) etAlThreshold =
         if inSortKey
            then NullOutput
            else etAl
       | inSortKey = name
       | idx == numnames
         = formatted mempty{ formatPrefix =
                       Just (beforeLastDelim <> fromMaybe "" mbAndDelim) }
            [name]
       | otherwise = formatted mempty{ formatPrefix = Just delim } [name]
  let names'' = zipWith addNameAndDelim names' [1..]
  -- we set delimiter to Nothing because we're handling delim
  -- manually, to allow for things like "and" and no final comma
  return $ Tagged (TagNames var namesFormat names)
         $ grouped $
           if namesLabelBeforeName namesFormat
              then label ++ names''
              else names'' ++ label

formatNames _ _ _ (var, Just _) = do
  warn $ "ignoring non-name value for variable " <> fromVariable var
  return NullOutput
formatNames _ _ _ (_, Nothing) = return NullOutput

formatName :: CiteprocOutput a
           => NameFormat -> Formatting -> Int -> Name -> Eval a (Output a)
formatName nameFormat formatting order name = do
  disamb <- gets (referenceDisambiguation . stateReference)
  let nameFormat' =
        case M.lookup name . disambNameMap =<< disamb of
          Nothing -> nameFormat
          Just AddInitials
            -> nameFormat{ nameForm = LongName }
          Just AddInitialsIfPrimary
            | order == 1  -> nameFormat{ nameForm = LongName }
            | otherwise -> nameFormat
          Just AddGivenName ->
            nameFormat{ nameForm = LongName
                      , nameInitialize = False
                      }
          Just AddGivenNameIfPrimary
            | order == 1 ->
               nameFormat{ nameForm = LongName
                         , nameInitialize = False
                         }
            | otherwise -> nameFormat
  Tagged (TagName name) <$>
    case nameLiteral name of
      Just t  -> return $ formatted formatting
                        $ maybe [literal t]
                          (\f -> [Formatted f [literal t]])
                          (nameFamilyFormatting nameFormat)
      Nothing -> getDisplayName nameFormat' formatting order name


getNamePartSortOrder :: Name -> Eval a [Text]
getNamePartSortOrder name = do
  demoteNonDroppingParticle <-
    asks (styleDemoteNonDroppingParticle . contextStyleOptions)
  map (fromMaybe mempty) <$>
    case nameLiteral name of
      Nothing
        | isByzantineName name
           -> return $
                   case demoteNonDroppingParticle of
                     DemoteNever ->
                           [nameNonDroppingParticle name <> nameFamily name,
                            nameDroppingParticle name,
                            nameGiven name,
                            nameSuffix name]
                     _ ->  [nameFamily name,
                            nameDroppingParticle name <>
                              nameNonDroppingParticle name,
                            nameGiven name,
                            nameSuffix name]
        | otherwise
           -> return [nameFamily name,
                      nameGiven name]
      Just n -> return [Just n]

literal :: CiteprocOutput a => Text -> Output a
literal = Literal . fromText

showYearSuffix :: Int -> Text
showYearSuffix x
  | x < 27    = T.singleton $ chr $ ord 'a' + (x - 1)
  | otherwise =
      let x' = x - 1
       in T.pack [chr (ord 'a' - 1 + (x' `div` 26)),
                  chr (ord 'a' + (x' `mod` 26))]

initialize :: Maybe Lang
           -> Bool       -- ^ initialize
           -> Bool       -- ^ with hyphen
           -> Text       -- ^ initialize with (suffix)
           -> Text
           -> Text
initialize mblang makeInitials useHyphen initializeWith =
   stripSpaces . T.replace " -" "-" . mconcat . map initializeWord . splitWords
  where
   stripSpaces = T.dropWhile (==' ') . T.dropWhileEnd (==' ') -- preserve nbsp
   -- Left values are already initials
   -- Right values are not
   splitWords =
     reverse . (\(ws,cs) ->
                  case cs of
                    [] -> ws
                    [d] -> Left (T.singleton d) : ws
                    _   -> Right (T.pack (reverse cs)) : ws) .
     T.foldl'
     (\(ws, cs) c ->
       case c of
         '.' | null cs   -> (ws, [])
             | otherwise -> (Left (T.pack (reverse cs)) : ws, [])
         '-' | null cs   -> (ws, ['-'])
             | otherwise -> (Right (T.pack (reverse cs)) : ws, ['-'])
         ' ' -> case cs of
                  []  -> (ws, cs)
                  [d] -> (Left (T.singleton d) : ws, [])
                  _   -> (Right (T.pack (reverse cs)) : ws, [])
         _   -> (ws, c:cs))
     ([], mempty)
   addSuffix t
     | T.null t  = mempty
     | otherwise = t <> initializeWith
   toInitial t =
       case T.uncons t of
         Just ('-', t') ->
           case T.uncons t' of
             Just (c, _)
               | isUpper c
               , useHyphen -> "-" <> Unicode.toUpper mblang (T.singleton c)
               | isUpper c -> Unicode.toUpper mblang (T.singleton c)
             _ -> mempty  -- e.g. Ji-ping -> J. not J.-p.
         Just (c, t')
           | isUpper c ->
             case T.uncons t' of
               Just (d, t'')
                 | isUpper d  -- see test/csl/name_LongAbbreviation.txt
                 , not (T.null t'')
                 , T.all isLower t''
                 -> T.singleton c <> T.toLower (T.singleton d)
               _ -> T.singleton c
         _ -> t
   initializeWord (Left t) -- Left values already initialized
     = addSuffix t
   initializeWord (Right t) -- Right values not already initialized
     | T.all isLower t = if endsWithSpace initializeWith
                            then t <> " "
                            else " " <> t <> " "
     | makeInitials    = (addSuffix . toInitial) t
     | otherwise       = t <> " "

getDisplayName :: CiteprocOutput a
               => NameFormat -> Formatting -> Int -> Name -> Eval a (Output a)
getDisplayName nameFormat formatting order name = do
  inSortKey <- asks contextInSortKey
  demoteNonDroppingParticle <-
    asks (styleDemoteNonDroppingParticle . contextStyleOptions)
  initializeWithHyphen <-
    asks (styleInitializeWithHyphen . contextStyleOptions)
  mblang <- asks (localeLanguage . contextLocale)
  let initialize' =
        case nameFamily name of
          Nothing -> id
          Just _ ->
            case nameInitializeWith nameFormat of
              Just initializeWith ->
                initialize
                mblang
                (nameInitialize nameFormat)
                initializeWithHyphen
                initializeWith
              Nothing -> id
  let separator = nameSortSeparator nameFormat
  let x <+> NullOutput = x
      NullOutput <+> x = x
      Literal x <+> y =
        case T.unsnoc (toText x) of
          Just (_, c) | c == '’' || c == '\'' || c == '-' || c == '\x2013' ||
                        c == '\xa0' ->
               formatted mempty [Literal x, y]
          _ | isByzantineName name ->
               formatted mempty{ formatDelimiter = Just " " } [Literal x, y]
            | otherwise -> formatted mempty [Literal x, y]
      Formatted f x <+> y =
        formatted mempty{ formatDelimiter =
                            case formatSuffix f of
                              Just t | endsWithSpace t -> Nothing
                              _ -> Just " " } [formatted f x, y]
      Linked i x <+> y =
        formatted mempty{ formatDelimiter = Just " " } [Linked i x, y]
      Tagged _ x <+> y = x <+> y
      InNote x <+> y = x <+> y
  let x <:> NullOutput = x
      NullOutput <:> x = x
      Literal x <:> y =
        formatted mempty{ formatDelimiter = Just separator } [Literal x, y]
      Formatted f x <:> y = formatted
        (mempty{ formatDelimiter = Just separator }) [Formatted f x, y]
      Linked i x <:> y = formatted
        (mempty{ formatDelimiter = Just separator }) [Linked i x, y]
      Tagged _ x <:> y = x <:> y
      InNote x <:> y = x <:> y

  let familyAffixes = formatted
          (case nameFamilyFormatting nameFormat of
             Nothing -> mempty
             Just f  -> mempty{ formatSuffix = formatSuffix f
                              , formatPrefix = formatPrefix f })
  let givenAffixes = formatted
          (case nameGivenFormatting nameFormat of
             Nothing -> mempty
             Just f  -> mempty{ formatSuffix = formatSuffix f
                              , formatPrefix = formatPrefix f })
  let familyFormatting = formatted
          (case nameFamilyFormatting nameFormat of
             Nothing -> mempty
             Just f  -> f{ formatSuffix = Nothing
                         , formatPrefix = Nothing })
  let givenFormatting = formatted
          (case nameGivenFormatting nameFormat of
             Nothing -> mempty
             Just f  -> f{ formatSuffix = Nothing
                         , formatPrefix = Nothing })
  let nonDroppingParticle =
        maybe NullOutput (familyFormatting . (:[]) . literal) $
          nameNonDroppingParticle name
  let droppingParticle =
        maybe NullOutput (givenFormatting . (:[]) . literal) $
          nameDroppingParticle name
  let given =
        maybe NullOutput (givenFormatting . (:[]) . literal . initialize') $
          nameGiven name
  let family =
        maybe NullOutput (familyFormatting . (:[]) . literal) $
          nameFamily name
  let suffix = maybe NullOutput literal $ nameSuffix name
  let useSortOrder = inSortKey ||
                     case nameAsSortOrder nameFormat of
                       Just NameAsSortOrderAll -> True
                       Just NameAsSortOrderFirst -> order == 1
                       _ -> False
  return $ formatted formatting . (:[]) $
    if isByzantineName name
       then
         case nameForm nameFormat of
              LongName
                | demoteNonDroppingParticle == DemoteNever ||
                  demoteNonDroppingParticle == DemoteSortOnly
                , useSortOrder->
                      familyAffixes
                      [ nonDroppingParticle <+>
                        family ] <:>
                      givenAffixes
                      [ given <+>
                        droppingParticle ] <:>
                      suffix
                | demoteNonDroppingParticle == DemoteDisplayAndSort
                , useSortOrder->
                      familyAffixes
                      [ family ] <:>
                      givenAffixes
                      [ given <+>
                        droppingParticle <+>
                        nonDroppingParticle ] <:>
                      suffix
                | nameCommaSuffix name ->
                      givenAffixes
                      [ given ] <+>
                      familyAffixes
                      [ droppingParticle <+>
                        nonDroppingParticle <+>
                        family <:>
                        suffix ]
                | otherwise ->
                      givenAffixes
                      [ given ] <+>
                      familyAffixes
                      [ droppingParticle <+>
                        nonDroppingParticle <+>
                        family <+>
                        suffix ]
              ShortName ->
                      familyAffixes
                      [ nonDroppingParticle <+>
                        family ]
              CountName -> NullOutput
       else
         case nameForm nameFormat of
              LongName  -> grouped
                [ familyAffixes
                  [ family ]
                , givenAffixes
                  [ given ] ]
              ShortName -> familyAffixes
                             [ family ]
              CountName -> NullOutput


eGroup :: CiteprocOutput a
          => Bool -> Formatting -> [Element a] -> Eval a (Output a)
eGroup isMacro formatting els = do
  -- A group is suppressed if it directly or indirectly
  -- calls at least one variable but all of the variables
  -- it calls are empty.
  VarCount oldVars oldNonempty <- gets stateVarCount
  xs <- mconcat <$> mapM eElement els
  VarCount newVars newNonempty <- gets stateVarCount
  -- see
  -- https://github.com/citation-style-language/documentation/blob/master/specification.rst#group
  -- "When a cs:group contains a child cs:macro, if the cs:macro is
  -- non-empty, it is treated as a non-empty variable for the purposes of
  -- determining suppression of the outer cs:group."
  when (isMacro && not (all (== NullOutput) xs)) $
    updateVarCount 1 1
  return $ if oldVars == newVars || newNonempty > oldNonempty
              then formatted formatting xs
              else NullOutput

eChoose :: CiteprocOutput a
        => [(Match, [Condition], [Element a])] -> Eval a [Output a]
eChoose [] = return []
eChoose ((match, conditions, els):rest) = do
  ref <- gets stateReference
  label <- asks contextLabel
  let disambiguate = maybe False
                      disambCondition (referenceDisambiguation ref)
  positions <- asks contextPosition
  hasLocator <- isJust <$> asks contextLocator
  let isNumeric t = all
        (\chunk -> T.any isDigit chunk && not (T.any isSpace chunk)) $
        T.split (\c -> c == ',' || c == '-' || c == '&')
         (T.replace ", " "," . T.replace "& " "&" . T.replace ", " "," $ t)
  let testCondition cond =
        case cond of
           HasVariable "locator" -> hasLocator
           HasVariable t ->
             case lookupVariable t ref of
               Just x  -> isNonEmpty x
               Nothing -> False
           HasType t -> lookupVariable "type" ref == Just (TextVal t)
           IsUncertainDate t -> case lookupVariable t ref of
                                  Just (DateVal d) -> dateCirca d
                                  _                -> False
           IsNumeric t -> case lookupVariable t ref of
                            Just (NumVal _)   -> True
                            Just (TextVal x)  -> isNumeric x
                            Just (FancyVal x) -> isNumeric (toText x)
                            _                 -> False
           HasLocatorType t -> case label of
                                 Just "sub verbo" -> t == "sub-verbo"
                                 Just x -> toVariable x == t
                                 Nothing -> t == "page"
           HasPosition pos -> pos `elem` positions
           WouldDisambiguate -> disambiguate
  let matched = (case match of
                   MatchAll  -> all testCondition
                   MatchAny  -> any testCondition
                   MatchNone -> not . any testCondition) conditions
  if matched
     then mconcat <$> mapM eElement els
     else eChoose rest


eNumber :: CiteprocOutput a => Variable -> NumberForm -> Eval a (Output a)
eNumber var nform = do
  mbv <- askVariable var
  varTerms <- lookupTerm emptyTerm { termName = fromVariable var }
  let mbGender = case varTerms of
                   [] -> Nothing
                   ((t,_):_) -> termGender t
  let nparts = case mbv of
                 Just x@NumVal{}   -> [x]
                 Just (FancyVal x) -> splitNums (toText x)
                 Just (TextVal t)  -> splitNums t
                 _                 -> []
  grouped <$> mapM (evalNumber nform mbGender) nparts

evalNumber :: CiteprocOutput a
           => NumberForm -> Maybe TermGender -> Val a -> Eval a (Output a)
evalNumber form mbGender (NumVal i) = do
  let numterm s x = emptyTerm { termName = T.pack $ printf s x
                              , termGenderForm = mbGender }
  let dectext = T.pack (show i)
  let twomatch = numterm "ordinal-%02d" (i `mod` 100)
  let onematch = numterm "ordinal-%02d" (i `mod` 10)
  let fallback = emptyTerm { termName = "ordinal" }
  case form of
    NumberNumeric -> return $ Literal $ fromText dectext
    NumberOrdinal -> do
      res <- (if i > 99
                 then filter (\(t,_) -> termMatch t /= Just WholeNumber)
                 else id) <$> lookupTerm twomatch
      case res of
        ((_,suff):_) ->
          return $ Literal $ fromText (dectext <> suff)
        [] -> do -- not an exact match
          res' <- (if i > 10
                      then filter (\(t,_) ->
                             isNothing (termMatch t) ||
                             termMatch t == Just LastDigit)
                      else id) <$> lookupTerm onematch
          case res' of
            ((_,suff):_) ->
              return $ Literal $ fromText (dectext <> suff)
            [] -> do
              res'' <- lookupTerm fallback
              case res'' of
                ((_,suff):_) ->
                  return $ Literal $ fromText (dectext <> suff)
                [] -> do
                  warn $ "no ordinal suffix found for " <> dectext
                  return $ Literal $ fromText (T.pack (show i))
    NumberLongOrdinal
      | i >= 1
      , i <= 10 -> do
        res <- lookupTerm (numterm "long-ordinal-%02d" i)
        case res of
          ((_,t):_) -> return $ Literal $ fromText t
          []        -> evalNumber NumberOrdinal mbGender (NumVal i)
      | otherwise -> evalNumber NumberOrdinal mbGender (NumVal i)
    NumberRoman -> return $ Literal $ fromText $ toRomanNumeral i
evalNumber _ _ (TextVal t) = return $ Literal $ fromText t
evalNumber _ _ (FancyVal t) = return $ Literal t
evalNumber _ _ _ = return NullOutput


warn :: Text -> Eval a ()
warn t = tell $ Set.singleton t

-- | Convert number < 4000 to lowercase roman numeral.
toRomanNumeral :: Int -> Text
toRomanNumeral x
  | x >= 4000 || x < 0 = T.pack (show x)
  | x >= 1000 = "m" <> toRomanNumeral (x - 1000)
  | x >= 900  = "cm" <> toRomanNumeral (x - 900)
  | x >= 500  = "d" <> toRomanNumeral (x - 500)
  | x >= 400  = "cd" <> toRomanNumeral (x - 400)
  | x >= 100  = "c" <> toRomanNumeral (x - 100)
  | x >= 90   = "xc" <> toRomanNumeral (x - 90)
  | x >= 50   = "l"  <> toRomanNumeral (x - 50)
  | x >= 40   = "xl" <> toRomanNumeral (x - 40)
  | x >= 10   = "x" <> toRomanNumeral (x - 10)
  | x == 9    = "ix"
  | x >= 5    = "v" <> toRomanNumeral (x - 5)
  | x == 4    = "iv"
  | x >= 1    = "i" <> toRomanNumeral (x - 1)
  | x == 0    = ""
  | otherwise = T.pack (show x)

-- Gets variable while updating var count.
askVariable :: CiteprocOutput a => Variable -> Eval a (Maybe (Val a))
askVariable "page-first" = do
  res <- askVariable "page"
  case res of
    Just (TextVal t)  ->
      return $ Just $ TextVal $ T.takeWhile (not . isSepPunct) t
    Just (FancyVal x) ->
      return $ Just $ TextVal $ T.takeWhile (not . isSepPunct) $ toText x
    Just (NumVal n)   -> return $ Just $ NumVal n
    _                 -> return Nothing
askVariable v = do
  ref <- gets stateReference
  case lookupVariable v ref of
    Just x | isNonEmpty x -> do
      updateVarCount 1 1
      return $ Just x
    _ -> do
      updateVarCount 1 0
      return Nothing

isNonEmpty :: CiteprocOutput a => Val a -> Bool
isNonEmpty (TextVal t) = not (T.null t)
isNonEmpty (FancyVal x) = x /= mempty
isNonEmpty (NamesVal []) = False
isNonEmpty (DateVal (Date [] _ Nothing Nothing)) = False
isNonEmpty _       = True

citationLabel :: Reference a -> Val a
citationLabel ref = TextVal trigraph
 where
  trigraph = namepart <> datepart
  datepart = case M.lookup "issued" varmap of
               Just (DateVal d) -> getYear d
               _ -> ""
  namepart = if "author" `elem` namevars
                then getNames "author"
                else case namevars of
                       (n:_) -> getNames n
                       _     -> "Xyz"
  varmap = referenceVariables ref
  vars = M.keys varmap
  namevars = [v | v <- vars, variableType v == NameVariable]
  getNames var = case M.lookup var varmap of
                   Just (NamesVal ns) ->
                     let x = case length ns of
                               1  -> 4
                               n | n >= 4 -> 1
                                 | otherwise -> 2
                     in mconcat $
                        map (T.take x . fromMaybe "" .  nameFamily)
                        (take 4 ns)
                   _ -> ""
  getYear d = case dateParts d of
                (DateParts (x:_):_) ->
                  T.pack (printf "%02d" $ x `mod` 100)
                _ -> ""

removeDoubleSpaces :: Text -> Text
removeDoubleSpaces = T.replace "  " " "

endsWithSpace :: Text -> Bool
endsWithSpace t = not (T.null t) && isSpace (T.last t)

beginsWithSpace :: Text -> Bool
beginsWithSpace t = not (T.null t) && isSpace (T.head t)

