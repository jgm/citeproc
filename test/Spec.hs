{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Citeproc
import Citeproc.CslJson
import Data.Algorithm.DiffContext
import System.TimeIt (timeIt)
import Control.Monad (unless)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Exit
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Text.PrettyPrint as Pretty
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl', isInfixOf, intersperse, sortOn, sort)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Char (isDigit, isLetter, toLower)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A
import Data.Aeson ((.:?), (.!=))
import Data.Text.Encoding (decodeUtf8)
import System.FilePath
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

data CiteprocTest a =
  CiteprocTest
  { name          :: Text
  , path          :: FilePath
  , category      :: Text
  , mode          :: Text
  , result        :: Text
  , csl           :: ByteString
  , input         :: [Reference a]
  , bibentries    :: Maybe A.Value
  , bibsection    :: Maybe A.Value
  , citeItems     :: Maybe [Citation a]
  , citations     :: Maybe [Citation a]
  , abbreviations :: Maybe Abbreviations
  , skipReason    :: Maybe Text
  , options       :: Maybe TestOptions
  } deriving (Show)

newtype TestOptions =
    TestOptions { testCiteprocOpts :: CiteprocOptions }
    deriving (Show, Eq)

defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions
    { testCiteprocOpts = defaultCiteprocOptions }

instance A.FromJSON TestOptions where
    parseJSON = A.withObject "TestOptions" $ fmap TestOptions . \v ->
        CiteprocOptions
        <$> v .:? "linkCitations"    .!= False
        <*> v .:? "linkBibliography" .!= False

data TestResult =
    Passed
  | Skipped Text
  | Failed Text Text
  | Errored CiteprocError
  deriving (Show, Eq)

runTest :: CiteprocTest (CslJson Text)
        -> StateT Counts IO TestResult
runTest test = do
  let opts = fromMaybe defaultTestOptions . options $ test
  let cites =
        case citations test of
          Just cs -> cs
          Nothing ->
            case citeItems test of
                Nothing
                  | mode test == "citation"
                    -> [referencesToCitation
                         (nubOrdOn referenceId (input test))]
                  | otherwise
                    -> map (referencesToCitation . (:[]))
                        (nubOrdOn referenceId (input test))
                Just cs -> cs
  let doError err = do
        modify $ \st -> st{ errored = category test : errored st }
        liftIO $ do
          TIO.putStrLn $ "[ERRORED]  " <> T.pack (path test)
          TIO.putStrLn $ T.pack $ show err
          TIO.putStrLn ""
        return $ Errored err
  let doSkip reason = do
        modify $ \st -> st{ skipped = category test : skipped st }
        liftIO $ do
          TIO.putStrLn $ "[SKIPPED]  " <> T.pack (path test)
          -- TIO.putStrLn $ T.strip reason
          TIO.putStrLn ""
        return $ Skipped reason
  case skipReason test of
    Just reason -> doSkip reason
    Nothing ->
      case parseStyle (const Nothing) (decodeUtf8 $ csl test) of
        Nothing -> doError $ CiteprocParseError
                      "Could not fetch independent parent"
        Just (Left err) -> doError err
        Just (Right style') -> do
            let style = style'{ styleAbbreviations = abbreviations test }
            let loc = mergeLocales Nothing style
            let actual = citeproc (testCiteprocOpts opts)
                           style Nothing (input test) cites
            unless (null (resultWarnings actual)) $ do
              liftIO $ do
                TIO.putStrLn $ "[WARNING]  " <> T.pack (path test)
                mapM_ (TIO.putStrLn . ("==> " <>))
                     $ resultWarnings actual
            case mode test of
              "citation" -> compareTest test
                  (T.intercalate "\n" $ map (renderCslJson' loc)
                                          (resultCitations actual))
              "bibliography" -> compareTest test
                  (T.intercalate "\n"
                    (addDivs $ map (renderCslJson' loc . snd)
                                          (resultBibliography actual)))
              _ -> doSkip $ "unknown mode " <> mode test

renderCslJson' :: Locale -> CslJson Text -> Text
renderCslJson' loc x =
  if T.null res
     then "[CSL STYLE ERROR: reference with no printed form.]"
     else res
 where
  res = renderCslJson True loc x

addDivs :: [Text] -> [Text]
addDivs ts = "<div class=\"csl-bib-body\">" : map addItemDiv ts ++ ["</div>"]
  where addItemDiv t
          | "<div" `T.isPrefixOf` t =
            "  <div class=\"csl-entry\">\n    " <> t <> "\n  </div>"
          | "</div>" `T.isSuffixOf` t =
            "  <div class=\"csl-entry\">" <> t <> "\n  </div>"
          | otherwise =
            "  <div class=\"csl-entry\">" <> t <> "</div>"

referencesToCitation :: [Reference a] -> Citation a
referencesToCitation rs =
  Citation { citationId = Nothing
           , citationNoteNumber = Nothing
           , citationItems = map (\r ->
               CitationItem{ citationItemId = referenceId r
                           , citationItemLabel = Nothing
                           , citationItemLocator = Nothing
                           , citationItemType = NormalCite
                           , citationItemPrefix = Nothing
                           , citationItemSuffix = Nothing }) rs
           }

-- remove >>[0] or ..[1]
removeCitationNums :: Text -> Text
removeCitationNums =
  T.intercalate "\n" . map removeNum . T.lines
 where
  removeNum = T.dropWhile (== ' ') .
              T.dropWhile (\c -> c == '>' || c == '.' ||
                                 c == '[' || c == ']' || isDigit c)


compareTest :: CiteprocTest (CslJson Text)
            -> Text
            -> StateT Counts IO TestResult
compareTest test actual = do
  let expected = case citations test of
                   Just _ -> removeCitationNums $ result test
                   Nothing -> result test
  if actual == expected
     then do
       modify $ \st -> st{ passed = category test : passed st }
       -- suppress PASSED messages
       -- liftIO $ TIO.putStrLn $ "[PASSED]   " <> path test
       return Passed
     else do
       modify $ \st -> st{ failed = category test : failed st }
       liftIO $ do
         TIO.putStrLn $ "[FAILED]   " <> T.pack (path test)
         showDiff expected actual
       return $ Failed actual expected

splitSections :: ByteString -> [(Text, ByteString)]
splitSections = snd . foldl' go startingState . B.lines . removeBOM
 where
  removeBOM bs = if "\xef\xbb\xbf" `B.isPrefixOf` bs
                    then B.drop 3 bs
                    else bs
  startingState ::
    (Maybe (Text, [ByteString]), [(Text, ByteString)])
  startingState = (Nothing, mempty)
  go (Nothing, accum) t
      | ">>==" `B.isPrefixOf` t =
        let secname = T.toLower $ T.filter (\c -> isLetter c || c == '-')
                      $ decodeUtf8 t
         in (Just (secname, mempty), accum)
      | otherwise   = (Nothing, accum)
  go (Just (sec, buffer), accum) t
      | "<<==" `B.isPrefixOf` t =
        (Nothing, (sec, mconcat $ intersperse "\n" (reverse buffer)) : accum)
      | otherwise               = (Just (sec, t:buffer), accum)


loadTestCase :: FilePath -> IO (CiteprocTest (CslJson Text))
loadTestCase fp = do
  sections <- splitSections <$> B.readFile fp
  let cslBs = fromMaybe mempty $ lookup "csl" sections
  let fromJSON field x =
              case A.eitherDecode (L.fromStrict x) of
                     Left e  -> error $ "JSON decoding error " <>
                                 " in " <> fp <> " (" <>
                                 field <> ")\n" <> show e
                     Right z -> z
  reason <- do
    exists <- doesFileExist (fp <> ".skip")
    if exists
       then Just <$> TIO.readFile (fp <> ".skip")
       else return Nothing
  return CiteprocTest
    { name = T.pack $ dropExtension $ takeBaseName fp
    , path = fp
    , category = T.takeWhile (/='_') $ T.pack $ takeBaseName fp
    , mode = maybe mempty decodeUtf8 $ lookup "mode" sections
    , result = maybe mempty decodeUtf8 $ lookup "result" sections
    , csl = cslBs
    , input = maybe (error "No INPUT") (fromJSON "INPUT") $
               lookup "input" sections
    , bibentries = fromJSON "BIBENTRIES" <$> lookup "bibentries" sections
    , bibsection = fromJSON "BIBSECTION" <$> lookup "bibsection" sections
    , citeItems  = fromJSON "CITATION-ITEMS" <$>
                    lookup "citation-items" sections
    , citations =  removeDuplicates . fromJSON "CITATIONS" <$>
                    lookup "citations" sections
       -- need appropriate fromjson instance:
       -- fromJSON "CITATION" <$> lookup "citation-items" sections
    , abbreviations = fromJSON "ABBREVIATIONS" <$>
                     lookup "abbreviations" sections
    , skipReason = reason
    , options = fromJSON "TESTOPTIONS" <$> lookup "options" sections
    }

-- for motivation see e.g. test/csl/collapse_CitationNumberRangesInsert.txt
-- Later Citations can replace earlier ones with the same citationId.
removeDuplicates :: [Citation a] -> [Citation a]
removeDuplicates [] = []
removeDuplicates (c:cs) =
  case citationId c of
    Just cid ->
      if any (\cit -> citationId cit == Just cid) cs
         then removeDuplicates cs
         else c : removeDuplicates cs
    Nothing -> c : removeDuplicates cs

testDir :: FilePath
testDir = "test" </> "csl"

overrideDir :: FilePath
overrideDir = "test" </> "overrides"

extraDir :: FilePath
extraDir = "test" </> "extra"

main :: IO ()
main = do
  args <- getArgs
  let matchesPattern x =
        takeExtension x == ".txt" &&
        case args of
          [] -> True
          _  -> any (\arg -> map toLower arg `isInfixOf` map toLower x) args
  overrides <- if any ('/' `elem`) args
                  then return []
                  else filter matchesPattern <$>
                          getDirectoryContents overrideDir

  let addDir fp = if fp `elem` overrides
                     then overrideDir </> fp
                     else testDir </> fp

  testFiles <- if any ('/' `elem`) args
                  then return args
                  else do
                    cslTests <- map addDir . filter matchesPattern
                                 <$> getDirectoryContents testDir
                    extraTests <- map (extraDir </>) . filter matchesPattern
                                 <$> getDirectoryContents extraDir
                    return $ cslTests ++ extraTests


  testCases <- sortOn name <$> mapM loadTestCase testFiles
  (_,counts) <- timeIt $
                 runStateT (mapM_ runTest testCases)
                           Counts{ failed   = []
                                 , errored  = []
                                 , passed   = []
                                 , skipped  = [] }
  putStrLn ""
  let categories = sort $ Set.toList
                        $ foldr (Set.insert . category) mempty testCases
  putStrLn $ printf "%-15s %6s %6s %6s %6s"
               ("CATEGORY" :: String)
               ("PASS" :: String)
               ("FAIL" :: String)
               ("ERROR" :: String)
               ("SKIP" :: String)
  let resultsFor cat = do
        let p = length . filter (== cat) . passed $ counts
        let f = length . filter (== cat) . failed $ counts
        let e = length . filter (== cat) . errored $ counts
        let s = length . filter (== cat) . skipped $ counts
        let percent = (fromIntegral p / fromIntegral (p + f + e) :: Double)
        putStrLn $ printf "%-15s %6d %6d %6d %6d    |%-20s|"
                     (T.unpack cat) p f e s
                     (replicate (floor (percent * 20.0)) '+')
  mapM_ resultsFor categories
  putStrLn $ printf "%-15s %6s %6s %6s %6s"
               ("-------------" :: String)
               ("-----" :: String)
               ("-----" :: String)
               ("-----" :: String)
               ("-----" :: String)
  putStrLn $ printf "%-15s %6d %6d %6d %6d"
               ("(all)" :: String)
               (length (passed counts))
               (length (failed counts))
               (length (errored counts))
               (length (skipped counts))
  case length (failed counts) + length (errored counts) of
    0 -> exitSuccess
    n | n <= 65 -> do
         putStrLn "We have passed all the CSL tests we expect to..."
         exitSuccess
      | otherwise -> exitWith $ ExitFailure n

data Counts  =
    Counts
    { failed   :: [Text]
    , errored  :: [Text]
    , passed   :: [Text]
    , skipped  :: [Text]
    } deriving (Show)

showDiff :: Text -> Text -> IO ()
showDiff expected actual = do
  -- Use this to see unicode characters (e.g. dashes) better
  -- let f = mconcat . map
  --            (\c -> if isAscii c
  --                      then T.singleton c
  --                      else T.pack (printf "[U+%04X]" (ord c))) . T.unpack
  putStrLn $ Pretty.render $ prettyContextDiff
    (Pretty.text "expected")
    (Pretty.text "actual")
    (Pretty.text . T.unpack)
    $ getContextDiff 1 (T.lines expected) (T.lines actual)

