{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Citeproc
import Citeproc.CslJson
import Data.Algorithm.DiffContext
import System.TimeIt (timeIt)
import Control.Monad (unless, when)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Exit
import System.Directory (getDirectoryContents, doesFileExist, removeFile)
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
import Data.Maybe (fromMaybe, isJust)
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
  , expectedFailure :: Maybe Text
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

-- | Command line options of the test suite itself.
data SpecOptions =
  SpecOptions
  { accept  :: Bool  -- ^ record failures in .expected files
  , verbose :: Bool  -- ^ report warnings, passes and expected failures
  } deriving (Show)

runTest :: SpecOptions
        -> CiteprocTest (CslJson Text)
        -> StateT Counts IO TestResult
runTest specOpts test = do
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
        modify $ \st -> st{ errored = (category test, path test) : errored st }
        liftIO $ do
          TIO.putStrLn $ "[ERRORED]  " <> T.pack (path test)
          TIO.putStrLn $ T.pack $ show err
          TIO.putStrLn ""
        return $ Errored err
  let doSkip reason = do
        modify $ \st -> st{ skipped = (category test, path test) : skipped st }
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
            when (verbose specOpts && not (null (resultWarnings actual))) $
              liftIO $ do
                TIO.putStrLn $ "[WARNING]  " <> T.pack (path test)
                mapM_ (TIO.putStrLn . ("==> " <>))
                     $ resultWarnings actual
            case mode test of
              "citation" -> compareTest specOpts test
                  (T.intercalate "\n" $ map (renderCslJson' loc)
                                          (resultCitations actual))
              "bibliography" -> compareTest specOpts test
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
           , citationResetPosition = False
           , citationNoteNumber = Nothing
           , citationPrefix = Nothing
           , citationSuffix = Nothing
           , citationItems = map (\r ->
               CitationItem{ citationItemId = referenceId r
                           , citationItemLabel = Nothing
                           , citationItemLocator = Nothing
                           , citationItemType = NormalCite
                           , citationItemPrefix = Nothing
                           , citationItemSuffix = Nothing
                           , citationItemData = Nothing }) rs
           }

-- remove >>[0] or ..[1]
removeCitationNums :: Text -> Text
removeCitationNums =
  T.intercalate "\n" . map removeNum . T.lines
 where
  removeNum = T.dropWhile (== ' ') .
              T.dropWhile (\c -> c == '>' || c == '.' ||
                                 c == '[' || c == ']' || isDigit c)


compareTest :: SpecOptions
            -> CiteprocTest (CslJson Text)
            -> Text
            -> StateT Counts IO TestResult
compareTest specOpts test actual = do
  let expected = if mode test == "citation" && isJust (citations test)
                    then removeCitationNums $ result test
                    else result test
  let expectedFailureFile = expectedFailurePath (path test)
  if actual == expected
     then do
       modify $ \st -> st{ passed = (category test, path test) : passed st }
       when (verbose specOpts) $
         liftIO $ TIO.putStrLn $ "[PASSED]   " <> T.pack (path test)
       case expectedFailure test of
         Nothing -> return ()
         Just _
           | accept specOpts -> liftIO $ do
               removeFile expectedFailureFile
               putStrLn $ "[REMOVED]  " <> expectedFailureFile
           | otherwise -> modify $ \st ->
               st{ unexpectedPasses = path test : unexpectedPasses st }
       return Passed
     else do
       modify $ \st -> st{ failed = (category test, path test) : failed st }
       if expectedFailure test == Just actual
          then when (verbose specOpts) $ liftIO $ do
                 TIO.putStrLn $ "[FAILED:EXPECTED] " <> T.pack (path test)
                 showDiff expected actual
          else if accept specOpts
                  then liftIO $ do
                    TIO.writeFile expectedFailureFile (actual <> "\n")
                    putStrLn $ "[ACCEPTED] " <> expectedFailureFile
                  else do
                    modify $ \st ->
                      st{ unexpectedFailures =
                            path test : unexpectedFailures st }
                    liftIO $ do
                      TIO.putStrLn $
                        "[FAILED:UNEXPECTED] " <> T.pack (path test)
                      showDiff expected actual
       return $ Failed actual expected

-- A test that is known to fail has, alongside its .txt file, an .expected
-- file containing the output we currently produce.  The failure counts as
-- an expected failure only if the output still matches this file exactly.
expectedFailurePath :: FilePath -> FilePath
expectedFailurePath fp = fp -<.> ".expected"

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
  expectedFail <- do
    let expectedFp = expectedFailurePath fp
    exists <- doesFileExist expectedFp
    if exists
       -- the file has a final newline, the test output does not
       then Just . T.dropWhileEnd (== '\n') <$> TIO.readFile expectedFp
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
    , expectedFailure = expectedFail
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
  -- with --accept, the .expected file of every failing test is (re)written
  -- with its current output, and that of every passing test is removed;
  -- with --verbose, passes and expected failures are reported too
  let specOpts = SpecOptions{ accept  = "--accept" `elem` args
                            , verbose = "--verbose" `elem` args }
  let patterns = filter (`notElem` ["--accept", "--verbose"]) args
  let matchesPattern x =
        takeExtension x == ".txt" &&
        case patterns of
          [] -> True
          _  -> any (\arg -> map toLower arg `isInfixOf` map toLower x) patterns
  overrides <- if any ('/' `elem`) patterns
                  then return []
                  else filter matchesPattern <$>
                          getDirectoryContents overrideDir

  let addDir fp = if fp `elem` overrides
                     then overrideDir </> fp
                     else testDir </> fp

  testFiles <- if any ('/' `elem`) patterns
                  then return patterns
                  else do
                    cslTests <- map addDir . filter matchesPattern
                                 <$> getDirectoryContents testDir
                    extraTests <- map (extraDir </>) . filter matchesPattern
                                 <$> getDirectoryContents extraDir
                    return $ cslTests ++ extraTests


  testCases <- sortOn name <$> mapM loadTestCase testFiles
  (_,counts) <- timeIt $
                 runStateT (mapM_ (runTest specOpts) testCases)
                           Counts{ failed   = []
                                 , errored  = []
                                 , passed   = []
                                 , skipped  = []
                                 , unexpectedFailures = []
                                 , unexpectedPasses   = [] }
  putStrLn ""
  let categories = sort $ Set.toList
                        $ foldr (Set.insert . category) mempty testCases
  putStrLn $ printf "%-29s %6s %6s %6s %6s"
               ("CATEGORY" :: String)
               ("  PASS" :: String)
               ("  FAIL" :: String)
               (" ERROR" :: String)
               ("  SKIP" :: String)
  let resultsFor cat = do
        let p = length . filter ((== cat) . fst) . passed $ counts
        let f = length . filter ((== cat) . fst) . failed $ counts
        let e = length . filter ((== cat) . fst) . errored $ counts
        let s = length . filter ((== cat) . fst) . skipped $ counts
        let percent = (fromIntegral p / fromIntegral (p + f + e) :: Double)
        putStrLn $ printf "%-29s %6d %6d %6d %6d |%-20s|"
                     (T.unpack cat) p f e s
                     (replicate (floor (percent * 20.0)) '+')
  mapM_ resultsFor categories
  putStrLn $ printf "%-30s %6s %6s %6s %6s"
               ("-------------" :: String)
               ("-----" :: String)
               ("-----" :: String)
               ("-----" :: String)
               ("-----" :: String)
  putStrLn $ printf "%-30s %6d %6d %6d %6d"
               ("(all)" :: String)
               (length (passed counts))
               (length (failed counts))
               (length (errored counts))
               (length (skipped counts))
  unless (null (unexpectedFailures counts)) $ do
    putStrLn ""
    putStrLn "Unexpected failures"
    putStrLn "-------------------"
    mapM_ putStrLn (unexpectedFailures counts)
    putStrLn ""
  unless (null (unexpectedPasses counts)) $ do
    putStrLn ""
    putStrLn "Unexpected passes"
    putStrLn "-----------------"
    mapM_ putStrLn (unexpectedPasses counts)
    putStrLn ""
  case length (unexpectedFailures counts) + length (errored counts) of
    0 -> do
      putStrLn "(All failures were expected failures.)"
      exitSuccess
    n -> exitWith $ ExitFailure n

data Counts  =
    Counts
    { failed   :: [(Text,FilePath)]  -- category, filepath
    , errored  :: [(Text,FilePath)]
    , passed   :: [(Text,FilePath)]
    , skipped  :: [(Text,FilePath)]
    , unexpectedFailures :: [FilePath]  -- failed, but not as recorded
    , unexpectedPasses   :: [FilePath]  -- passed, though a failure was recorded
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
    (Pretty.text . T.unpack . unnumber)
    $ getContextDiff Nothing (T.lines expected) (T.lines actual)
