{-# LANGUAGE OverloadedStrings #-}
-- | Benchmark for citation processing dominated by disambiguation.
-- Generates n synthetic references, cites them all in clusters of
-- three, and times 'citeproc', in two scenarios:
--
-- * dense: every reference's authors and years collide heavily (so
--   that add-names, add-givenname, and year-suffix disambiguation all
--   kick in); reference data repeats with period 24, so every
--   reference has many exact duplicates.
--
-- * sparse: only every 20th reference collides; the rest have unique
--   authors.  This is the realistic case for large bibliographies.
--
-- * collapse: the style has collapse="year" and the citation clusters
--   have 100 items each, to exercise the cite grouping/collapsing
--   code; authors are unique so that disambiguation stays quiet (and
--   grouping does all-pairs comparisons, its worst case).
--
-- Run with, e.g.:
--   cabal bench --benchmark-options="800 1600 3200"
module Main (main) where
import Citeproc
import Citeproc.CslJson (CslJson, renderCslJson)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.TimeIt (timeItT)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let sizes = if null args then [800, 1600, 3200] else map read args
  let getStyle collapse = do
        parseResult <- parseStyle (\_ -> return "") (styleText collapse)
        case parseResult of
          Left err  -> print err >> exitFailure
          Right sty -> return (sty :: Style (CslJson Text))
  style <- getStyle False
  collapseStyle <- getStyle True
  let scenarios = [ ("dense",    const True,            3,   style)
                  , ("sparse",   \i -> i `mod` 20 == 0, 3,   style)
                  , ("collapse", const False,           100, collapseStyle)
                  ] :: [(String, Int -> Bool, Int, Style (CslJson Text))]
  forM_ scenarios $ \(scenario, collides, clusterSize, sty) ->
      forM_ sizes $ \n -> do
        refs <- either fail return $ mapM refFromValue
                  $ mkRefValues collides n
        let result = citeproc defaultCiteprocOptions sty Nothing refs
                       (mkCitations clusterSize n)
        (t, outlen) <- timeItT $ evaluate $ T.length $ T.concat
                         $ map (renderCslJson False mempty)
                         $ resultCitations result
        printf "%-6s  n = %5d   %8.3f s   (%d chars of output)\n"
               scenario n t outlen

refFromValue :: Aeson.Value -> Either String (Reference (CslJson Text))
refFromValue v =
  case Aeson.fromJSON v of
    Aeson.Success r -> Right r
    Aeson.Error e   -> Left e

mkRefValues :: (Int -> Bool) -> Int -> [Aeson.Value]
mkRefValues collides n = map mkRef [1..n]
 where
  mkRef :: Int -> Aeson.Value
  mkRef i = object
    [ "id" .= itemName i
    , "type" .= ("book" :: Text)
    , "title" .= ("Title " <> T.pack (show i))
    , "issued" .= object ["date-parts" .= [[2000 + i `mod` 4]]]
    , "author" .=
        if collides i
           then map (mkAuthor i) [0 .. i `mod` 3]
           else [object [ "family" .= ("Unique" <> T.pack (show i))
                        , "given"  .= ("Author" :: Text) ]]
    ]
  mkAuthor i j = object
    [ "family" .= families !! ((i + j) `mod` length families)
    , "given"  .= givens !! ((i + j) `mod` length givens)
    ]
  families, givens :: [Text]
  families = ["Smith", "Jones", "Garcia", "Chen",
              "Miller", "Davis", "Wilson", "Moore"]
  givens = ["Alexandra", "Benjamin", "Catherine",
            "Daniel", "Eleanor", "Frederick"]

mkCitations :: Int -> Int -> [Citation (CslJson Text)]
mkCitations clusterSize n = map mkCitation (chunksOf clusterSize [1..n])
 where
  chunksOf _ [] = []
  chunksOf k xs = let (as, bs) = splitAt k xs in as : chunksOf k bs
  mkCitation is = Citation
    { citationId = Nothing
    , citationResetPosition = False
    , citationNoteNumber = Nothing
    , citationPrefix = Nothing
    , citationSuffix = Nothing
    , citationItems = map mkItem is
    }
  mkItem i = CitationItem
    { citationItemId = ItemId (itemName i)
    , citationItemLabel = Nothing
    , citationItemLocator = Nothing
    , citationItemType = NormalCite
    , citationItemPrefix = Nothing
    , citationItemSuffix = Nothing
    , citationItemData = Nothing
    }

itemName :: Int -> Text
itemName i = "ref" <> T.pack (show i)

-- An author-date style with every disambiguation strategy enabled
-- (and, if the argument is True, collapse=\"year\").
styleText :: Bool -> Text
styleText collapse = T.unlines
  [ "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\" version=\"1.0\">"
  , "  <info> <id/> <title/> <updated>2020-01-01T00:00:00Z</updated> </info>"
  , "  <citation disambiguate-add-names=\"true\""
  , "            disambiguate-add-givenname=\"true\""
  , "            disambiguate-add-year-suffix=\"true\""
  , if collapse then "            collapse=\"year\"" else ""
  , "            et-al-min=\"3\" et-al-use-first=\"1\">"
  , "    <layout prefix=\"(\" suffix=\")\" delimiter=\"; \">"
  , "      <group delimiter=\" \">"
  , "        <names variable=\"author\">"
  , "          <name form=\"short\" and=\"symbol\"/>"
  , "        </names>"
  , "        <date variable=\"issued\" form=\"numeric\" date-parts=\"year\"/>"
  , "        <choose>"
  , "          <if disambiguate=\"true\">"
  , "            <text value=\"[d]\"/>"
  , "          </if>"
  , "        </choose>"
  , "      </group>"
  , "    </layout>"
  , "  </citation>"
  , "</style>"
  ]
