{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Citeproc
import Citeproc.CslJson
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty as AesonPretty
import Data.Ord (comparing)
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
  rawargs <- getArgs
  let (opts, args, _errs) = getOpt Permute options rawargs
  let opt = foldr ($) (Opt Nothing Nothing Nothing False) opts
  when (optHelp opt) $ do
    putStr $ usageInfo "citeproc [OPTIONS] [FILE]" options
    exitWith ExitSuccess
  format <- case optFormat opt of
              Just "html" -> return Html
              Just "json" -> return Json
              Just _      -> err "--format must be html or json"
              Nothing     -> return Html
  bs <- case args of
          [] -> BL.getContents
          (f:_) -> BL.readFile f
  case Aeson.eitherDecode bs of
    Left e -> err e
    Right (inp :: Inputs (CslJson Text)) -> do
      stylesheet <- case inputsStylesheet inp of
                      Just s -> return s
                      Nothing ->
                        case optStyle opt of
                          Just fp -> TIO.readFile fp
                          Nothing -> err "No stylesheet specified"
      references <- case inputsReferences inp of
                      Just r -> return r
                      Nothing ->
                        case optReferences opt of
                          Just fp -> do
                            raw <- BL.readFile fp
                            case Aeson.eitherDecode raw of
                              Left e   -> err e
                              Right rs -> return rs
                          Nothing -> err "No references specified"

      parseResult <-
        parseStyle (\_ -> return mempty) stylesheet
      case parseResult of
        Left e -> err (T.unpack $ prettyCiteprocError e)
        Right parsedStyle -> do
          let locale = mergeLocales (inputsLang inp) parsedStyle
          let result= citeproc defaultCiteprocOptions
                         parsedStyle
                         (inputsLang inp)
                         references
                         (fromMaybe [] (inputsCitations inp))
          let jsonResult :: Aeson.Value
              jsonResult =
                case format of
                   Json -> Aeson.object
                          [ ("citations", Aeson.toJSON $
                               map (cslJsonToJson locale)
                                   (resultCitations result))
                          , ("bibliography", Aeson.toJSON $
                               map (\(id',ent) ->
                                 (id', cslJsonToJson locale ent))
                               (resultBibliography result))
                          , ("warnings", Aeson.toJSON $ resultWarnings result)
                          ]
                   Html -> Aeson.toJSON result
          BL.putStr $ AesonPretty.encodePretty'
                       AesonPretty.defConfig
                         { confIndent = AesonPretty.Spaces 2
                         , confCompare = AesonPretty.keyOrder
                             ["citations","bibliography","warnings"]
                             `mappend` comparing T.length }
                       jsonResult
          BL.putStr "\n"

data Format = Json | Html deriving (Show, Ord, Eq)

data Opt =
  Opt{ optStyle          :: Maybe String
     , optReferences :: Maybe String
     , optFormat       :: Maybe String
     , optHelp         :: Bool
     } deriving Show

options :: [OptDescr (Opt -> Opt)]
options =
  [ Option ['s'] ["style"]
     (ReqArg (\fp opt -> opt{ optStyle = Just fp }) "FILE")
     "CSL style file"
  , Option ['r'] ["references"]
     (ReqArg (\fp opt -> opt{ optReferences = Just fp }) "FILE")
     "CSL JSON bibliography"
  , Option ['f'] ["format"]
     (ReqArg (\format opt -> opt{ optFormat = Just format }) "html|json")
     "Controls formatting of entries in result"
  , Option ['h'] ["help"]
     (NoArg (\opt -> opt{ optHelp = True }))
     "Print usage information"
  ]

err :: String -> IO a
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
