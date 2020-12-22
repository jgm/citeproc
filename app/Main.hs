{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Citeproc
import Citeproc.CslJson
import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Data.Bifunctor (second)
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
  let (opts, args, errs) = getOpt Permute options rawargs
  unless (null errs) $ do
    mapM_ err errs
    exitWith $ ExitFailure 1
  let opt = foldr ($) defaultOpt opts
  when (optHelp opt) $ do
    putStr $ usageInfo "citeproc [OPTIONS] [FILE]" options
    exitSuccess
  when (optVersion opt) $ do
    putStrLn $ "citeproc version " <> VERSION_citeproc
    exitSuccess
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
      stylesheet <- case optStyle opt of
                      Just fp -> T.dropWhile (=='\xFEFF') <$> -- drop BOM
                                       TIO.readFile fp
                      Nothing ->
                        case inputsStyle inp of
                          Just s  -> return s
                          Nothing -> err "No style specified"
      references <- case optReferences opt of
                      Just fp -> do
                        raw <- BL.readFile fp
                        case Aeson.eitherDecode raw of
                          Left e   -> err e
                          Right rs -> return rs
                      Nothing ->
                        case inputsReferences inp of
                          Just rs  -> return rs
                          Nothing  -> err "No references specified"
      abbreviations <- case optAbbreviations opt of
                          Just fp -> do
                            raw <- BL.readFile fp
                            case Aeson.eitherDecode raw of
                              Left e   -> err e
                              Right ab -> return $ Just ab
                          Nothing -> return $ inputsAbbreviations inp
      let lang = optLang opt <|> inputsLang inp

      parseResult <-
        parseStyle (\_ -> return mempty) stylesheet
      case parseResult of
        Left e -> err (T.unpack $ prettyCiteprocError e)
        Right parsedStyle -> do
          let style = parsedStyle{ styleAbbreviations = abbreviations }
          let locale = mergeLocales lang style
          let result= citeproc defaultCiteprocOptions
                         style
                         lang
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
                               map (second $ cslJsonToJson locale)
                                   (resultBibliography result))
                          , ("warnings", Aeson.toJSON $ resultWarnings result)
                          ]
                   Html -> Aeson.toJSON result
          BL.putStr $ AesonPretty.encodePretty'
                       AesonPretty.defConfig
                         { confIndent = AesonPretty.Spaces 2
                         , confCompare = AesonPretty.keyOrder
                             ["citations","bibliography","warnings"]
                             `mappend` comparing T.length
                         , confTrailingNewline = True }
                       jsonResult

data Format = Json | Html deriving (Show, Ord, Eq)

data Opt =
  Opt{ optStyle         :: Maybe String
     , optReferences    :: Maybe String
     , optAbbreviations :: Maybe String
     , optFormat        :: Maybe String
     , optLang          :: Maybe Lang
     , optHelp          :: Bool
     , optVersion       :: Bool
     } deriving Show

defaultOpt :: Opt
defaultOpt =
  Opt { optStyle = Nothing
      , optReferences = Nothing
      , optAbbreviations = Nothing
      , optFormat = Nothing
      , optLang = Nothing
      , optHelp = False
      , optVersion = False
      }

options :: [OptDescr (Opt -> Opt)]
options =
  [ Option ['s'] ["style"]
     (ReqArg (\fp opt -> opt{ optStyle = Just fp }) "FILE")
     "CSL style file"
  , Option ['r'] ["references"]
     (ReqArg (\fp opt -> opt{ optReferences = Just fp }) "FILE")
     "CSL JSON bibliography"
  , Option ['a'] ["abbreviations"]
     (ReqArg (\fp opt -> opt{ optAbbreviations = Just fp }) "FILE")
     "CSL abbreviations table"
  , Option ['l'] ["lang"]
     (ReqArg (\lang opt -> opt{ optLang = Just $ parseLang $ T.pack lang })
        "IETF language")
     "Override locale"
  , Option ['f'] ["format"]
     (ReqArg (\format opt -> opt{ optFormat = Just format }) "html|json")
     "Controls formatting of entries in result"
  , Option ['h'] ["help"]
     (NoArg (\opt -> opt{ optHelp = True }))
     "Print usage information"
  , Option ['V'] ["version"]
     (NoArg (\opt -> opt{ optVersion = True }))
     "Print version number"
  ]

err :: String -> IO a
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
