{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Citeproc
import Citeproc.CslJson
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
  let (opts, _args, _errs) = getOpt Permute options rawargs
  let opt = foldr ($) (Opt Nothing Nothing Nothing) opts
  format <- case optFormat opt of
              Just "html" -> return Html
              Just "json" -> return Json
              Just _      -> err "--format must be html or json"
              Nothing     -> return Html
  bs <- BL.getContents
  case Aeson.eitherDecode bs of
    Left e -> err e
    Right (inp :: Inputs (CslJson Text)) -> do
      stylesheet <- case inputsStylesheet inp of
                      Just s -> return s
                      Nothing ->
                        case optCsl opt of
                          Just fp -> TIO.readFile fp
                          Nothing -> err "No stylesheet specified"
      references <- case inputsReferences inp of
                      Just r -> return r
                      Nothing ->
                        case optBibliography opt of
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
  Opt{ optCsl          :: Maybe String
     , optBibliography :: Maybe String
     , optFormat       :: Maybe String
     } deriving Show

options :: [OptDescr (Opt -> Opt)]
options =
  [ Option ['s'] ["csl"]
     (ReqArg (\fp opt -> opt{ optCsl = Just fp }) "FILE")
     "CSL style file"
  , Option ['b'] ["bibliography"]
     (ReqArg (\fp opt -> opt{ optBibliography = Just fp }) "FILE")
     "CSL JSON bibliography"
  , Option [] ["format"]
     (ReqArg (\format opt -> opt{ optFormat = Just format }) "FILE")
     "html|json"
  ]

err :: String -> IO a
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
