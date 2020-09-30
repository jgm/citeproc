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
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
  rawargs <- getArgs
  let (opts, _args, _errs) = getOpt Permute options rawargs
  let opt = foldr ($) (Opt Nothing Nothing) opts
  print opt
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
           BL.putStr $ Aeson.encode $ citeproc
                                        defaultCiteprocOptions
                                        parsedStyle
                                        (inputsLang inp)
                                        references
                                        (fromMaybe []
                                          (inputsCitations inp))
           BL.putStr "\n"

data Opt =
  Opt{ optCsl          :: Maybe String
     , optBibliography :: Maybe String
     } deriving Show

options :: [OptDescr (Opt -> Opt)]
options =
  [ Option ['s'] ["csl"]
     (ReqArg (\fp opt -> opt{ optCsl = Just fp }) "FILE")
     "CSL style file"
  , Option ['b'] ["bibliography"]
     (ReqArg (\fp opt -> opt{ optBibliography = Just fp }) "FILE")
     "CSL JSON bibliography"
  ]

err :: String -> IO a
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
