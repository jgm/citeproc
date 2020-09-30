{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Citeproc
import Citeproc.CslJson
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import System.IO
import System.Exit

main :: IO ()
main = do
  bs <- BL.getContents
  case Aeson.eitherDecode bs of
    Left e -> err e
    Right (inp :: Inputs (CslJson Text)) -> do
      parseResult <-
        parseStyle (\_ -> return mempty) (inputsStylesheet inp)
      case parseResult of
        Left e -> err (unpack $ prettyCiteprocError e)
        Right parsedStyle -> do
           BL.putStr $ Aeson.encode $ citeproc
                                        defaultCiteprocOptions
                                        parsedStyle
                                        (inputsLang inp)
                                        (inputsReferences inp)
                                        (inputsCitations inp)
           BL.putStr "\n"

err :: String -> IO ()
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
