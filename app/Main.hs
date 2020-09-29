{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Citeproc
import Citeproc.CslJson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import System.IO
import System.Exit

main :: IO ()
main = do
  bs <- BL.getContents
  case Aeson.eitherDecode bs of
    Left e -> err e
    Right (inp :: Inputs (CslJson Text)) ->
      print inp


err :: String -> IO ()
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
