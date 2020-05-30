{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Citeproc.Data
  (localeFiles)
where
import System.FilePath (FilePath)
import Data.ByteString (ByteString)
import           Data.FileEmbed

localeFiles :: [(FilePath, ByteString)]
localeFiles = $(embedDir "locales")

