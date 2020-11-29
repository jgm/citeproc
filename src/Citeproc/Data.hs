{-# LANGUAGE TemplateHaskell #-}
module Citeproc.Data
  (localeFiles)
where
import Data.ByteString (ByteString)
import Data.FileEmbed

localeFiles :: [(FilePath, ByteString)]
localeFiles = $(embedDir "locales")

