{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides functions that facilitate defining textcase transformations.
-- To see how these can be used used, see the definitions of @addTextCase@
-- in "Citeproc.Pandoc" and "Citproc.CslJson".
module Citeproc.CaseTransform
  ( CaseTransformState(..)
  , CaseTransformer(..)
  , withUppercaseAll
  , withLowercaseAll
  , withCapitalizeWords
  , withCapitalizeFirst
  , withSentenceCase
  , withTitleCase
  )
where

import Data.Ord ()
import Data.Char (isUpper, isLower, isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import Citeproc.Types (Lang(..))
import qualified Citeproc.Unicode as Unicode

-- | Wraps a function used to define textcase transformations.
newtype CaseTransformer =
  CaseTransformer
  { unCaseTransformer :: Maybe Lang -> CaseTransformState -> Text -> Text }

-- | Tracks context in textcase transformations.
data CaseTransformState =
      Start
    | StartSentence
    | AfterWordEnd
    | AfterWordChar
    | AfterSentenceEndingPunctuation
    | BeforeLastWord
    deriving (Show, Eq)

-- | Uppercase everything.
withUppercaseAll :: CaseTransformer
withUppercaseAll =
  CaseTransformer (\mblang _ -> Unicode.toUpper mblang)

-- | Lowercase everything.
withLowercaseAll :: CaseTransformer
withLowercaseAll =
  CaseTransformer (\mblang _ -> Unicode.toLower mblang)

-- | Capitalize all words.
withCapitalizeWords :: CaseTransformer
withCapitalizeWords = CaseTransformer go
 where
  go mblang st chunk
     | isMixedCase chunk = chunk
     | st == Start || st == StartSentence || st == AfterWordEnd ||
       st == BeforeLastWord
       = if T.all isLower chunk
            then capitalizeText mblang chunk
            else chunk
     | otherwise = chunk

-- | Capitalize first letter.
withCapitalizeFirst :: CaseTransformer
withCapitalizeFirst = CaseTransformer go
 where
  go mblang st chunk
     | isMixedCase chunk = chunk
     | st == Start
       = if T.all isLower chunk
            then capitalizeText mblang chunk
            else chunk
     | otherwise = chunk

-- | Capitalize first letter of each sentence.
withSentenceCase :: CaseTransformer
withSentenceCase = CaseTransformer go
 where
  go mblang st chunk
     | isCapitalized chunk
     , not (st == Start || st == StartSentence)
       = Unicode.toLower mblang chunk
     | isCapitalized chunk || T.all isLower chunk
     , st == Start || st == StartSentence
       = capitalizeText mblang $ Unicode.toLower mblang chunk
     | otherwise = chunk

-- | Use title case.
withTitleCase :: CaseTransformer
withTitleCase = CaseTransformer go
 where
  go mblang st chunk
     | isMixedCase chunk = chunk
     | T.all (\c -> isUpper c || not (isLetter c)) chunk = chunk
        -- spec doesn't say this but tests do: textcase_TitleCapitalization.txt
     | st == StartSentence || st == Start =
       capitalizeText mblang $ Unicode.toLower mblang chunk
     | st == AfterWordEnd
     , not (isStopWord chunk)
     , T.compareLength chunk 1 == GT =
         capitalizeText mblang $ Unicode.toLower mblang chunk
     | st == BeforeLastWord
     , T.compareLength chunk 1 == GT =
         capitalizeText mblang $ Unicode.toLower mblang chunk
     | otherwise = chunk

isCapitalized :: Text -> Bool
isCapitalized t =
  case T.uncons t of
    Just (c, t') -> isUpper c && T.all isLower t'
    _ -> False

isMixedCase :: Text -> Bool
isMixedCase t = T.any isUpper t && T.any isLower t

capitalizeText :: Maybe Lang -> Text -> Text
capitalizeText mblang x =
  case T.uncons x of
    Just (c,x') -> Unicode.toUpper mblang (T.singleton c) <> x'
    Nothing     -> x

-- Source: https://raw.githubusercontent.com/citation-style-language/schema/master/schemas/styles/stop-words.json .
-- We have omitted the ones containing spaces, like "ahead of".
isStopWord :: Text -> Bool
isStopWord "a" = True
isStopWord "across" = True
isStopWord "afore" = True
isStopWord "after" = True
isStopWord "against" = True
isStopWord "along" = True
isStopWord "alongside" = True
isStopWord "amid" = True
isStopWord "amidst" = True
isStopWord "among" = True
isStopWord "amongst" = True
isStopWord "an" = True
isStopWord "and" = True
isStopWord "anenst" = True
isStopWord "apropos" = True
isStopWord "apud" = True
isStopWord "around" = True
isStopWord "as" = True
isStopWord "aside" = True
isStopWord "astride" = True
isStopWord "at" = True
isStopWord "athwart" = True
isStopWord "atop" = True
isStopWord "barring" = True
isStopWord "before" = True
isStopWord "behind" = True
isStopWord "below" = True
isStopWord "beneath" = True
isStopWord "beside" = True
isStopWord "besides" = True
isStopWord "between" = True
isStopWord "beyond" = True
isStopWord "but" = True
isStopWord "by" = True
isStopWord "c" = True
isStopWord "ca" = True
isStopWord "circa" = True
isStopWord "close to" = True
isStopWord "d'" = True
isStopWord "de" = True
isStopWord "despite" = True
isStopWord "down" = True
isStopWord "during" = True
isStopWord "et" = True
isStopWord "except" = True
isStopWord "for" = True
isStopWord "forenenst" = True
isStopWord "from" = True
isStopWord "given" = True
isStopWord "in" = True
isStopWord "inside" = True
isStopWord "into" = True
isStopWord "lest" = True
isStopWord "like" = True
isStopWord "modulo" = True
isStopWord "near" = True
isStopWord "next" = True
isStopWord "nor" = True
isStopWord "notwithstanding" = True
isStopWord "of" = True
isStopWord "off" = True
isStopWord "on" = True
isStopWord "onto" = True
isStopWord "or" = True
isStopWord "out" = True
isStopWord "over" = True
isStopWord "per" = True
isStopWord "plus" = True
isStopWord "pro" = True
isStopWord "qua" = True
isStopWord "sans" = True
isStopWord "since" = True
isStopWord "so" = True
isStopWord "than" = True
isStopWord "the" = True
isStopWord "through" = True
isStopWord "throughout" = True
isStopWord "thru" = True
isStopWord "thruout" = True
isStopWord "till" = True
isStopWord "to" = True
isStopWord "toward" = True
isStopWord "towards" = True
isStopWord "under" = True
isStopWord "underneath" = True
isStopWord "until" = True
isStopWord "unto" = True
isStopWord "up" = True
isStopWord "upon" = True
isStopWord "v." = True
isStopWord "van" = True
isStopWord "versus" = True
isStopWord "via" = True
isStopWord "vis-à-vis" = True
isStopWord "von" = True
isStopWord "vs." = True
isStopWord "where as" = True
isStopWord "with" = True
isStopWord "within" = True
isStopWord "without" = True
isStopWord "yet" = True
-- not in original list but seems required by test flipflop_Apostrophes
-- and textcase_SkipNameParticlesInTitleCase
isStopWord "about" = True
isStopWord "d" = True
isStopWord "l" = True
isStopWord _ = False

