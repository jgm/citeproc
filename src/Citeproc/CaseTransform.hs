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
import Data.Char (isUpper, isLower)
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
     | T.all isUpper chunk = chunk  -- spec doesn't say this but tests do
                                    -- textcase_TitleCapitalization.txt
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

isStopWord :: Text -> Bool
isStopWord "a" = True
isStopWord "an" = True
isStopWord "and" = True
isStopWord "as" = True
isStopWord "at" = True
isStopWord "but" = True
isStopWord "by" = True
isStopWord "down" = True
isStopWord "for" = True
isStopWord "from" = True
isStopWord "in" = True
isStopWord "into" = True
isStopWord "nor" = True
isStopWord "of" = True
isStopWord "on" = True
isStopWord "onto" = True
isStopWord "or" = True
isStopWord "over" = True
isStopWord "so" = True
isStopWord "the" = True
isStopWord "till" = True
isStopWord "to" = True
isStopWord "up" = True
isStopWord "via" = True
isStopWord "with" = True
isStopWord "yet" = True
-- not in original list but seems required by test flipflop_Apostrophes
-- and textcase_SkipNameParticlesInTitleCase
isStopWord "about" = True
isStopWord "van" = True
isStopWord "von" = True
isStopWord "de" = True
isStopWord "d" = True
isStopWord "l" = True
isStopWord _ = False

