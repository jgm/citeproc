{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Semigroup
import Data.Char (isUpper, isLower, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Citeproc.Types (Lang(..))

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
    | AfterOtherPunctuation
    | BeforeLastWord
    deriving (Show, Eq)

toUpper' :: Maybe Lang -> Text -> Text
toUpper' mblang = T.toUpper .
  case mblang of
    Just (Lang "tr" _) -> T.map (\c -> case c of
                                        'i' -> 'İ'
                                        'ı' -> 'I'
                                        _   -> c)
    _                  -> id

toLower' :: Maybe Lang -> Text -> Text
toLower' mblang = T.toLower .
  case mblang of
    Just (Lang "tr" _) -> T.map (\c -> case c of
                                        'İ' -> 'i'
                                        'I' -> 'ı'
                                        _   -> c)
    _                  -> id

-- | Uppercase everything.
withUppercaseAll :: CaseTransformer
withUppercaseAll = CaseTransformer (\mblang _ -> toUpper' mblang)

-- | Lowercase everything.
withLowercaseAll :: CaseTransformer
withLowercaseAll = CaseTransformer (\mblang _ -> toLower' mblang)

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
       = T.toLower chunk
     | isCapitalized chunk || T.all isLower chunk
     , st == Start || st == StartSentence
       = capitalizeText mblang $ T.toLower chunk
     | otherwise = chunk

-- | Use title case.
withTitleCase :: CaseTransformer
withTitleCase = CaseTransformer go
 where
  go mblang st chunk
     | isMixedCase chunk = chunk
     | T.all isUpper chunk = chunk  -- spec doesn't say this but tests do
                                    -- textcase_TitleCapitalization.txt
     | T.any (not . isAscii) chunk = chunk
     | st == StartSentence || st == Start =
       capitalizeText mblang $ T.toLower chunk
     | st == AfterWordEnd
     , not (isStopWord chunk)
     , T.compareLength chunk 1 == GT = capitalizeText mblang $ T.toLower chunk
     | st == BeforeLastWord
     , T.compareLength chunk 1 == GT = capitalizeText mblang $ T.toLower chunk
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
    Just (c,x') -> toUpper' mblang (T.singleton c) <> x'
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

