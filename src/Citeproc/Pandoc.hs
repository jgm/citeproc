{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines a CiteprocOutput instance for pandoc Inlines.
module Citeproc.Pandoc
  ( caseTransform
  )
where
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Citeproc.Types
import Citeproc.CaseTransform
import Control.Monad.Trans.State.Strict as S
import Control.Monad (unless, when)
import Data.Functor.Reverse
import Data.Char (isSpace, isAscii, isPunctuation, isAlphaNum)

instance CiteprocOutput Inlines where
  toText                = stringify
  fromText              = B.text
  dropTextWhile f       = dropTextWhile' f
  dropTextWhileEnd f    = dropTextWhileEnd' f
  addFontVariant x      =
    case x of
      NormalVariant    -> id
      SmallCapsVariant -> B.smallcaps
  addFontStyle x        =
    case x of
      NormalFont       -> id
      ItalicFont       -> B.emph
      ObliqueFont      -> B.emph
  addFontWeight x       =
    case x of
      NormalWeight     -> id
      LightWeight      -> id
      BoldWeight       -> B.strong
  addTextDecoration x   =
    case x of
      NoDecoration        -> B.spanWith ("",["nodecoration"],[])
      UnderlineDecoration -> B.underline
  addVerticalAlign x    =
    case x of
      BaselineAlign    -> id
      SubAlign         -> B.subscript
      SupAlign         -> B.superscript
  addTextCase mblang x =
    case x of
      Lowercase        -> caseTransform (withLowercaseAll mblang)
      Uppercase        -> caseTransform (withUppercaseAll mblang)
      CapitalizeFirst  -> caseTransform (withCapitalizeFirst mblang)
      CapitalizeAll    -> caseTransform (withCapitalizeWords mblang)
      SentenceCase     -> caseTransform (withSentenceCase mblang)
      TitleCase        -> caseTransform (withTitleCase mblang)
  addDisplay x          =
    case x of
      DisplayBlock       -> B.spanWith ("",["csl-block"],[])
      DisplayLeftMargin  -> B.spanWith ("",["csl-left-margin"],[])
      DisplayRightInline -> B.spanWith ("",["csl-right-inline"],[])
      DisplayIndent      -> B.spanWith ("",["csl-indent"],[])
  addQuotes             = B.doubleQuoted . flipFlopQuotes DoubleQuote
  inNote                = B.note . B.para .
                            addTextCase Nothing CapitalizeFirst
  movePunctuationInsideQuotes
                        = punctuationInsideQuotes
  mapText f             = walk go
    where go (Str t) = Str (f t)
          go x       = x
  addHyperlink t        = B.link t ""

flipFlopQuotes :: QuoteType -> Inlines -> Inlines
flipFlopQuotes qt = B.fromList . map (go qt) . B.toList
 where
  go :: QuoteType -> Inline -> Inline
  go q (Quoted _ zs) =
    let q' = case q of
               SingleQuote -> DoubleQuote
               DoubleQuote -> SingleQuote
    in  Quoted q' (map (go q') zs)
  go q (SmallCaps zs) = SmallCaps (map (go q) zs)
  go q (Superscript zs) = Superscript (map (go q) zs)
  go q (Subscript zs) = Subscript (map (go q) zs)
  go q (Span attr zs) = Span attr (map (go q) zs)
  go q (Emph zs) = Emph (map (go q) zs)
  go q (Underline zs) = Underline (map (go q) zs)
  go q (Strong zs) = Strong (map (go q) zs)
  go q (Strikeout zs) = Strikeout (map (go q) zs)
  go q (Cite cs zs) = Cite cs (map (go q) zs)
  go q (Link attr zs t) = Link attr (map (go q) zs) t
  go q (Image attr zs t) = Image attr (map (go q) zs) t
  go _ x = x

punctuationInsideQuotes :: Inlines -> Inlines
punctuationInsideQuotes = B.fromList . go . walk go . B.toList
 where
  go [] = []
  go (Quoted qt xs : Str t : rest)
    | "." `T.isPrefixOf` t ||
      "," `T.isPrefixOf` t
      = Quoted qt (xs ++ [Str (T.take 1 t) | not (endWithPunct True xs)]) :
        if T.length t == 1
           then go rest
           else Str (T.drop 1 t) : go rest
  go (x:xs) = x : go xs

endWithPunct :: Bool -> [Inline] -> Bool
endWithPunct _ [] = False
endWithPunct onlyFinal xs@(_:_) =
  case reverse (T.unpack $ stringify xs) of
       []                       -> True
       -- covers .), .", etc.:
       (d:c:_) | isPunctuation d
                 && not onlyFinal
                 && isEndPunct c -> True
       (c:_) | isEndPunct c      -> True
             | otherwise         -> False
  where isEndPunct c = c `elem` (".,;:!?" :: String)

dropTextWhile' :: (Char -> Bool) -> Inlines -> Inlines
dropTextWhile' f ils = evalState (walkM go ils) True
 where
  go x = do
    atStart <- get
    if atStart
       then
         case x of
           Str t -> do
             let t' = T.dropWhile f t
             unless (T.null t') $
               put False
             return $ Str t'
           Space ->
             if f ' '
                then return $ Str ""
                else do
                  put False
                  return Space
           _ -> return x
       else return x


dropTextWhileEnd' :: (Char -> Bool) -> Inlines -> Inlines
dropTextWhileEnd' f ils =
  getReverse $ evalState (walkM go $ Reverse ils) True
 where
  go x = do
    atEnd <- get
    if atEnd
       then
         case x of
           Str t -> do
             let t' = T.dropWhileEnd f t
             unless (T.null t') $
               put False
             return $ Str t'
           Space | f ' ' -> do
             return $ Str ""
           _ -> return x
       else return x

-- taken from Text.Pandoc.Shared:

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> T.Text
stringify = query go . walk (unNote . unQuote)
 where
  go :: Inline -> T.Text
  go Space                                       = " "
  go SoftBreak                                   = " "
  go (Str x)                                     = x
  go (Code _ x)                                  = x
  go (Math _ x)                                  = x
  go (RawInline (Format "html") (T.unpack -> ('<':'b':'r':_)))
                                                 = " " -- see #2105
  go LineBreak                                   = " "
  go _                                           = ""

  unNote :: Inline -> Inline
  unNote (Note _) = Str ""
  unNote x        = x

  unQuote :: Inline -> Inline
  unQuote (Quoted SingleQuote xs) =
    Span ("",[],[]) (Str "\8216" : xs ++ [Str "\8217"])
  unQuote (Quoted DoubleQuote xs) =
    Span ("",[],[]) (Str "\8220" : xs ++ [Str "\8221"])
  unQuote x = x


caseTransform :: (CaseTransformState -> Text -> Text)
              -> Inlines
              -> Inlines
caseTransform f x =
  evalState (caseTransform' f x) Start


-- custom traversal which does not descend into
-- SmallCaps, Superscript, Subscript, Span "nocase" (implicit nocase)
caseTransform' :: (CaseTransformState -> Text -> Text)
               -> Inlines
               -> State CaseTransformState Inlines
caseTransform' f ils =
  case Seq.viewr (unMany ils) of
    xs Seq.:> Str t | not (Seq.null xs)
                    , not (hasWordBreak t) -> do
        xs' <- mapM go xs
        st <- get
        when (st == AfterWordEnd || st == StartSentence || st == Start) $
          put BeforeLastWord
        x' <- go (Str t)
        return $ Many $ xs' Seq.|> x'
    _ -> mapM go ils
 where
  go (Str t) = Str . mconcat <$> mapM g (splitUp t)
  go Space = Space <$ g " "
  go (SmallCaps zs) = return' $ SmallCaps zs
  go (Superscript zs) = return' $ Superscript zs
  go (Subscript zs) = return' $ Subscript zs
  go (Span attr@(_,classes,_) zs)
      | "nocase" `elem` classes = do
            st <- get
            case st of
              AfterWordChar | classes == ["nocase"]
                   -- we need to apply g to update the state:
                -> return' $ Span nullAttr zs
              _ -> return' $ Span attr zs
      | otherwise = Span attr <$> mapM go zs
  go (Emph zs) = Emph <$> mapM go zs
  go (Underline zs) = Underline <$> mapM go zs
  go (Strong zs) = Strong <$> mapM go zs
  go (Strikeout zs) = Strikeout <$> mapM go zs
  go (Quoted qt zs) = Quoted qt <$> mapM go zs
  go (Cite cs zs) = Cite cs <$> mapM go zs
  go (Link attr zs t) = (\x -> Link attr x t) <$> mapM go zs
  go (Image attr zs t) = (\x -> Image attr x t) <$> mapM go zs
  go x = return x

  -- we need to apply g to update the state:
  return' x = x <$ g (query fromStr x)

  fromStr (Str t) = t
  fromStr _ = mempty

  g :: Text -> State CaseTransformState Text
  g t = do
    st <- get
    put $ case T.unsnoc t of
            Nothing -> st
            Just (_,c)
              | c == '.' || c == '?' || c == '!' || c == ':' ->
                AfterSentenceEndingPunctuation
              | isAlphaNum c -> AfterWordChar
              | isSpace c
              , st == AfterSentenceEndingPunctuation -> StartSentence
              | isWordBreak c -> AfterWordEnd
              | otherwise -> AfterOtherPunctuation
    return $
      if T.all isAlphaNum t
         then f st t
         else t
  isWordBreak '-' = True
  isWordBreak '/' = True
  isWordBreak '\x2013' = True
  isWordBreak '\x2014' = True
  isWordBreak c = isSpace c
  hasWordBreak = T.any isWordBreak
  splitUp = T.groupBy sameType
  sameType c d =
    -- note that non-English characters get treated differently
    -- by titlecase transformation
    (isAscii c && isAlphaNum c && isAscii d && isAlphaNum d) ||
    (not (isAscii c) && isAlphaNum c && not (isAscii d) && isAlphaNum d) ||
    (isSpace c && isSpace d)
