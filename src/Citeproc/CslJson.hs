{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
-- | CSL JSON is the structured text format defined in
-- <https://citeproc-js.readthedocs.io/en/latest/csl-json/markup.html>.
-- It is used to represent formatted text inside CSL JSON bibliographies.
-- For the most part it is a subset of HTML, with some special
-- features like smart quote parsing.  This module defines a parser
-- and a renderer for this format, as well as 'CiteprocOutput' and
-- other typeclass instances.
module Citeproc.CslJson
  ( CslJson(..)
  , cslJsonToJson
  , renderCslJson
  , parseCslJson
  )
where

--  represent and parse CSL JSON pseudo-html
--  https://citeproc-js.readthedocs.io/en/latest/csl-json/markup.html
--  Supported:
--  <i>italics</i>  -- will flip-flop
--  <b>bold</b>     -- will flip-flop
--  <span style="font-variant:small-caps;">...</span> -- ill flip-flop
--  <sup>..</sup>
--  <sub>..</sub>
--  <span class="nocase">..</span>  -- suppress case transformations


import Citeproc.Types
import Citeproc.Locale (lookupQuotes)
import Citeproc.CaseTransform
import Data.Ord ()
import Data.Char (isAlphaNum, isSpace, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Attoparsec.Text as P
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object)
import Control.Monad.Trans.State
import Control.Monad (guard, when)
import Control.Applicative ((<|>))
import Data.Generics.Uniplate.Direct

data CslJson a =
     CslText a
   | CslEmpty
   | CslConcat (CslJson a) (CslJson a)
   | CslQuoted (CslJson a)
   | CslItalic (CslJson a)
   | CslNormal (CslJson a)
   | CslBold   (CslJson a)
   | CslUnderline (CslJson a)
   | CslNoDecoration (CslJson a)
   | CslSmallCaps (CslJson a)
   | CslBaseline  (CslJson a)
   | CslSup       (CslJson a)
   | CslSub       (CslJson a)
   | CslNoCase    (CslJson a)
   | CslDiv Text  (CslJson a)
   | CslLink Text (CslJson a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (CslJson a) where
  (CslConcat x y) <> z = x <> (y <> z)
  CslEmpty <> x = x
  x <> CslEmpty = x
  x <> y = CslConcat x y

instance Monoid (CslJson a) where
  mempty = CslEmpty
  mappend = (<>)

instance FromJSON (CslJson Text) where
  parseJSON = fmap (parseCslJson mempty) . parseJSON

instance ToJSON (CslJson Text) where
  toJSON = toJSON . renderCslJson False mempty

instance Uniplate (CslJson a) where
  uniplate (CslText x)         = plate CslText |- x
  uniplate (CslEmpty)          = plate CslEmpty
  uniplate (CslConcat x y)     = plate CslConcat |* x |* y
  uniplate (CslQuoted x)       = plate CslQuoted |* x
  uniplate (CslItalic x)       = plate CslItalic |* x
  uniplate (CslNormal x)       = plate CslNormal |* x
  uniplate (CslBold x)         = plate CslBold |* x
  uniplate (CslUnderline x)    = plate CslUnderline |* x
  uniplate (CslNoDecoration x) = plate CslNoDecoration |* x
  uniplate (CslSmallCaps x)    = plate CslSmallCaps |* x
  uniplate (CslBaseline x)     = plate CslBaseline |* x
  uniplate (CslSup x)          = plate CslSup |* x
  uniplate (CslSub x)          = plate CslSub |* x
  uniplate (CslNoCase x)       = plate CslNoCase |* x
  uniplate (CslDiv t x)        = plate CslDiv |- t |* x
  uniplate (CslLink t x)        = plate CslLink |- t |* x

instance Biplate (CslJson a) (CslJson a) where
  biplate = plateSelf

instance CiteprocOutput (CslJson Text) where
  toText                = fold
  fromText              = parseCslJson mempty
  dropTextWhile         = dropTextWhile'
  dropTextWhileEnd      = dropTextWhileEnd'
  addFontVariant x      =
    case x of
      NormalVariant    -> id
      SmallCapsVariant -> CslSmallCaps
  addFontStyle x        =
    case x of
      NormalFont       -> CslNormal
      ItalicFont       -> CslItalic
      ObliqueFont      -> CslItalic
  addFontWeight x       =
    case x of
      NormalWeight     -> id
      LightWeight      -> id
      BoldWeight       -> CslBold
  addTextDecoration x   =
    case x of
      NoDecoration        -> CslNoDecoration
      UnderlineDecoration -> CslUnderline
  addVerticalAlign x    =
    case x of
      BaselineAlign    -> CslBaseline
      SubAlign         -> CslSub
      SupAlign         -> CslSup
  addTextCase mblang x =
    case x of
      Lowercase        -> caseTransform mblang withLowercaseAll
      Uppercase        -> caseTransform mblang withUppercaseAll
      CapitalizeFirst  -> caseTransform mblang withCapitalizeFirst
      CapitalizeAll    -> caseTransform mblang withCapitalizeWords
      SentenceCase     -> caseTransform mblang withSentenceCase
      TitleCase        -> caseTransform mblang withTitleCase
  addDisplay x          =
    case x of
      DisplayBlock       -> CslDiv "block"
      DisplayLeftMargin  -> CslDiv "left-margin"
      DisplayRightInline -> CslDiv "right-inline"
      DisplayIndent      -> CslDiv "indent"
  addQuotes             = CslQuoted
  inNote                = id -- no-op
  movePunctuationInsideQuotes
                        = punctuationInsideQuotes
  mapText f             = runIdentity . traverse (return . f)
  addHyperlink url x    = CslLink url x
  localizeQuotes        = convertQuotes

dropTextWhile' :: (Char -> Bool) -> CslJson Text -> CslJson Text
dropTextWhile' f x = evalState (traverse g x) False
  where
   g t = do
     pastFirst <- get
     if pastFirst
        then return t
        else do
          put True
          return (T.dropWhile f t)

dropTextWhileEnd' :: (Char -> Bool) -> CslJson Text -> CslJson Text
dropTextWhileEnd' f el =
  case el of
     CslEmpty -> CslEmpty
     CslText t -> CslText (T.dropWhileEnd f t)
     CslConcat x y -> CslConcat x (dropTextWhileEnd' f y)
     CslQuoted x -> CslQuoted (dropTextWhileEnd' f x)
     CslItalic x -> CslItalic (dropTextWhileEnd' f x)
     CslNormal x -> CslNormal (dropTextWhileEnd' f x)
     CslBold x -> CslBold (dropTextWhileEnd' f x)
     CslUnderline x -> CslUnderline (dropTextWhileEnd' f x)
     CslNoDecoration x -> CslNoDecoration (dropTextWhileEnd' f x)
     CslSmallCaps x -> CslSmallCaps (dropTextWhileEnd' f x)
     CslBaseline x -> CslBaseline (dropTextWhileEnd' f x)
     CslSub x -> CslSub (dropTextWhileEnd' f x)
     CslSup x -> CslSup (dropTextWhileEnd' f x)
     CslNoCase x -> CslNoCase (dropTextWhileEnd' f x)
     CslDiv t x -> CslDiv t (dropTextWhileEnd' f x)
     CslLink t x -> CslLink t (dropTextWhileEnd' f x)

parseCslJson :: Locale -> Text -> CslJson Text
parseCslJson locale t =
  case P.parseOnly
         (P.many' (pCslJson locale) <* P.endOfInput) t of
    Left _   -> CslText t
    Right xs -> mconcat xs

pCslJson :: Locale -> P.Parser (CslJson Text)
pCslJson locale = P.choice
  [ pCslText
  , pCslQuoted
  , pCslItalic
  , pCslBold
  , pCslUnderline
  , pCslNoDecoration
  , pCslSmallCaps
  , pCslSup
  , pCslSub
  , pCslBaseline
  , pCslNoCase
  , pCslSymbol
  ]
 where
  ((outerOpenQuote, outerCloseQuote), (innerOpenQuote, innerCloseQuote)) =
     lookupQuotes locale
  isSpecialChar c = c == '<' || c == '>' || c == '\'' || c == '"' ||
       c == '’' || (not (isAscii c) && (isSuperscriptChar c || isQuoteChar c))
  isQuoteChar = P.inClass
       (T.unpack (outerOpenQuote <> outerCloseQuote <>
                 innerOpenQuote <> innerCloseQuote))
  isSuperscriptChar = P.inClass superscriptChars
  isApostrophe '\'' = True
  isApostrophe '’'  = True
  isApostrophe _    = False
  pCsl = pCslJson locale
  notFollowedBySpace =
    P.peekChar' >>= guard . not . isSpaceChar
  isSpaceChar = P.inClass [' ','\t','\n','\r']
  pOpenQuote = (("\"" <$ P.char '"')
                <|> ("'" <$ P.char '\'')
                <|> (outerCloseQuote <$ P.string outerOpenQuote)
                <|> (innerCloseQuote <$ P.string innerOpenQuote))
                 <* notFollowedBySpace
  pSpace = P.skipWhile isSpaceChar
  pCslText = CslText . addNarrowSpace <$>
    (  do t <- P.takeWhile1 (\c -> isAlphaNum c && not (isSpecialChar c))
          -- apostrophe
          P.option t $ do _ <- P.satisfy isApostrophe
                          t' <- P.takeWhile1 isAlphaNum
                          return (t <> "’" <> t')
    <|>
      (P.takeWhile1 (\c -> not (isAlphaNum c || isSpecialChar c))) )
  pCslQuoted = CslQuoted <$>
    do cl <- pOpenQuote
       mbc <- peekChar
       case mbc of
         Just c  | T.singleton c == cl -> fail "unexpected close quote"
         _ -> return ()
       mconcat <$> P.manyTill' pCsl (P.string cl)
  pCslSymbol = do
    c <- P.satisfy isSpecialChar
    return $
       if isApostrophe c
          then CslText "’"
          else charToSup c
  pCslItalic = CslItalic . mconcat <$>
    (P.string "<i>" *> P.manyTill' pCsl (P.string "</i>"))
  pCslBold = CslBold . mconcat <$>
    (P.string "<b>" *> P.manyTill' pCsl (P.string "</b>"))
  pCslUnderline = CslUnderline . mconcat <$>
    (P.string "<u>" *> P.manyTill' pCsl (P.string "</u>"))
  pCslNoDecoration = CslNoDecoration . mconcat <$>
    (P.string "<span" *> pSpace *>
     P.string "class=\"nodecor\"" *> pSpace *> P.char '>' *>
     P.manyTill' pCsl (P.string "</span>"))
  pCslSup = CslSup . mconcat <$>
    (P.string "<sup>" *> P.manyTill' pCsl (P.string "</sup>"))
  pCslSub = CslSub . mconcat <$>
    (P.string "<sub>" *> P.manyTill' pCsl (P.string "</sub>"))
  pCslBaseline = CslBaseline . mconcat <$>
    (P.string "<span" *> pSpace *> P.string "style=\"baseline\">" *>
      P.manyTill' pCsl (P.string "</span>"))
  pCslSmallCaps = CslSmallCaps . mconcat <$>
    ((P.string "<span" *> pSpace *>
      P.string "style=\"font-variant:" *> pSpace *>
      P.string "small-caps;" *> pSpace *> P.char '"' *>
      pSpace *> P.char '>' *> P.manyTill' pCsl (P.string "</span>"))
    <|>
     (P.string "<sc>" *> P.manyTill' pCsl (P.string "</sc>")))
  pCslNoCase = CslNoCase . mconcat <$>
    (P.string "<span" *> pSpace *>
     P.string "class=\"nocase\"" *> pSpace *> P.char '>' *>
     P.manyTill' pCsl (P.string "</span>"))
  addNarrowSpace =
    T.replace " ;" "\x202F;" .
    T.replace " ?" "\x202F?" .
    T.replace " !" "\x202F!" .
    T.replace " »" "\x202F»" .
    T.replace "« " "«\x202F"

data RenderContext =
  RenderContext
  { useOuterQuotes  :: Bool
  , useItalics      :: Bool
  , useBold         :: Bool
  , useSmallCaps    :: Bool
  } deriving (Show, Eq)

-- | Render 'CslJson' as 'Text'.  Set the first parameter to True
-- when rendering HTML output (so that entities are escaped).
-- Set it to False when rendering for CSL JSON bibliographies.
renderCslJson :: Bool          -- ^ Escape < > & using entities
              -> Locale        -- ^ Locale (used for quote styles)
              -> CslJson Text  -- ^ CslJson to render
              -> Text
renderCslJson useEntities locale =
  go (RenderContext True True True True)
 where
  (outerQuotes, innerQuotes) = lookupQuotes locale
  go :: RenderContext -> CslJson Text -> Text
  go ctx el =
    case el of
      CslText t -> escape t
      CslEmpty -> mempty
      CslConcat x y -> go ctx x <> go ctx y
      CslQuoted x
        | useOuterQuotes ctx
          -> fst outerQuotes <>
             go ctx{ useOuterQuotes = False } x <>
             snd outerQuotes
        | otherwise
          -> fst innerQuotes <>
             go ctx{ useOuterQuotes = True } x <>
             snd innerQuotes
      CslNormal x
        | useItalics ctx -> go ctx x
        | otherwise      -> "<span style=\"font-style:normal;\">" <>
                              go ctx x <> "</span>"
      CslItalic x
        | useItalics ctx -> "<i>" <> go ctx{ useItalics = False } x <> "</i>"
        | otherwise -> "<span style=\"font-style:normal;\">" <>
                          go ctx{ useItalics = True } x <> "</span>"
      CslBold x
        | useBold ctx -> "<b>" <> go ctx{ useBold = False } x <> "</b>"
        | otherwise -> "<span style=\"font-weight:normal;\">" <>
                          go ctx{ useBold = True } x <> "</span>"
      CslUnderline x -> "<u>" <> go ctx x <> "</u>"
      CslNoDecoration x -> "<span style=\"" <>
                           (if useSmallCaps ctx
                               then ""
                               else "font-variant:normal;") <>
                           (if useBold ctx
                               then ""
                               else "font-weight:normal;") <>
                           (if useItalics ctx
                               then ""
                               else "font-style:normal;") <>
                           "\">" <> go ctx x <> "</span>"
      CslSmallCaps x
        | useSmallCaps ctx -> "<span style=\"font-variant:small-caps;\">"
                                <> go ctx{ useSmallCaps = False } x <>
                                "</span>"
        | otherwise -> "<span style=\"font-variant:normal;\">" <>
                          go ctx{ useSmallCaps = True } x <> "</span>"
      CslSup x -> "<sup>" <> go ctx x <> "</sup>"
      CslSub x -> "<sub>" <> go ctx x <> "</sub>"
      CslBaseline x -> "<span style=\"baseline\">" <> go ctx x <> "</span>"
      CslDiv t x -> "<div class=\"csl-" <> t <> "\">" <> go ctx x <> "</div>"
      CslLink t x -> "<a href=\"" <> t <> "\">" <> go ctx x <> "</a>"
      CslNoCase x -> go ctx x -- nocase is just for internal purposes
  escape t
    | useEntities
      = case T.findIndex (\c -> c == '<' || c == '>' || c == '&') t of
               Just _ -> T.replace "<" "&#60;" .
                         T.replace ">" "&#62;" .
                         T.replace "&" "&#38;" $ t
               Nothing -> t
    | otherwise = t

-- localized quotes
convertQuotes :: Locale -> CslJson Text -> CslJson Text
convertQuotes locale = go True
 where
  (outerQuotes, innerQuotes) = lookupQuotes locale

  go useOuter el =
    case el of
      CslConcat x y -> go useOuter x <> go useOuter y
      CslQuoted x
        | useOuter
          -> CslText (fst outerQuotes) <>
             go (not useOuter) x <>
             CslText (snd outerQuotes)
        | otherwise
          -> CslText (fst innerQuotes) <>
             go (not useOuter) x <>
             CslText (snd innerQuotes)
      CslNormal x -> CslNormal $ go useOuter x
      CslItalic x -> CslItalic $ go useOuter x
      CslBold x -> CslBold $ go useOuter x
      CslUnderline x -> CslUnderline $ go useOuter x
      CslNoDecoration x -> CslNoDecoration $ go useOuter x
      CslSmallCaps x -> CslSmallCaps $ go useOuter x
      CslSup x -> CslSup $ go useOuter x
      CslSub x -> CslSub $ go useOuter x
      CslBaseline x -> CslBaseline $ go useOuter x
      CslDiv t x -> CslDiv t $ go useOuter x
      CslNoCase x -> CslNoCase $ go useOuter x
      x -> x


cslJsonToJson :: CslJson Text -> [Value]
cslJsonToJson = go (RenderContext True True True True)
 where
  isString (String _) = True
  isString _ = False
  consolidateStrings :: [Value] -> [Value]
  consolidateStrings [] = []
  consolidateStrings (String t : rest) =
    let (xs,ys) = span isString rest
     in String (t <> mconcat [t' |  String t' <- xs]) :
        consolidateStrings ys
  consolidateStrings (x : rest) =
    x : consolidateStrings rest
  go :: RenderContext -> CslJson Text -> [Value]
  go ctx el = consolidateStrings $
    case el of
      CslText t -> [String t]
      CslEmpty -> []
      CslConcat x CslEmpty -> go ctx x
      CslConcat (CslConcat x y) z -> go ctx (CslConcat x (CslConcat y z))
      CslConcat x y -> go ctx x <> go ctx y
      CslQuoted x -> go ctx x  -- should be localized already
      CslNormal x
        | useItalics ctx -> go ctx x
        | otherwise      -> [ object
                               [ ("format", "no-italics")
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslItalic x
        | useItalics ctx -> [ object
                               [ ("format", "italics")
                               , ("contents", toJSON $
                                    go ctx{ useItalics = False } x)
                               ]
                            ]
        | otherwise      -> [ object
                               [ ("format", "no-italics")
                               , ("contents", toJSON $
                                    go ctx{ useItalics = False } x)
                               ]
                            ]
      CslBold x
        | useItalics ctx -> [ object
                               [ ("format", "bold")
                               , ("contents", toJSON $
                                    go ctx{ useBold = False } x)
                               ]
                            ]
        | otherwise      -> [ object
                               [ ("format", "no-bold")
                               , ("contents", toJSON $
                                    go ctx{ useBold = False } x)
                               ]
                            ]
      CslUnderline x     -> [ object
                               [ ("format", "underline")
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslNoDecoration x -> [ object
                               [ ("format", "no-decoration")
                               , ("contents", toJSON $ go ctx x)
                               ]
                           ]
      CslSmallCaps x
        | useSmallCaps ctx -> [ object
                               [ ("format", "small-caps")
                               , ("contents", toJSON $
                                    go ctx{ useSmallCaps = False } x)
                               ]
                            ]
        | otherwise      -> [ object
                               [ ("format", "no-small-caps")
                               , ("contents", toJSON $
                                    go ctx{ useSmallCaps = False } x)
                               ]
                            ]
      CslSup x           -> [ object
                               [ ("format", "superscript")
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslSub x           -> [ object
                               [ ("format", "subscript")
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslBaseline x      -> [ object
                               [ ("format", "baseline")
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslDiv t x         -> [ object
                               [ ("format", "div")
                               , ("class", toJSON $ "csl-" <> t)
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslLink t x        -> [ object
                               [ ("format", "link")
                               , ("target", toJSON $ t)
                               , ("contents", toJSON $ go ctx x)
                               ]
                            ]
      CslNoCase x -> go ctx x -- nocase is just for internal purposes


-- custom traversal which does not descend into
-- CslSmallCaps, Baseline, SUp, Sub, or NoCase (implicit nocase)
caseTransform' :: (CaseTransformState -> Text -> Text)
               -> Int -- level in hierarchy
               -> CslJson Text
               -> State CaseTransformState (CslJson Text)
caseTransform' f lev el =
  case el of
     CslText x         -> CslText . mconcat <$> mapM g (splitUp x)
     CslConcat x y     -> do
       x' <- caseTransform' f lev x
       let lastWord = lev == 0 && not (hasWordBreak y)
       st <- get
       when (lastWord &&
             (st == AfterWordEnd || st == StartSentence || st == Start)) $
        put BeforeLastWord
       y' <- caseTransform' f lev y
       return $ CslConcat x' y'
     CslQuoted x       -> CslQuoted <$> caseTransform' f (lev + 1) x
     CslItalic x       -> CslItalic <$> caseTransform' f (lev + 1) x
     CslNormal x       -> CslNormal <$> caseTransform' f (lev + 1) x
     CslBold   x       -> CslBold   <$> caseTransform' f (lev + 1) x
     CslUnderline x    -> CslUnderline <$> caseTransform' f (lev + 1) x
     CslNoDecoration _ -> return' el
     CslSmallCaps _    -> return' el
     CslBaseline _     -> return' el
     CslSub _          -> return' el
     CslSup _          -> return' el
     CslNoCase _       -> return' el
     CslDiv _ _        -> return' el
     CslLink _ _       -> return' el
     CslEmpty          -> return' el
 where
  -- we need to apply g to update the state:
  return' x = x <$ g (toText x)

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
              | otherwise -> st
    return $
      if T.all isAlphaNum t
         then f st t
         else t
  isWordBreak '-' = True
  isWordBreak '/' = True
  isWordBreak '\x2013' = True
  isWordBreak '\x2014' = True
  isWordBreak c = isSpace c
  hasWordBreak = any (T.any isWordBreak)
  splitUp = T.groupBy sameType
  sameType c d =
    (isAlphaNum c && isAlphaNum d) || (isSpace c && isSpace d)

caseTransform :: Maybe Lang
              -> CaseTransformer
              -> CslJson Text
              -> CslJson Text
caseTransform mblang f x =
  evalState (caseTransform' (unCaseTransformer f mblang) 0 x) Start

punctuationInsideQuotes :: CslJson Text -> CslJson Text
punctuationInsideQuotes = go
 where
  startsWithMovable t =
    case T.uncons t of
      Just (c,_) -> c == '.' || c == ','
      Nothing    -> False
  go el =
    case el of
      CslConcat CslEmpty x -> go x
      CslConcat x CslEmpty -> go x
      CslConcat (CslQuoted x) y ->
         case go y of
           (CslText t) | startsWithMovable t
             -> CslQuoted (go (x <> CslText (T.take 1 t)))
               <> CslText (T.drop 1 t)
           (CslConcat (CslText t) z) | startsWithMovable t
             -> CslQuoted (go (x <> CslText (T.take 1 t))) <>
                 CslText (T.drop 1 t) <> z
           z                      -> CslQuoted x <> z
      CslConcat (CslConcat x y) z -> go (CslConcat x (CslConcat y z))
      CslConcat x y               -> go x <> go y
      CslQuoted x                 -> CslQuoted (go x)
      CslItalic x                 -> CslItalic (go x)
      CslNormal x                 -> CslNormal (go x)
      CslBold x                   -> CslBold (go x)
      CslUnderline x              -> CslUnderline (go x)
      CslNoDecoration x           -> CslNoDecoration (go x)
      CslSmallCaps x              -> CslSmallCaps (go x)
      CslSup x                    -> CslSup (go x)
      CslSub x                    -> CslSub (go x)
      CslBaseline x               -> CslBaseline (go x)
      CslNoCase x                 -> CslNoCase (go x)
      CslDiv t x                  -> CslDiv t (go x)
      CslLink t x                 -> CslLink t (go x)
      CslText t                   -> CslText t
      CslEmpty                    -> CslEmpty

superscriptChars :: [Char]
superscriptChars =
  [ '\x00AA'
  , '\x00B2'
  , '\x00B3'
  , '\x00B9'
  , '\x00BA'
  , '\x02B0'
  , '\x02B1'
  , '\x02B2'
  , '\x02B3'
  , '\x02B4'
  , '\x02B5'
  , '\x02B6'
  , '\x02B7'
  , '\x02B8'
  , '\x02E0'
  , '\x02E1'
  , '\x02E2'
  , '\x02E3'
  , '\x02E4'
  , '\x1D2C'
  , '\x1D2D'
  , '\x1D2E'
  , '\x1D30'
  , '\x1D31'
  , '\x1D32'
  , '\x1D33'
  , '\x1D34'
  , '\x1D35'
  , '\x1D36'
  , '\x1D37'
  , '\x1D38'
  , '\x1D39'
  , '\x1D3A'
  , '\x1D3C'
  , '\x1D3D'
  , '\x1D3E'
  , '\x1D3F'
  , '\x1D40'
  , '\x1D41'
  , '\x1D42'
  , '\x1D43'
  , '\x1D44'
  , '\x1D45'
  , '\x1D46'
  , '\x1D47'
  , '\x1D48'
  , '\x1D49'
  , '\x1D4A'
  , '\x1D4B'
  , '\x1D4C'
  , '\x1D4D'
  , '\x1D4F'
  , '\x1D50'
  , '\x1D51'
  , '\x1D52'
  , '\x1D53'
  , '\x1D54'
  , '\x1D55'
  , '\x1D56'
  , '\x1D57'
  , '\x1D58'
  , '\x1D59'
  , '\x1D5A'
  , '\x1D5B'
  , '\x1D5C'
  , '\x1D5D'
  , '\x1D5E'
  , '\x1D5F'
  , '\x1D60'
  , '\x1D61'
  , '\x2070'
  , '\x2071'
  , '\x2074'
  , '\x2075'
  , '\x2076'
  , '\x2077'
  , '\x2078'
  , '\x2079'
  , '\x207A'
  , '\x207B'
  , '\x207C'
  , '\x207D'
  , '\x207E'
  , '\x207F'
  , '\x2120'
  , '\x2122'
  , '\x3192'
  , '\x3193'
  , '\x3194'
  , '\x3195'
  , '\x3196'
  , '\x3197'
  , '\x3198'
  , '\x3199'
  , '\x319A'
  , '\x319B'
  , '\x319C'
  , '\x319D'
  , '\x319E'
  , '\x319F'
  , '\x02C0'
  , '\x02C1'
  , '\x06E5'
  , '\x06E6'
  ]

charToSup :: Char -> CslJson Text
charToSup c =
  case c of
    '\x00AA' -> CslSup (CslText "\x0061")
    '\x00B2' -> CslSup (CslText "\x0032")
    '\x00B3' -> CslSup (CslText "\x0033")
    '\x00B9' -> CslSup (CslText "\x0031")
    '\x00BA' -> CslSup (CslText "\x006F")
    '\x02B0' -> CslSup (CslText "\x0068")
    '\x02B1' -> CslSup (CslText "\x0266")
    '\x02B2' -> CslSup (CslText "\x006A")
    '\x02B3' -> CslSup (CslText "\x0072")
    '\x02B4' -> CslSup (CslText "\x0279")
    '\x02B5' -> CslSup (CslText "\x027B")
    '\x02B6' -> CslSup (CslText "\x0281")
    '\x02B7' -> CslSup (CslText "\x0077")
    '\x02B8' -> CslSup (CslText "\x0079")
    '\x02E0' -> CslSup (CslText "\x0263")
    '\x02E1' -> CslSup (CslText "\x006C")
    '\x02E2' -> CslSup (CslText "\x0073")
    '\x02E3' -> CslSup (CslText "\x0078")
    '\x02E4' -> CslSup (CslText "\x0295")
    '\x1D2C' -> CslSup (CslText "\x0041")
    '\x1D2D' -> CslSup (CslText "\x00C6")
    '\x1D2E' -> CslSup (CslText "\x0042")
    '\x1D30' -> CslSup (CslText "\x0044")
    '\x1D31' -> CslSup (CslText "\x0045")
    '\x1D32' -> CslSup (CslText "\x018E")
    '\x1D33' -> CslSup (CslText "\x0047")
    '\x1D34' -> CslSup (CslText "\x0048")
    '\x1D35' -> CslSup (CslText "\x0049")
    '\x1D36' -> CslSup (CslText "\x004A")
    '\x1D37' -> CslSup (CslText "\x004B")
    '\x1D38' -> CslSup (CslText "\x004C")
    '\x1D39' -> CslSup (CslText "\x004D")
    '\x1D3A' -> CslSup (CslText "\x004E")
    '\x1D3C' -> CslSup (CslText "\x004F")
    '\x1D3D' -> CslSup (CslText "\x0222")
    '\x1D3E' -> CslSup (CslText "\x0050")
    '\x1D3F' -> CslSup (CslText "\x0052")
    '\x1D40' -> CslSup (CslText "\x0054")
    '\x1D41' -> CslSup (CslText "\x0055")
    '\x1D42' -> CslSup (CslText "\x0057")
    '\x1D43' -> CslSup (CslText "\x0061")
    '\x1D44' -> CslSup (CslText "\x0250")
    '\x1D45' -> CslSup (CslText "\x0251")
    '\x1D46' -> CslSup (CslText "\x1D02")
    '\x1D47' -> CslSup (CslText "\x0062")
    '\x1D48' -> CslSup (CslText "\x0064")
    '\x1D49' -> CslSup (CslText "\x0065")
    '\x1D4A' -> CslSup (CslText "\x0259")
    '\x1D4B' -> CslSup (CslText "\x025B")
    '\x1D4C' -> CslSup (CslText "\x025C")
    '\x1D4D' -> CslSup (CslText "\x0067")
    '\x1D4F' -> CslSup (CslText "\x006B")
    '\x1D50' -> CslSup (CslText "\x006D")
    '\x1D51' -> CslSup (CslText "\x014B")
    '\x1D52' -> CslSup (CslText "\x006F")
    '\x1D53' -> CslSup (CslText "\x0254")
    '\x1D54' -> CslSup (CslText "\x1D16")
    '\x1D55' -> CslSup (CslText "\x1D17")
    '\x1D56' -> CslSup (CslText "\x0070")
    '\x1D57' -> CslSup (CslText "\x0074")
    '\x1D58' -> CslSup (CslText "\x0075")
    '\x1D59' -> CslSup (CslText "\x1D1D")
    '\x1D5A' -> CslSup (CslText "\x026F")
    '\x1D5B' -> CslSup (CslText "\x0076")
    '\x1D5C' -> CslSup (CslText "\x1D25")
    '\x1D5D' -> CslSup (CslText "\x03B2")
    '\x1D5E' -> CslSup (CslText "\x03B3")
    '\x1D5F' -> CslSup (CslText "\x03B4")
    '\x1D60' -> CslSup (CslText "\x03C6")
    '\x1D61' -> CslSup (CslText "\x03C7")
    '\x2070' -> CslSup (CslText "\x0030")
    '\x2071' -> CslSup (CslText "\x0069")
    '\x2074' -> CslSup (CslText "\x0034")
    '\x2075' -> CslSup (CslText "\x0035")
    '\x2076' -> CslSup (CslText "\x0036")
    '\x2077' -> CslSup (CslText "\x0037")
    '\x2078' -> CslSup (CslText "\x0038")
    '\x2079' -> CslSup (CslText "\x0039")
    '\x207A' -> CslSup (CslText "\x002B")
    '\x207B' -> CslSup (CslText "\x2212")
    '\x207C' -> CslSup (CslText "\x003D")
    '\x207D' -> CslSup (CslText "\x0028")
    '\x207E' -> CslSup (CslText "\x0029")
    '\x207F' -> CslSup (CslText "\x006E")
    '\x2120' -> CslSup (CslText "\x0053\x004D")
    '\x2122' -> CslSup (CslText "\x0054\x004D")
    '\x3192' -> CslSup (CslText "\x4E00")
    '\x3193' -> CslSup (CslText "\x4E8C")
    '\x3194' -> CslSup (CslText "\x4E09")
    '\x3195' -> CslSup (CslText "\x56DB")
    '\x3196' -> CslSup (CslText "\x4E0A")
    '\x3197' -> CslSup (CslText "\x4E2D")
    '\x3198' -> CslSup (CslText "\x4E0B")
    '\x3199' -> CslSup (CslText "\x7532")
    '\x319A' -> CslSup (CslText "\x4E59")
    '\x319B' -> CslSup (CslText "\x4E19")
    '\x319C' -> CslSup (CslText "\x4E01")
    '\x319D' -> CslSup (CslText "\x5929")
    '\x319E' -> CslSup (CslText "\x5730")
    '\x319F' -> CslSup (CslText "\x4EBA")
    '\x02C0' -> CslSup (CslText "\x0294")
    '\x02C1' -> CslSup (CslText "\x0295")
    '\x06E5' -> CslSup (CslText "\x0648")
    '\x06E6' -> CslSup (CslText "\x064A")
    _        -> CslText $ T.singleton c
