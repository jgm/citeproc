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
import Citeproc.CaseTransform
import Data.Ord ()
import qualified Data.Map as M
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object)
import Control.Monad.Trans.State
import Control.Monad (when)
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
  fromText              = \t -> if T.null t
                                   then CslEmpty
                                   else CslText t
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
      PreserveCase     -> CslNoCase
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
      CslText t
        | Just (c, "") <- T.uncons t
        , Just t' <- M.lookup c superscriptChars
          -> "<sup>" <> t' <> "</sup>"
        | otherwise -> escape t
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
    (isAlphaNum c && isAlphaNum d) || (isSpace c && isSpace d) ||
      (isPunctuation c && isPunctuation d)

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

