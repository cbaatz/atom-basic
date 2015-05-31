{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Web.Atom
    ( makeFeed
    , makeEntry
    , feedXML
    , entryXML
    , XMLGen (..)
    , Feed(..)
    , Entry(..)
    , Source(..)
    , Content(..)
    , Category(..)
    , Generator(..)
    , Person(..)
    , Email(..)
    , Rel(..)
    , Text(..)
    , Link(..)
    , LanguageTag(..)
    , MediaType(..)
    , UTCTime
    , URI(..)
    ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (catMaybes)
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T (decodeUtf8)
import           Data.Time              (UTCTime, formatTime)
import           Network.URI            (URI (..))
import           System.Locale          (defaultTimeLocale)

-- ------------------------
-- Convenience constructors
-- ------------------------

makeFeed :: URI -> Text e -> UTCTime -> Feed e
makeFeed uri title updated = Feed
    { feedId           = uri
    , feedTitle        = title
    , feedUpdated      = updated
    , feedSubtitle     = Nothing
    , feedIcon         = Nothing
    , feedLogo         = Nothing
    , feedRights       = Nothing
    , feedGenerator    = Nothing
    , feedAuthors      = []
    , feedContributors = []
    , feedCategories   = []
    , feedLinks        = []
    , feedEntries      = []
    }

makeEntry :: URI -> Text e -> UTCTime -> Entry e
makeEntry uri title updated = Entry
    { entryId           = uri
    , entryTitle        = title
    , entryUpdated      = updated
    , entryPublished    = Nothing
    , entrySummary      = Nothing
    , entryContent      = Nothing
    , entryRights       = Nothing
    , entrySource       = Nothing
    , entryAuthors      = []
    , entryContributors = []
    , entryCategories   = []
    , entryLinks        = []
    }

-- -------------------------
-- External XML construction
-- -------------------------

feedXML :: XMLGen e node name attr -> Feed e -> e
feedXML xmlgen feed = toXML xmlgen "feed" feed

entryXML :: XMLGen e node name attr -> Entry e -> e
entryXML xmlgen entry = toXML xmlgen "entry" entry

data XMLGen elem node name attr = XMLGen
    { xmlElem     :: name -> [attr] -> [node] -> elem
    , xmlName     :: Maybe T.Text -> T.Text -> name
    , xmlAttr     :: name -> T.Text -> attr
    , xmlTextNode :: T.Text -> node
    , xmlElemNode :: elem -> node
    }

-- ----------
-- Atom Types
-- ----------

data LanguageTag = LanguageTag T.Text deriving (Show, Eq)

data Email = Email T.Text deriving (Show, Eq)

data MediaType = MediaType ByteString deriving (Show, Eq)

data Rel = RelText T.Text | RelURI URI deriving (Eq)

instance Show Rel where
    show (RelText t) = T.unpack t
    show (RelURI u)  = show u

instance IsString MediaType where
    fromString s = MediaType (BC.pack s)

data Text e = TextPlain T.Text
            | TextHTML  T.Text
            | TextXHTML e
            deriving (Show, Eq)

instance IsString (Text e) where
    fromString s = TextPlain (T.pack s)

data Content e = InlinePlainContent  T.Text
               | InlineHTMLContent   T.Text
               | InlineXHTMLContent  e
               | InlineXMLContent    e          (Maybe MediaType)
               | InlineTextContent   T.Text     (Maybe MediaType)
               | InlineBase64Content ByteString (Maybe MediaType)
               | OutOfLineContent    URI        (Maybe MediaType)
               deriving (Show, Eq)

data Person = Person
    { personName  :: T.Text
    , personURI   :: Maybe URI
    , personEmail :: Maybe Email
    } deriving (Show, Eq)

data Generator = Generator
    { generatorName :: T.Text
    , generatorURI  :: Maybe URI
    , version       :: Maybe T.Text
    } deriving (Show, Eq)

data Category = Category
    { categoryTerm   :: T.Text
    , categoryScheme :: Maybe URI
    , categoryLabel  :: Maybe T.Text
    } deriving (Show, Eq)

data Link = Link
    { linkHref     :: URI
    , linkRel      :: Maybe Rel
    , linkType     :: Maybe MediaType
    , linkHrefLang :: Maybe LanguageTag
    , linkTitle    :: Maybe T.Text
    , linkLength   :: Maybe Integer
    } deriving (Show, Eq)

data Feed e = Feed
    { feedId           :: URI
    , feedTitle        :: Text e
    , feedUpdated      :: UTCTime
    , feedSubtitle     :: Maybe (Text e)
    , feedIcon         :: Maybe URI
    , feedLogo         :: Maybe URI
    , feedRights       :: Maybe (Text e)
    , feedGenerator    :: Maybe Generator
    , feedAuthors      :: [Person] -- At least one unless all child entries have at least one author
    , feedContributors :: [Person]
    , feedCategories   :: [Category]
    , feedLinks        :: [Link]
    , feedEntries      :: [Entry e]
    } deriving (Show, Eq)

data Source e = Source
    { sourceId           :: Maybe URI
    , sourceTitle        :: Maybe (Text e)
    , sourceUpdated      :: Maybe UTCTime
    , sourceSubtitle     :: Maybe (Text e)
    , sourceIcon         :: Maybe URI
    , sourceLogo         :: Maybe URI
    , sourceRights       :: Maybe (Text e)
    , sourceGenerator    :: Maybe Generator
    , sourceAuthors      :: [Person] -- At least one unless all child entries have at least one author
    , sourceContributors :: [Person]
    , sourceCategories   :: [Category]
    , sourceLinks        :: [Link]
    } deriving (Show, Eq)

data Entry e = Entry
    { entryId           :: URI
    , entryTitle        :: (Text e)
    , entryUpdated      :: UTCTime
    , entryPublished    :: Maybe UTCTime
    , entrySummary      :: Maybe (Text e)
    , entryContent      :: Maybe (Content e)
    , entryRights       :: Maybe (Text e)
    , entrySource       :: Maybe (Source e)
    , entryAuthors      :: [Person] -- At least one unless source child contains author or parent contains author
    , entryContributors :: [Person]
    , entryCategories   :: [Category]
    , entryLinks        :: [Link] -- Required if no content element
    } deriving (Show, Eq)

-- -------------------------
-- Internal helper functions
-- -------------------------

textShow :: (Show a) => a -> T.Text
textShow = T.pack . show

atomNS :: Maybe T.Text
atomNS = Just "http://www.w3.org/2005/Atom"

media :: XMLGen e n m a -> MediaType -> a
media XMLGen{..} (MediaType m) = xmlAttr (xmlName Nothing "type") (T.decodeUtf8 m)

attr :: XMLGen e n m a -> T.Text -> T.Text -> a
attr XMLGen{..} name value = xmlAttr (xmlName Nothing name) value

-- -------------------------
-- Internal XML construction
-- -------------------------

class ToXML e b where
    toXML :: XMLGen e n m a -> T.Text -> b -> e

instance ToXML e (Feed e) where
    toXML g@XMLGen{..} tag Feed{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          [ toXML g "id"      feedId
          , toXML g "title"   feedTitle
          , toXML g "updated" feedUpdated
          ]
          ++ catMaybes
          [ fmap (toXML g "subtitle")  feedSubtitle
          , fmap (toXML g "icon")      feedIcon
          , fmap (toXML g "logo")      feedLogo
          , fmap (toXML g "rights")    feedRights
          , fmap (toXML g "generator") feedGenerator
          ]
          ++ map (toXML g "author")      feedAuthors
          ++ map (toXML g "contributor") feedContributors
          ++ map (toXML g "category")    feedCategories
          ++ map (toXML g "link")        feedLinks
          ++ map (toXML g "entry")       feedEntries
          )

instance ToXML e (Entry e) where
    toXML g@XMLGen{..} tag Entry{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          [ toXML g "id"      entryId
          , toXML g "title"   entryTitle
          , toXML g "updated" entryUpdated
          ]
          ++ catMaybes
          [ fmap (toXML g "published") entryPublished
          , fmap (toXML g "summary")   entrySummary
          , fmap (toXML g "content")   entryContent
          , fmap (toXML g "rights")    entryRights
          , fmap (toXML g "source")    entrySource
          ]
          ++ map (toXML g "author")      entryAuthors
          ++ map (toXML g "contributor") entryContributors
          ++ map (toXML g "category")    entryCategories
          ++ map (toXML g "link")        entryLinks
          )

instance ToXML e (Source e) where
    toXML g@XMLGen{..} tag Source{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          catMaybes
          [ fmap (toXML g "id")        sourceId
          , fmap (toXML g "title")     sourceTitle
          , fmap (toXML g "updated")   sourceUpdated
          , fmap (toXML g "subtitle")  sourceSubtitle
          , fmap (toXML g "icon")      sourceIcon
          , fmap (toXML g "logo")      sourceLogo
          , fmap (toXML g "rights")    sourceRights
          , fmap (toXML g "generator") sourceGenerator
          ]
          ++ map (toXML g "author")      sourceAuthors
          ++ map (toXML g "contributor") sourceContributors
          ++ map (toXML g "category")    sourceCategories
          ++ map (toXML g "link")        sourceLinks
          )

instance ToXML e (Text e) where
    toXML g@XMLGen{..} tag x = case x of
        TextPlain c -> el [attr g "type" "text"]  [xmlTextNode c]
        TextHTML  c -> el [attr g "type" "html"]  [xmlTextNode c]
        TextXHTML c -> el [attr g "type" "xhtml"] [xmlElemNode c]
      where el = xmlElem (xmlName atomNS tag)

instance ToXML e (Content e) where
    toXML g@XMLGen{..} tag x = case x of
        InlinePlainContent t          -> el [attr g "type" "text"]  [xmlTextNode t]
        InlineHTMLContent t           -> el [attr g "type" "html"]  [xmlTextNode t]
        InlineXHTMLContent xml        -> el [attr g "type" "xhtml"] [xmlElemNode xml]
        InlineTextContent t mmedia    -> el (mediaAttrs mmedia)     [xmlTextNode t]
        InlineXMLContent xml mmedia   -> el (mediaAttrs mmedia)     [xmlElemNode xml]
        InlineBase64Content bs mmedia -> el (mediaAttrs mmedia)     [xmlTextNode (T.decodeUtf8 $ B64.encode bs)]
        OutOfLineContent uri mmedia   -> el (mediaAttrs mmedia ++ [attr g "src" $ textShow uri]) []
      where el = xmlElem (xmlName atomNS tag)
            mediaAttrs = maybe [] (\m -> [media g m])

instance ToXML e Person where
    toXML XMLGen{..} tag (Person name mu me) = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (catMaybes
            [ Just $ el "name" [] [xmlTextNode name]
            , fmap (\u -> el "uri" [] [xmlTextNode $ textShow u]) mu
            , fmap (\(Email e) -> el "email" [] [xmlTextNode e])  me
            ])
      where el n = xmlElem (xmlName atomNS n)

instance ToXML e Category where
    toXML g@XMLGen{..} tag (Category term mscheme mlabel) = el attrs []
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ Just (attr g "term" term)
                , fmap (attr g "scheme" . textShow) mscheme
                , fmap (attr g "label") mlabel
                ]

instance ToXML e Generator where
    toXML g@XMLGen{..} tag (Generator name muri mversion) = el attrs [xmlTextNode name]
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ fmap (attr g "uri" . textShow) muri
                , fmap (attr g "version")        mversion
                ]

instance ToXML e Link where
    toXML g@XMLGen{..} tag (Link href mrel mmedia mlang mtitle mlen) = el attrs []
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ Just . attr g "href" . textShow $ href
                , fmap (attr g "rel" . textShow)    mrel
                , fmap (media g)                    mmedia
                , fmap (\(LanguageTag lang) -> attr g "hreflang" lang) mlang
                , fmap (attr g "title")             mtitle
                , fmap (attr g "length" . textShow) mlen
                ]

instance ToXML e URI where
    toXML XMLGen{..} tag uri = xmlElem
        (xmlName atomNS tag)
        []
        [xmlTextNode (textShow uri)]

instance ToXML e UTCTime where
    toXML XMLGen{..} tag utc = xmlElem
        (xmlName atomNS tag)
        []
        [xmlTextNode (T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utc)]
