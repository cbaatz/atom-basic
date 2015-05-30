{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Atom
    ( toXML
    , makeFeed
    , makeEntry
    , XMLGens (..)
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

makeFeed :: forall e. URI -> Text e -> UTCTime -> Feed e
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

makeEntry :: forall e. URI -> Text e -> UTCTime -> Entry e
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

-- ----------------
-- XML construction
-- ----------------

class ToXML elem a where
    toXML :: forall node name content. XMLGens elem node name content -> a -> elem

instance ToXML e (Feed e) where
    toXML g f = toXMLElem g "feed" f

instance ToXML e (Entry e) where
    toXML g e = toXMLElem g "entry" e

data XMLGens elem node name attr = XMLGens
    { xmlName :: Maybe T.Text -> T.Text -> name
    , xmlElem :: name -> [attr] -> [node] -> elem
    , xmlNode :: elem -> node
    , xmlText :: T.Text -> node
    , xmlAttr :: name -> T.Text -> attr
    }

textShow :: (Show a) => a -> T.Text
textShow = T.pack . show

atomName :: XMLGens e n m a -> T.Text -> m
atomName g n = (xmlName g (Just "http://www.w3.org/2005/Atom") n)

media :: forall e n m a. XMLGens e n m a -> MediaType -> a
media g (MediaType m) = (xmlAttr g) (xmlName g Nothing "type") (T.pack $ BC.unpack m)

attr :: XMLGens e n m a -> T.Text -> T.Text -> a
attr g n v = (xmlAttr g) (xmlName g Nothing n) v

class ToXMLElem e b where
    toXMLElem :: forall n m a. XMLGens e n m a -> T.Text -> b -> e

instance ToXMLElem e (Feed e) where
    toXMLElem g _ f = (xmlElem g) (atomName g "feed") [] (map (xmlNode g) (
        [ toXMLElem g "id"      (feedId f)
        , toXMLElem g "title"   (feedTitle f)
        , toXMLElem g "updated" (feedUpdated f)
        ]
        ++ catMaybes
        [ fmap (toXMLElem g "subtitle")  (feedSubtitle f)
        , fmap (toXMLElem g "icon")      (feedIcon f)
        , fmap (toXMLElem g "logo")      (feedLogo f)
        , fmap (toXMLElem g "rights")    (feedRights f)
        , fmap (toXMLElem g "generator") (feedGenerator f)
        ]
        ++ map (\author   -> toXMLElem g "author"   author)   (feedAuthors f)
        ++ map (\contrib  -> toXMLElem g "contrib"  contrib)  (feedContributors f)
        ++ map (\category -> toXMLElem g "category" category) (feedCategories f)
        ++ map (\link     -> toXMLElem g "link"     link)     (feedLinks f)
        ++ map (\entry    -> toXMLElem g "entry"    entry)    (feedEntries f)
        ))

instance ToXMLElem e (Source e) where
    toXMLElem g tag s = (xmlElem g) (atomName g tag) [] (map (xmlNode g) (
        catMaybes
        [ fmap (toXMLElem g "id")        (sourceId s)
        , fmap (toXMLElem g "title")     (sourceTitle s)
        , fmap (toXMLElem g "updated")   (sourceUpdated s)
        , fmap (toXMLElem g "subtitle")  (sourceSubtitle s)
        , fmap (toXMLElem g "icon")      (sourceIcon s)
        , fmap (toXMLElem g "logo")      (sourceLogo s)
        , fmap (toXMLElem g "rights")    (sourceRights s)
        , fmap (toXMLElem g "generator") (sourceGenerator s)
        ]
        ++ map (\author   -> toXMLElem g "author"   author)   (sourceAuthors s)
        ++ map (\contrib  -> toXMLElem g "contrib"  contrib)  (sourceContributors s)
        ++ map (\category -> toXMLElem g "category" category) (sourceCategories s)
        ++ map (\link     -> toXMLElem g "link"     link)     (sourceLinks s)
        ))

instance ToXMLElem e (Entry e) where
    toXMLElem g _ e = (xmlElem g) (atomName g "entry") [] (map (xmlNode g) (
        [ toXMLElem g "id"       (entryId      e)
        , toXMLElem g "title"    (entryTitle   e)
        , toXMLElem g "updated"  (entryUpdated e)
        ]
        ++ catMaybes
        [ fmap (toXMLElem g "published") (entryPublished e)
        , fmap (toXMLElem g "summary")   (entrySummary   e)
        , fmap (toXMLElem g "content")   (entryContent   e)
        , fmap (toXMLElem g "rights")    (entryRights    e)
        , fmap (toXMLElem g "source")    (entrySource    e)
        ]
        ++ map (\author   -> toXMLElem g "author"   author)   (entryAuthors      e)
        ++ map (\contrib  -> toXMLElem g "contrib"  contrib)  (entryContributors e)
        ++ map (\category -> toXMLElem g "category" category) (entryCategories   e)
        ++ map (\link     -> toXMLElem g "link"     link)     (entryLinks        e)
        ))

instance ToXMLElem e (Text e) where
    toXMLElem g tag (TextPlain x) = (xmlElem g) (atomName g tag) [attr g "type" "text"]  [xmlText g x]
    toXMLElem g tag (TextHTML  x) = (xmlElem g) (atomName g tag) [attr g "type" "html"]  [xmlText g x]
    toXMLElem g tag (TextXHTML x) = (xmlElem g) (atomName g tag) [attr g "type" "xhtml"] [xmlNode g x]

instance ToXMLElem e (Content e) where
    toXMLElem g tag (InlinePlainContent t)          = (xmlElem g) (atomName g tag) [attr g "type" "text"] [xmlText g t]
    toXMLElem g tag (InlineHTMLContent t)           = (xmlElem g) (atomName g tag) [attr g "type" "html"] [xmlText g t]
    toXMLElem g tag (InlineXHTMLContent xml)        = (xmlElem g) (atomName g tag) [attr g "type" "xhtml"] [xmlNode g xml]
    toXMLElem g tag (InlineTextContent t mmedia)    = (xmlElem g) (atomName g tag) (catMaybes [fmap (media g) mmedia]) [xmlText g t]
    toXMLElem g tag (InlineXMLContent xml mmedia)   = (xmlElem g) (atomName g tag) (catMaybes [fmap (media g) mmedia]) [xmlNode g xml]
    toXMLElem g tag (InlineBase64Content bs mmedia) = (xmlElem g) (atomName g tag) (catMaybes [fmap (media g) mmedia]) [xmlText g (T.decodeUtf8 $ B64.encode bs)]
    toXMLElem g tag (OutOfLineContent uri mmedia)   = (xmlElem g) (atomName g tag) (catMaybes [fmap (media g) mmedia, Just (attr g "src" (textShow uri))]) []

instance ToXMLElem e Person where
    toXMLElem g tag (Person name mu me) = (xmlElem g) (atomName g tag) [] (map (xmlNode g) (catMaybes
                     [                      Just   $ ((xmlElem g) (atomName g "name")  [] [xmlText g name])
                     , mu >>= \u ->         return $ ((xmlElem g) (atomName g "uri")   [] [xmlText g $ textShow u])
                     , me >>= \(Email e) -> return $ ((xmlElem g) (atomName g "email") [] [xmlText g e])
                     ]))

instance ToXMLElem e Category where
    toXMLElem g tag (Category term mscheme mlabel) = (xmlElem g) (atomName g tag) attrs []
      where attrs = catMaybes
                [ Just (attr g "term" term)
                , fmap (\uri   -> attr g "scheme" (textShow uri)) mscheme
                , fmap (\label -> attr g "label"  label)          mlabel
                ]

instance ToXMLElem e Generator where
    toXMLElem g tag (Generator name muri mversion) = (xmlElem g) (atomName g tag) attrs [xmlText g name]
      where attrs = catMaybes
                [ fmap (\uri -> attr g "uri"     (textShow uri)) muri
                , fmap (\v   -> attr g "version" v)              mversion
                ]

instance ToXMLElem e Link where
    toXMLElem g tag (Link href mrel mmedia mlang mtitle mlen) = (xmlElem g) (atomName g tag) attrs []
      where attrs = catMaybes
                [ Just (attr g "href" (textShow href))
                , fmap (\rel                -> attr g "rel" (textShow rel))    mrel
                , fmap (media g) mmedia
                , fmap (\(LanguageTag lang) -> attr g "hreflang" lang)         mlang
                , fmap (\title              -> attr g "title" title)           mtitle
                , fmap (\len                -> attr g "length" (textShow len)) mlen
                ]

instance ToXMLElem e URI where
    toXMLElem g tag uri = (xmlElem g) (atomName g tag) [] [xmlText g (textShow uri)]

instance ToXMLElem e UTCTime where
    toXMLElem g tag utc = (xmlElem g) (atomName g tag) [] [xmlText g (T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utc)]

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
