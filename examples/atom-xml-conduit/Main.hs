{-# LANGUAGE OverloadedStrings #-}

import           Data.Map.Lazy     (fromList)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TL
import           Data.Time         (UTCTime (..), fromGregorian)
import           Text.XML
import qualified Web.Atom          as Atom

xmlgen :: Atom.XMLGen Element Node Name (Name, T.Text)
xmlgen = Atom.XMLGen
    { Atom.xmlElem     = \n as ns    -> Element n (fromList as) ns
    , Atom.xmlName     = \nsMay name -> Name name nsMay Nothing
    , Atom.xmlAttr     = \k v        -> (k, v)
    , Atom.xmlTextNode = NodeContent
    , Atom.xmlElemNode = NodeElement
    }

feed :: Atom.Feed Element
feed = Atom.makeFeed
    (Atom.unsafeURI "https://haskell.org/")
    (Atom.TextHTML "The <em>Title</em>")
    (UTCTime (fromGregorian 2015 7 8) 0)

main = TL.putStrLn $ renderText def (Document (Prologue [] Nothing []) xml [])
  where xml = Atom.feedXML xmlgen feed
