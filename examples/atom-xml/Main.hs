{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text      as T
import           Data.Time      (UTCTime (..), fromGregorian)
import           Text.XML.Light
import qualified Web.Atom       as Atom

xmlgen :: Atom.XMLGen Element Content QName Attr
xmlgen = Atom.XMLGen
    { Atom.xmlElem     = \n as ns    -> Element n as ns Nothing
    , Atom.xmlName     = \nsMay name -> QName (T.unpack name)
                                          (fmap T.unpack nsMay) Nothing
    , Atom.xmlAttr     = \k v        -> Attr k (T.unpack v)
    , Atom.xmlTextNode = \t          -> Text $ CData CDataText (T.unpack t) Nothing
    , Atom.xmlElemNode = Elem
    }

feed :: Atom.Feed Element
feed = Atom.makeFeed
    (Atom.unsafeURI "https://haskell.org/")
    (Atom.TextHTML "The <em>Title</em>")
    (UTCTime (fromGregorian 2015 7 8) 0)

main = putStr $ ppTopElement $ Atom.feedXML xmlgen feed
