atom-basic
==========

[`atom-basic`](https://hackage.haskell.org/package/atom-basic) aims to provide
type-safe [RFC4287](http://tools.ietf.org/html/rfc4287) Atom feed construction
using [`text`](https://hackage.haskell.org/package/text) for the Haskell XML
package of your choice.

The idea is that you construct a `Feed` or `Entry` using the constructors
provided by the package and then generate XML of your preferred type from this
using the `feedXML` or `entryXML` functions.

The type of XML generated is determiend by the `XMLGen` record. This consists
of generator functions that define how various XML values are constructed using
your XML library of choice.

```haskell
data XMLGen elem node name attr = XMLGen
    { xmlElem     :: name -> [attr] -> [node] -> elem
    -- ^ Element from name, attributes, and nodes/contents.
    , xmlName     :: Maybe T.Text -> T.Text -> name
    -- ^ Qualified name from optional namespace and name.
    , xmlAttr     :: name -> T.Text -> attr
    -- ^ Attribute from qualified name and text value.
    , xmlTextNode :: T.Text -> node
    -- ^ Text node/content from text value.
    , xmlElemNode :: elem -> node
    -- ^ Element node/content from element.
    }
```

Example usage
-------------

Using the [`xml`]() package for XML generation could look like this:

```haskell
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
```

Another example that uses the
[`xml-conduit`](https://hackage.haskell.org/package/xml-conduit) package is
available in the
[`examples/atom-xml-conduit/`](examples/atom-xml-conduit/Main.hs) folder.

Alternatives
------------

You might want to have a look at the
[`feed`](https://hackage.haskell.org/package/feed) package as an alternative to
[`atom-basic`](https://hackage.haskell.org/package/atom-basic). `atom-basic`
differs from `feed` in that it is more strongly typed and the XML generation is
more flexible (it's not limited to a particular XML library). The additional
type-safety and flexibility makes `atom-basic` somewhat less convenient to work
with however. Also note that `feed` also provides feed parsers, which
`atom-basic` does not.
