{-# LANGUAGE OverloadedStrings #-}

module Data.Quickson
    (
    -- * How to use this library
    -- $use

    -- * Syntax
    -- $syntax
      Quickson(..)
    , quicksonParse
    , quicksonExecute
    , quickson
    ) where


import           Control.Monad
import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import           Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T


-- | Quickson intermediary representation
data Quickson = Ob [(T.Text, Bool, Quickson)]
              | Li Quickson
              | Va deriving (Show)


-- | Parse a quickson structure
quicksonParse :: BS.ByteString -> Either String Quickson
quicksonParse = parseOnly parseStructure
  where
    parseStructure = parseObject <|> parseArray
    parseObject = Ob <$> ("{" *> sepBy1 parseLookups (char ',') <* "}")
    parseArray = Li <$> ("[" *> parseStructure <* "]")
    parseLookups = (,,) <$> (b2t <$> takeWhile1 (notInClass "?,:}"))
                        <*> ("?" *> pure True <|> pure False)
                        <*> (":" *> parseStructure <|> pure Va)
    b2t = T.pack . C8.unpack


-- | Execute a quickson structure against a value
quicksonExecute :: FromJSON a => Quickson -> Value -> Either String a
quicksonExecute que = AT.parseEither (drill que >=> parseJSON)
  where
    drill :: Quickson -> Value -> AT.Parser Value
    drill (Ob [l]) = parseJSON >=> flip look l
    drill (Ob keys) = parseJSON >=> forM keys . look >=> return . toJSON
    drill (Li q) = parseJSON >=> mapM (drill q) >=> return . toJSON
    drill Va = return
    look v (k,True,qq) = v.:?k >>= maybe (return Null) (drill qq)
    look v (k,False,qq) = v.:k >>= drill qq


-- | Perform a JSON extraction, returning either an error description
--   or a parsed data structure
quickson :: FromJSON a => BS.ByteString -> BS.ByteString -> Either String a
quickson structureSpec bs = do
    val <- eitherDecodeStrict bs
    query <- quicksonParse structureSpec
    quicksonExecute query val


-- $use
--
-- Quickson exports a function `quickson` which enables you to perform quick
-- extractions of JSON data using Aeson.
--
-- Aeson's type machinery allows decoding of complex data structures using
-- just the 'decode' function, however, JSON object lookups cannot be encoded
-- using the type system alone. Quickson helps by doing the lookups for you
-- so that the type system can do the rest. For example, say you have a JSON
-- document as such:
--
-- > { "name": "bob", "age": 25, "hobbies": [{"name": "Tennis"}] }
--
-- And you'd like to turn this into a `(String, Maybe Int, [String])` with
-- minimal fuss:
--
-- > >>> type Hobbyist = (String, Maybe Int, [String])
-- > >>> let eitherResult = quickson "{name,age?,hobbies:[{name}]}" jsonDoc :: Either String Hobbyist
-- > Right ("bob",Just 25,["Tennis"])
--
-- So the structure specification is just to remove the objects so that the type
-- system can do the rest.

-- $syntax
-- - Top level objects must be [] or {}
-- - Lookup: {key}
-- - Optional lookup: {key?} (yielding Maybe a)
-- - List: []
--