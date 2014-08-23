{-# LANGUAGE OverloadedStrings #-}

module Data.Quickson where

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


-- | Perform a JSON extraction
quickson :: FromJSON a => BS.ByteString -> BS.ByteString -> Either String a
quickson structureSpec bs = do
    val <- eitherDecodeStrict bs
    query <- quicksonParse structureSpec
    quicksonExecute query val
