{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aeson.Quick
    (
    -- * How to use this library
    -- $use

    -- * Syntax
    -- $syntax
      module Ae
    , Structure(..)
    , parseStructure
    , extract
    , build
    , (.!)
    , (.?)
    , (.%)
    ) where


import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Aeson as Ae
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Text hiding (parse)
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)



-- | Structure intermediary representation
data Structure = Obj [(T.Text, Bool, Structure)]
               | Arr Structure
               | Val
 deriving (Eq, Ord, Generic, NFData)


instance IsString Structure where
  fromString s =
    let e = error $ "Invalid structure: " ++ s
     in either (\_ -> e) id $ parseStructure $ T.pack s


instance Show Structure where
  show (Val) = "Val"
  show (Arr s) = "[" ++ show s ++ "]"
  show (Obj xs) = "{" ++ drop 1 (concatMap go xs) ++ "}"
    where go (k,o,s) = "," ++ T.unpack k ++ (if o then "?" else "")
                           ++ (if s == Val then "" else ":" ++ show s)
    

-- | Parse a structure, can fail
parseStructure :: T.Text -> Either String Structure
parseStructure = parseOnly structure
  where
    structure = object' <|> array
    object' = Obj <$> ("{" *> sepBy1 lookups (char ',') <* "}")
    array = Arr <$> ("[" *> structure <* "]")
    lookups = (,,) <$> (takeWhile1 isKeyChar)
                   <*> ("?" *> pure True <|> pure False)
                   <*> (":" *> structure <|> pure Val)
    isKeyChar = isAlphaNum -- TODO


extract :: FromJSON a => Structure -> Value -> AT.Parser a
extract structure = ggo structure >=> parseJSON
  where
    ggo (Obj [s])  = withObject "" (flip look s)
    ggo (Obj sx)   = withObject "" (forM sx . look) >=> pure . toJSON
    ggo (Arr s)    = withArray  "" (mapM $ ggo s)   >=> pure . Array
    ggo Val = pure
    look v (k,False,Val) = v .: k
    look v (k,False,s)   = v .:  k >>= ggo s
    look v (k,True,s)    = v .:? k >>= maybe (pure Null) (ggo s)


(.?) :: FromJSON a => Value -> Structure -> Maybe a
(.?) = AT.parseMaybe . flip extract
{-# INLINE (.?) #-}

-- TODO: Appropriate infixes?

(.!) :: FromJSON a => Value -> Structure -> a
(.!) v s = either err id $ AT.parseEither (extract s) v
  where err msg = error $ show s ++ ": " ++ msg ++ " in " ++ show v
{-# INLINE (.!) #-}


build :: ToJSON a => Structure -> Value -> a -> Value
build structure val = go structure val . toJSON
  where
    go (Val)      _          r         = r
    go (Arr s)    (Array v)  (Array r) = Array $ V.zipWith (go s) v r 
    go (Arr s)    Null       (Array r) = Array $ V.map (go s Null) r
    go (Arr s)    Null       r         = toJSON [go s Null r]
    go (Obj [ks]) (Object v) r         = Object $ update v ks r
    go (Obj keys) Null       r         = go (Obj keys) (Object mempty) r
    go (Obj keys) (Object v) r         = error "Cannot solve"
    go (Obj ks)   (Object v) (Array r) = Object $
      let maps = zip ks (V.toList r)
       in foldl (\v' (s,r') -> update v' s r') v maps
    go a b c = error $ show (a,b,c) -- TODO
    update v (k,_,s) r =
      H.alter (\mv' -> Just $ go s (maybe Null id mv') r) k v


(.%) :: ToJSON a => Structure -> a -> Value
(.%) s = build s Null
{-# INLINE (.%) #-}


-- $use
--
-- Structure exports a function `quickson` which enables you to perform quick
-- extractions of JSON data using Aeson.
--
-- Aeson's type machinery allows decoding of complex data structures using
-- just the 'decode' function, however, JSON object lookups cannot be encoded
-- using the type system alone. Structure helps by doing the lookups for you
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
-- - Arrst: []
--
