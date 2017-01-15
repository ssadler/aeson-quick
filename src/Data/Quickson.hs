{-# LANGUAGE OverloadedStrings #-}

module Data.Quickson
    (
    -- * How to use this library
    -- $use

    -- * Syntax
    -- $syntax
      Structure(..)
    , parse
    , euq
    , que
    , unQue
    , (.!)
    , (.?)
    ) where


import Control.Monad
import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Text hiding (parse)
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V

import Lens.Micro
import Lens.Micro.Platform ()


-- | Structure intermediary representation
data Structure = Obj [(T.Text, Bool, Structure)]
               | Arr Structure
               | Val


instance IsString Structure where
  fromString s =
    let e = error $ "Invalid quickson structure: " ++ s
     in either (\_ -> e) id $ parse $ T.pack s


instance Show Structure where
  show (Arr s) = "[" ++ show s ++ "]"
  show (Obj xs) = "{" ++ drop 1 (concatMap go xs) ++ "}"
    where go (k,o,Val) = "," ++ T.unpack k ++ if o then "?" else ""
          go (k,o,s) = "," ++ T.unpack k ++ if o then "?" else "" ++ ":" ++ show s
    

-- | Parse a quickson structure
parse :: T.Text -> Either String Structure
parse = parseOnly structure
  where
    structure = object' <|> array
    object' = Obj <$> ("{" *> sepBy1 lookups (char ',') <* "}")
    array = Arr <$> ("[" *> structure <* "]")
    lookups = (,,) <$> (takeWhile1 (notInClass "?,:}"))
                   <*> ("?" *> pure True <|> pure False)
                   <*> (":" *> structure <|> pure Val)


que :: FromJSON a => Structure -> Value -> AT.Parser a
que structure = {-# SCC "que" #-} ggo structure >=> parseJSON
  where
    ggo :: Structure -> Value -> AT.Parser Value
    ggo (Obj [l])  = {-# SCC "go0" #-} withObject "" (flip look l)
    ggo (Obj ks)   = {-# SCC "go1" #-} withObject "" (forM ks . look) >=> pure . toJSON
    ggo (Arr q)    = {-# SCC "go2" #-} withArray  "" (mapM $ ggo q)    >=> pure . Array
    ggo Val = pure
    look v (k,False,Val) = {-# SCC "go5" #-} v .: k
    look v (k,False,s) = {-# SCC "go3" #-} v .:  k >>= ggo s
    look v (k,True,s)  = {-# SCC "go4" #-} v .:? k >>= maybe (pure Null) (ggo s)


-- | Execute a quickson structure against a value
unQue :: FromJSON a => Structure -> Value -> Maybe a
unQue = AT.parseMaybe . que
{-# INLINE unQue #-}


(.?) :: FromJSON a => Value -> Structure -> Maybe a
(.?) = flip unQue


(.!) :: FromJSON a => Value -> Structure -> a
(.!) v s = either err id $ AT.parseEither (que s) v
  where err msg = error $ show s ++ ": " ++ msg ++ " in " ++ show v


euq :: ToJSON a => Structure -> Value -> a -> Value
euq structure val = go structure val . toJSON
  where
    go (Val)      _          r         = r
    go (Arr s)    (Array v)  (Array r) = Array $ V.zipWith (go s) v r 
    go (Arr s)    Null       (Array r) = Array $ V.map (go s Null) r
    go (Arr s)    Null       r         = toJSON [go s Null r]
    go (Obj [ks]) (Object v) r         = Object $ update v ks r
    go (Obj keys) Null       r         = go (Obj keys) (Object mempty) r
    go (Obj rs)   (Object v) (Array r) = Object $
      let maps = zip rs (V.toList r)
       in foldl (\v' (ks,r') -> update v' ks r') v maps
    go a b c = error $ show (a,b,c)
    update v (k,_,s) r = v & at k %~ \mv' -> Just $ go s (maybe Null id mv') r


--quicksonUpdate :: ToJSON a => Structure -> a -> Value -> Either String Value
--quicksonUpdate q a v = go q (toJSON a) v
--  where
--    go (Obj keys) (Array 


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
