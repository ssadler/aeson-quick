{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aeson.Quick
    (
    -- $use
    -- $examples
      module Ae
    , (.?)
    , (.!)
    , extract
    , (.%)
    , build
    , Structure(..)
    , parseStructure
    ) where


import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Aeson as Ae
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Text hiding (parse)
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)


data Structure = Obj [(T.Text, Bool, Structure)]
               | Arr Structure
               | Val
 deriving (Eq, Ord, Generic)

instance NFData Structure

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


{- |
Extracts instances of 'FromJSON' from a 'Value'

This is a wrapper around 'extract' which does the actual work.

Examples assume 'FromJSON' Foo and 'FromJSON' Bar.

Extract key from object:

>>> value .? "{key}" :: Maybe Foo

Extract list of objects:

>>> value .? "[{key}]" :: Maybe [Foo]

Extract with optional key:

>>> value .? "{key,opt?}" :: Maybe (Foo, Maybe Bar)
-}
(.?) :: FromJSON a => Value -> Structure -> Maybe a
(.?) = AT.parseMaybe . flip extract
{-# INLINE (.?) #-}

-- TODO: Appropriate infixes?

{- |
The (very!) unsafe version of '.?'. This can fail very easily, do not depend on this in your program. Will probably be removed.
-}
(.!) :: FromJSON a => Value -> Structure -> a
(.!) v s = either err id $ AT.parseEither (extract s) v
  where err msg = error $ show s ++ ": " ++ msg ++ " in " ++ show v
{-# INLINE (.!) #-}


{- |
The 'Parser' that executes a 'Structure' against a 'Value' to return an instance of 'FromJSON'. 
-}
extract :: FromJSON a => Structure -> Value -> AT.Parser a
extract structure = go structure >=> parseJSON
  where
    go (Obj [s])  = withObject "" (flip look s)
    go (Obj sx)   = withObject "" (forM sx . look) >=> pure . toJSON
    go (Arr s)    = withArray  "" (V.mapM $ go s)   >=> pure . Array
    go Val = pure
    look v (k,False,Val) = v .: k
    look v (k,False,s)   = v .:  k >>= go s
    look v (k,True,s)    = v .:? k >>= maybe (pure Null) (go s)


{- |
Turns data into JSON objects. 

This is a wrapper around 'build' which does the actual work.

Build a simple Value:

>>> encode $ "{a}" .% True
{\"a\": True}

Build a complex Value:

>>> encode $ "[{a}]" '.%' [True, False]
"[{\"a\":true},{\"a\":false}]"
-}
(.%) :: ToJSON a => Structure -> a -> Value
(.%) s = build s Null
{-# INLINE (.%) #-}


{- |
Executes a 'Structure' against provided data to update a 'Value'.

Note: /Still has undefined behaviours/, not at all stable.
-}
build :: ToJSON a => Structure -> Value -> a -> Value
build structure val = go structure val . toJSON
  where
    go (Val)      _          r         = r
    go (Arr s)    (Array v)  (Array r) = Array $ V.zipWith (go s) v r 
    go (Arr s)    Null       (Array r) = Array $ V.map (go s Null) r
    go (Arr s)    Null       r         = toJSON [go s Null r]
    go (Obj [ks]) (Object v) r         = Object $ update v ks r
    go (Obj keys) Null       r         = go (Obj keys) (Object mempty) r
    go (Obj ks)   (Object v) (Array r) = Object $
      let maps = zip ks (V.toList r)
       in foldl (\v' (s,r') -> update v' s r') v maps
    go (Obj keys) (Object v) r         = r
    go a b c = error $ show (a,b,c) -- TODO
    update v (k,_,s) r =
      let startVal = go s (H.lookupDefault Null k v) r
       in H.insert k startVal v


-- $use
--
-- aeson-quick is a library for terse marshalling of data to and from aeson's
-- 'Value'.
--
-- It works on the observation that by turning objects into tuples inside
-- the 'Value', the type system can be employed to do more of the work.
--
-- For example, given the JSON:
--
-- > { "name": "bob"
-- > , "age": 29
-- > , "hobbies": [{"name": "tennis"}, {"name": "cooking"}]
-- > }
--
-- You can write: 
--
-- @
-- extractHobbyist :: 'Value' -> 'Maybe' ('String', 'Int', ['String'])
-- extractHobbyist value = value '.?' "{name,age,hobbies:[{name}]}"
-- @
--
