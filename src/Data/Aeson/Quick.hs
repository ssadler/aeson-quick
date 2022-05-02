{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Quick
    (
    -- $use
      module Ae
    , (.?)
    , (.!)
    , extract
    , (.%)
    , build
    , Quick(..)
    , parseQuick
    , quick
    , jsonlit
    ) where

import Debug.Trace

import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Aeson as Ae
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Text hiding (parse)
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid hiding (All)
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote

import Data.Aeson.Quick.Internal

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key

type KeyType = Key.Key

keyToString :: KeyType -> String
keyToString = Key.toString

textToKey :: T.Text -> KeyType
textToKey = Key.fromText

#else

type KeyType = T.Text

keyToString :: KeyType -> String
keyToString = T.unpack

textToKey :: T.Text -> KeyType
textToKey = id

#endif

data Quick =
    Obj [(KeyType, Bool, Quick)]
  | Arr Quick Bounds
  | Val
  deriving (Eq, Ord, Generic)

instance NFData Quick

instance IsString Quick where
  fromString s =
    let e = error $ "Invalid structure: " ++ s
     in either (\_ -> e) id $ parseQuick $ T.pack s


instance Show Quick where
  show (Val) = "."
  show (Arr s b) = "[" ++ show s ++ "]" ++ showBound b where
    showBound (All) = ""
    showBound (Single i) = show i
    showBound (Range a mb) = show a ++ "-" ++ maybe "" show mb
  show (Obj xs) = "{" ++ drop 1 (concatMap go xs) ++ "}" where
    go (k,o,s) = "," ++ showKey (keyToString k) ++ (if o then "?" else "")
                     ++ (if s == Val then "" else ":" ++ show s)
    showKey "" = ""
    showKey (':':xs) = "\\:" ++ showKey xs
    showKey (',':xs) = "\\," ++ showKey xs
    showKey (c:xs) = c : showKey xs


-- | Parse a structure, can fail
parseQuick :: T.Text -> Either String Quick
parseQuick = parseOnly (structure <* endOfInput)
  where
    structure :: Parser Quick
    structure = object' <|> array <|> val

    object' :: Parser Quick
    object' = Obj <$> ("{" *> sepBy lookups (char ',') <* "}")

    array :: Parser Quick
    array = Arr <$> ("[" *> structure <* "]") <*> (arrayBounds <|> pure All)

    arrayBounds = do
      i <- decimal
      let rangeTo = (Just <$> decimal) <|> pure Nothing
      ("-" >> Range i <$> rangeTo) <|>
        pure (Single i)

    val :: Parser Quick
    val = "." >> pure Val

    lookups :: Parser (KeyType, Bool, Quick)
    lookups = (,,) <$> (textToKey <$> (quotedKey <|> plainKey))
                   <*> ("?" *> pure True <|> pure False)
                   <*> (":" *> structure <|> pure Val)

    quotedKey :: Parser T.Text
    quotedKey = "\"" *> scan False testChar <* "\""

    testChar :: Bool -> Char -> Maybe Bool
    testChar False '"'  = Nothing
    testChar False '\\' = Just True
    testChar _ _        = Just False

    plainKey :: Parser T.Text
    plainKey = takeWhile1 (notInClass "\",:{}?")


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
(.?) :: FromJSON a => Value -> Quick -> Maybe a
(.?) = AT.parseMaybe . flip extract
{-# INLINE (.?) #-}

-- TODO: Appropriate infixes?

{- |
Unsafe version of '.?'. Returns 'error' on failure.
-}
(.!) :: FromJSON a => Value -> Quick -> a
(.!) v s = either err id $ AT.parseEither (extract s) v
  where err msg = error $ show s ++ ": " ++ msg ++ " in " ++ show v
{-# INLINE (.!) #-}


{- |
The 'Parser' that executes a 'Quick' against a 'Value' to return an instance of 'FromJSON'.
-}
extract :: FromJSON a => Quick -> Value -> AT.Parser a
extract structure = go structure >=> parseJSON
  where
    -- The go function translates the Value into a Value that can then be
    -- further parsed automatically into the return type `a`
    go (Obj [s])  = withObject "" (flip look s)
    go (Obj sx)   = withObject "" (forM sx . look) >=> pure . toJSON
    go (Arr s b)  = withArray  "" (pure . V.map (go s)) >=> bound b
    go Val        = pure
    look v (k,False,Val) = v .: k
    look v (k,False,s)   = v .: k >>= go s
    look v (k,True,s)    = v .:? k >>= maybe (pure Null) (go s)

    bound All v = Array <$> sequence v
    bound (Single i) v =
      case v V.!? i of
        Nothing -> pure Null
        Just a -> a
    bound (Range a mb) v =
      bound All $ V.drop a $ maybe v (\b -> V.take b v) mb


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
(.%) :: ToJSON a => Quick -> a -> Value
(.%) s a = either error id $ build s a
{-# INLINE (.%) #-}


{- |
Executes a 'Quick' against provided data to update a 'Value'.
-}
build :: ToJSON a => Quick -> a -> Either String Value
build structure a = go structure $ toJSON a where
  go (Val)       v           = pure v
  go (Arr s All) (Array r)   = Array <$> V.mapM (go s) r
  -- The reason that one element is wrapped in an array is that
  -- tuples should always be provided for objects, but a tuple of one
  -- is not wrapped.
  go (Arr s _)   _           = Left "Cannot index an array during construction"
  go (Obj [k])   v           = object <$> items (zip [k] [v])
  go (Obj xs)    (Array v)   =
    if length xs /= V.length v
       then Left "Object / tuple length mismatch"
       else object <$> items (zip xs $ V.toList v)
  go _          _            = Left "Expected an array"

  items [] = pure []
  items (((k, o, s), val):xs) = do
      if o && val == Null
         then items xs
         else go s val >>= \h -> (k .= h:) <$> items xs


{- |
QuasiQuoter for a structure, provides compile time checking ie:

>>> val .! [quick|{foo,bar}|]
-}
quick :: QuasiQuoter
quick = QuasiQuoter
  { quotePat = error "quick quasi quoter cannot be used as a pattern"
  , quoteDec = error "quick quasi quoter cannot be used as a declaration"
  , quoteType = error "quick quasi quoter cannot be used as a type"
  , quoteExp = \s ->
      let q = fromString s :: Quick
       in q `seq` [|fromString s|]
  }

jsonlit :: QuasiQuoter
jsonlit = QuasiQuoter
  { quotePat = error "quick quasi quoter cannot be used as a pattern"
  , quoteDec = error "quick quasi quoter cannot be used as a declaration"
  , quoteType = error "quick quasi quoter cannot be used as a type"
  , quoteExp = \s ->
      let bs = fromString s
          r = either error id $ eitherDecode bs :: Value
       in r `seq` [|r|]
  }



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
-- extractHobbyist = ('.?' "{name,age,hobbies:[{name}]}")
-- @
--
