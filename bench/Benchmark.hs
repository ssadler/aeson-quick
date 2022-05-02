{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad

import Criterion.Main

import Data.Aeson.Quick
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import System.IO.Unsafe

main :: IO ()
main = defaultMain
  -- TODO: Refactor such that "bench" also tests
  [ bench "quickGetSimple"     $ nf quickGetSimple jsonSimple
  , bench "aesonGetSimple"     $ nf aesonGetSimple jsonSimple
  , bench "quickGetComplex"    $ nf quickGetComplex jsonComplex
  , bench "aesonGetComplex"    $ nf aesonGetComplex jsonComplex
  , bench "quickSetSimple"     $ nf quickSetSimple simple
  , bench "aesonSetSimple"     $ nf aesonSetSimple simple
  , bench "quickSetComplex"    $ nf quickSetComplex complex
  , bench "aesonSetComplex"    $ nf aesonSetComplex complex
  , bench "parseSimple"        $ nf parseQuick "{a}"
  , bench "parseComplex"       $ nf parseQuick "{a,b:[{c,d:[{e,f}]}]}"
 ]
  where
    jsonSimple = d "{\"a\":2,\"b\":[1,1]}" :: Value
    jsonComplex = d $ unsafePerformIO $ BL.readFile "bench/complex.json"

    check :: (Value -> Maybe a) -> Value -> a
    check f = maybe (error "Nothing") id . f

    quickGetSimple, aesonGetSimple :: Value -> Integer
    quickGetSimple = check (.? "{a}")
    aesonGetSimple = check $ parseMaybe $ withObject "" (.: "a")
    
    quickGetComplex, aesonGetComplex :: Value -> [(Text,Float,[Text],[Text])]
    quickGetComplex = check (.! "[{id,ppu,batters:{batter:[{id}]},topping:[{id}]}]")
    aesonGetComplex = check $ parseMaybe $ parseJSON >=> mapM (\o ->
      (,,,) <$> o .: "id" <*> o .: "ppu" <*> batters o <*> toppings o)
      where
        batters = (.:"batters") >=> withObject "" (.:"batter")
                                >=> mapM (withObject "" (.:"id"))
        toppings = (.:"topping") >=> mapM (withObject "" (.:"id"))
    
    simple = object ["a" .= Number 1]
    quickSetSimple, aesonSetSimple :: Value -> Bool
    quickSetSimple r = r `must` ("{a}" .% (1::Int))
    aesonSetSimple r = r `must` object ["a" .= (1::Int)]

    Just complex = decode "{\"a\":1,\"b\":[{\"a\":1},{\"a\":2},{\"a\":3}]}"
    quickSetComplex, aesonSetComplex :: Value -> Bool
    quickSetComplex r =
      let vals = ((1,[1,2,3])::(Int,[Int]))
       in r `must` ("{a,b:[{a}]}" .% vals)
    aesonSetComplex r =
      let inner = [object ["a" .= n] | n <- [1,2,3::Int]]
       in r `must` object ["a" .= (1::Int), "b" .= inner]


must :: Eq a => a -> a -> Bool
must a b = if a == b then True else error "False"

d :: BL.ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s
