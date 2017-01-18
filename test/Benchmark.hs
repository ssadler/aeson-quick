{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Criterion.Main

import Data.Aeson.Quick
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import Lens.Micro
import Lens.Micro.Aeson

import System.IO.Unsafe

main :: IO ()
main = defaultMain
  -- TODO: Refactor such that "bench" also tests
  [ bench "aqGetSimple"        $ nf getSimple jsonSimple
  , bench "aesonGetSimple"     $ nf aesonGetSimple jsonSimple
  , bench "microLensGetSimple" $ nf microLensGetSimple jsonSimple
  , bench "aqGetComplex"       $ nf getComplex jsonComplex
  , bench "aesonGetComplex"    $ nf aesonGetComplex jsonComplex
  , bench "aqSetSimple"        $ nf setSimple simple
  , bench "aesonSetSimple"     $ nf aesonSetSimple simple
  , bench "aqSetComplex"       $ nf setComplex complex
  , bench "aesonSetComplex"    $ nf aesonSetComplex complex
  , bench "parseSimple"        $ nf parseStructure "{a}"
  , bench "parseComplex"       $ nf parseStructure "{a,b:[{c,d:[{e,f}]}]}"
 ]
  where
    jsonSimple = d "{\"a\":2,\"b\":[1,1]}" :: Value
    jsonComplex = d $ unsafePerformIO $ BL.readFile "test/complex.json"

    check :: (Value -> Maybe a) -> Value -> a
    check f = maybe (error "Nothing") id . f

    getSimple, aesonGetSimple, microLensGetSimple :: Value -> Integer
    getSimple = check (.? "{a}")
    aesonGetSimple = check $ parseMaybe $ withObject "" (.: "a")
    microLensGetSimple = check (^? key "a" . _Integer)
    
    getComplex, aesonGetComplex :: Value -> [(Text,Float,[Text],[Text])]
    getComplex = check (.! "[{id,ppu,batters:{batter:[{id}]},topping:[{id}]}]")
    aesonGetComplex = check $ parseMaybe $ parseJSON >=> mapM (\o ->
      (,,,) <$> o .: "id" <*> o .: "ppu" <*> batters o <*> toppings o)
      where
        batters = (.:"batters") >=> withObject "" (.:"batter")
                                >=> mapM (withObject "" (.:"id"))
        toppings = (.:"topping") >=> mapM (withObject "" (.:"id"))
    
    simple = object ["a" .= Number 1]
    setSimple, aesonSetSimple :: Value -> Bool
    setSimple r = r `must` build "{a}" Null (1::Int) 
    aesonSetSimple r = r `must` object ["a" .= (1::Int)]

    Just complex = decode "{\"a\":1,\"b\":[{\"a\":1},{\"a\":2},{\"a\":3}]}"
    setComplex, aesonSetComplex :: Value -> Bool
    setComplex r =
      let vals = ((1,[1,2,3])::(Int,[Int]))
       in r `must` build "{a,b:[{a}]}" Null vals
    aesonSetComplex r =
      let inner = [object ["a" .= n] | n <- [1,2,3::Int]]
       in r `must` object ["a" .= (1::Int), "b" .= inner]


must :: Eq a => a -> a -> Bool
must a b = if a == b then True else error "False"

d :: BL.ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s
