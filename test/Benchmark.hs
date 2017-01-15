{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Quickson
import Data.Text

import System.IO.Unsafe

main :: IO ()
main = defaultMain
  [ bench "parseSimple" $ whnf parse "{a}"
  
  , bench "parseComplex" $ whnf parse "{a,b:[{c,d:[{e,f}]}]}"

  , bench "getSimple" $ whnf getSimple jsonSimple

  , bench "aesonGetSimple" $ whnf aesonGetSimple jsonSimple

  , bench "getComplex" $ whnf getComplex jsonComplex
  
  , bench "aesonGetComplex" $ whnf aesonGetComplex jsonComplex
  ]
  where
    jsonSimple = d "{\"a\":2,\"b\":[1,1]}" :: Value
    jsonComplex = d $ unsafePerformIO $ BL.readFile "test/complex.json"
    getSimple = check $ unQue "{a}" :: Value -> Int
    getComplex = check $ unQue "[{id,ppu,batters:{batter:[{id}]},topping:[{id}]}]" :: Value -> [(Text,Float,[Text],[Text])]
    check :: (Value -> Maybe a) -> Value -> a
    check f = maybe (error "Nothing") id . f

    aesonGetSimple :: Value -> Int
    aesonGetSimple = check $ parseMaybe $ withObject "" (.:"a")

    aesonGetComplex :: Value -> [(Text,Float,[Text],[Text])]
    aesonGetComplex = check $ parseMaybe $
         \val -> parseJSON val >>= mapM agcItem
    agcItem wat = (,,,) <$> wat .: "id"
                        <*> wat .: "ppu"
                        <*> acgBatters wat
                        <*> acgToppings wat
    acgBatters wat = do
      batters <- (wat.:"batters") >>= withObject "" (.:"batter")
      parseJSON batters >>= mapM (withObject "" (.:"id"))
    acgToppings wat = do
      wat .: "topping" >>= parseJSON >>= mapM (withObject "" (.:"id"))




  


d :: BL.ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s
