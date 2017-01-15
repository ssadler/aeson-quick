{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Criterion.Main

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Quickson
import Data.Text

import Lens.Micro
import Lens.Micro.Aeson

import System.IO.Unsafe

main :: IO ()
main = defaultMain
  [ bench "parseSimple" $ whnf parse "{a}"
  
  , bench "parseComplex" $ whnf parse "{a,b:[{c,d:[{e,f}]}]}"

  , bench "getSimple" $ whnf getSimple jsonSimple

  , bench "aesonGetSimple" $ whnf aesonGetSimple jsonSimple
  
  , bench "microLensGetSimple" $ whnf microLensGetSimple jsonSimple

  , bench "getComplex" $ whnf getComplex jsonComplex
  
  , bench "aesonGetComplex" $ whnf aesonGetComplex jsonComplex
  ]
  where
    jsonSimple = d "{\"a\":2,\"b\":[1,1]}" :: Value
    jsonComplex = d $ unsafePerformIO $ BL.readFile "test/complex.json"

    check :: (Value -> Maybe a) -> Value -> a
    check f = maybe (error "Nothing") id . f

    getSimple, aesonGetSimple, microLensGetSimple :: Value -> Integer
    getSimple = check $ unQue "{a}"
    aesonGetSimple = check $ parseMaybe $ withObject "" (.: "a")
    microLensGetSimple = check (^? key "a" . _Integer)
    
    getComplex, aesonGetComplex :: Value -> [(Text,Float,[Text],[Text])]
    getComplex = check $ unQue "[{id,ppu,batters:{batter:[{id}]},topping:[{id}]}]"
    aesonGetComplex = check $ parseMaybe $ parseJSON >=> mapM (\o ->
      (,,,) <$> o .: "id" <*> o .: "ppu" <*> batters o <*> toppings o)
      where
        batters = (.:"batters") >=> withObject "" (.:"batter")
                                >=> mapM (withObject "" (.:"id"))
        {-# INLINE batters #-}
        toppings = (.:"topping") >=> mapM (withObject "" (.:"id"))
        {-# INLINE toppings #-}




  


d :: BL.ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s
