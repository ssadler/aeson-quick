{-# LANGUAGE OverloadedStrings #-}


import Data.Aeson
import Data.ByteString (ByteString)
import Data.Quickson

import Lens.Micro
import Lens.Micro.Aeson

import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace


main :: IO ()
main = defaultMain $ testGroup "Tests" 
  [ oneKey
  , multipleKeys
  , deepKey
  , keyInArray
  ]


oneKey, multipleKeys, deepKey, keyInArray :: TestTree

oneKey = testGroup "one key"
  [ testCase "get" $
      unQue "{a}" simple @?= Just one

  , testCase "set" $
      euq "{a}" simple Null @?= d "{\"a\":null}"
  ]
  where simple = d "{\"a\":1}"

multipleKeys = testGroup "multiple keys"
  [ testCase "get" $
      unQue "{a,b}" multiple @?= Just (one,two)

  , testCase "set" $
      euq "{a,b}" multiple (two,[one,one]) @?= d "{\"a\":2,\"b\":[1,1]}"
  ]
  where multiple = d "{\"a\":1,\"b\":2}"

deepKey = testGroup "deep key"
  [ testCase "get" $
      unQue "{a:{b}}" multilevel @?= Just one

  , testCase "set" $
      euq "{a:{b}}" multilevel two @?= d "{\"a\":{\"b\":2}}"
  ]
  where multilevel = d "{\"a\":{\"b\":1}}"

keyInArray = testGroup "key in array"
  [ testCase "get" $
      unQue "[{a}]" many @?= Just [one,two]

  , testCase "set" $
      euq "[{a}]" many [True,False] @?= d "[{\"a\":true},{\"a\":false}]"
  ]
  where many = d "[{\"a\":1},{\"a\":2}]"


one, two :: Int
one = 1 
two = 2


d :: ByteString -> Value
d s = let (Just v) = decodeStrict s in v

