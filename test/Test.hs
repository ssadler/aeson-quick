{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Quickson

import Lens.Micro

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup "Tests" 
  [ oneKey
  , multipleKeys
  , deepKey
  , keyInArray
  , complex
  , optionalKey
  , nonExistentKey
  , asLens
  ]


oneKey :: TestTree
oneKey = testGroup "oneKey"
  [ testCase "get" $
      val .! "{a}" @?= one

  , testCase "set" $
      euq "{a}" val Null `jt` "{\"a\":null}"
  ]
  where val = d "{\"a\":1}"


multipleKeys :: TestTree
multipleKeys = testGroup "multipleKeys"
  [ testCase "get" $
      multiple .! "{a,b}" @?= (one,two)

  , testCase "set" $
      euq "{a,b}" multiple (two,[one,one]) `jt` "{\"a\":2,\"b\":[1,1]}"
  ]
  where multiple = d "{\"a\":1,\"b\":2}"


deepKey :: TestTree
deepKey = testGroup "deepKey"
  [ testCase "get" $
      multilevel .! "{a:{b}}" @?= Just one

  , testCase "set" $
      euq "{a:{b}}" multilevel two `jt` "{\"a\":{\"b\":2}}"
  ]
  where multilevel = d "{\"a\":{\"b\":1}}"


keyInArray :: TestTree
keyInArray = testGroup "keyInArray"
  [ testCase "get" $
      many .! "[{a}]" @?= Just [one,two]

  , testCase "set" $
      euq "[{a}]" many [True,False] `jt` "[{\"a\":true},{\"a\":false}]"
  ]
  where many = d "[{\"a\":1},{\"a\":2}]"


complex :: TestTree
complex = testGroup "complex"
  [ testCase "get" $
      val .! "{a,b:[{a}]}" @?= Just (one,[[two,one]])
  , testCase "set" $
      euq "{a,b:[{a}]}" val (two,[[one]]) `jt` "{\"a\":2,\"b\":[{\"a\":[1]}]}"
  ]
  where val = d "{\"a\":1,\"b\":[{\"a\":[2,1]}]}"


optionalKey :: TestTree
optionalKey = testGroup "optionalKey"
  [ testCase "get" $
      val .! "{a?,b?}" @?= (Just one, Nothing :: Maybe Int)
  ]
  where val = d "{\"a\":1}"


nonExistentKey :: TestTree
nonExistentKey = testGroup "nonExistentKey"
  [ testCase "get" $
      val .? "{b}" @?= (Nothing :: Maybe Int)
  
  , testCase "set" $
      euq "{a}" (d "{}") Null `jt` "{\"a\":null}"

  , testCase "setDeep" $
      euq "{a:[{b}]}" (d "{}") Null `jt` "{\"a\":[{\"b\":null}]}"
  ]
  where val = d "{\"a\":1}"


asLens :: TestTree
asLens = testGroup "asLens"
  [ testCase "get" $
      let l = queLens "{a,b}" :: Lens' Value (Int,Int)
       in val ^. l . _2 @?= two

  , testCase "getDoesNotExist" $
      -- doesn't work, make a Traversal?
      d "{}" ^? queLens "{a}" @?= (Nothing :: Maybe (Int,Int))

  , testCase "set" $
      (val & (queLens "{a,b}") .~ (two,one)) `jt` "{\"a\":2,\"b\":1}"
  ]
  where
    val = d "{\"a\":1,\"b\":2}"
    queLens :: (FromJSON a, ToJSON a) => Structure -> Lens' Value a
    queLens s = lens (.!s) (euq s)


one, two :: Int
one = 1 
two = 2


jt :: Value -> ByteString -> IO ()
jt v b = encode v @?= b


d :: ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s

