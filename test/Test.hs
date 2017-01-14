{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


import Data.Aeson
import Data.ByteString (ByteString)
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
  , asLens
  ]


oneKey :: TestTree
oneKey = testGroup "one key"
  [ testCase "get" $
      unQue "{a}" simple @?= Just one

  , testCase "set" $
      euq "{a}" simple Null @?= d "{\"a\":null}"
  ]
  where simple = d "{\"a\":1}"


multipleKeys :: TestTree
multipleKeys = testGroup "multiple keys"
  [ testCase "get" $
      unQue "{a,b}" multiple @?= Just (one,two)

  , testCase "set" $
      euq "{a,b}" multiple (two,[one,one]) @?= d "{\"a\":2,\"b\":[1,1]}"
  ]
  where multiple = d "{\"a\":1,\"b\":2}"


deepKey :: TestTree
deepKey = testGroup "deep key"
  [ testCase "get" $
      unQue "{a:{b}}" multilevel @?= Just one

  , testCase "set" $
      euq "{a:{b}}" multilevel two @?= d "{\"a\":{\"b\":2}}"
  ]
  where multilevel = d "{\"a\":{\"b\":1}}"


keyInArray :: TestTree
keyInArray = testGroup "key in array"
  [ testCase "get" $
      unQue "[{a}]" many @?= Just [one,two]

  , testCase "set" $
      euq "[{a}]" many [True,False] @?= d "[{\"a\":true},{\"a\":false}]"
  ]
  where many = d "[{\"a\":1},{\"a\":2}]"


complex :: TestTree
complex = testGroup "complex"
  [ testCase "get" $
      unQue "{a,b:[{a}]}" val @?= Just (one,[[two,one]])
  , testCase "set" $
      euq "{a,b:[{a}]}" val (two,[[one]]) @?= d "{\"a\":2,\"b\":[{\"a\":[1]}]}"
  ]
  where val = d "{\"a\":1,\"b\":[{\"a\":[2,1]}]}"


asLens :: TestTree
asLens = testGroup "as lens"
  [ testCase "get" $
      let l = queLens "{a,b}" :: Lens' Value (Int,Int)
       in val ^. l . _2 @?= two

  , testCase "getDoesNotExist" $
      d "{}" ^. queLens "{a}" @?= (Nothing :: Maybe (Int,Int))

  , testCase "set" $
      (val & (queLens "{a,b}") .~ (two,one)) @?= d "{\"a\":2,\"b\":1}"
  ]
  where
    val = d "{\"a\":1,\"b\":2}"
    queLens :: (FromJSON a, ToJSON a) => Structure -> Lens' Value a
    queLens s = lens (\v -> let Just r = unQue s v in r) (euq s)


one, two :: Int
one = 1 
two = 2


d :: ByteString -> Value
d s = let (Just v) = decodeStrict s in v

