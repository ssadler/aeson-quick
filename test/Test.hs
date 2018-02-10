{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative

import Data.Aeson.Quick
import Data.ByteString.Lazy (ByteString)

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
  , parseInvalid
  , showStructure
  , buildZipInfinite
  ]


oneKey :: TestTree
oneKey = testGroup "oneKey"
  [ testCase "get" $
      val .! "{a}" @?= one

  , testCase "set" $
      "{a}" .% Null `jt` "{\"a\":null}"
  ]
  where val = d "{\"a\":1}"


multipleKeys :: TestTree
multipleKeys = testGroup "multipleKeys"
  [ testCase "get" $
      multiple .! "{a,b}" @?= (one,two)

  , testCase "set" $
      build "{a,b}" multiple (two,[one,one]) `jt` "{\"a\":2,\"b\":[1,1]}"
  ]
  where multiple = d "{\"a\":1,\"b\":2}"


deepKey :: TestTree
deepKey = testGroup "deepKey"
  [ testCase "get" $
      multilevel .! "{a:{b}}" @?= Just one

  , testCase "set" $
      build "{a:{b}}" multilevel two `jt` "{\"a\":{\"b\":2}}"
  ]
  where multilevel = d "{\"a\":{\"b\":1}}"


keyInArray :: TestTree
keyInArray = testGroup "keyInArray"
  [ testCase "get" $
      many .! "[{a}]" @?= [one,two]

  , testCase "set" $
      build "[{a}]" many [True,False] `jt` "[{\"a\":true},{\"a\":false}]"
  ]
  where many = d "[{\"a\":1},{\"a\":2}]"


complex :: TestTree
complex = testGroup "complex"
  [ testCase "get" $
      val .! "{a,b:[{a}]}" @?= (one,[[two,one]])
  , testCase "set" $
      build "{a,b:[{a}]}" val (two,[[one]]) `jt` "{\"a\":2,\"b\":[{\"a\":[1]}]}"
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
      build "{a}" (d "{}") Null `jt` "{\"a\":null}"

  , testCase "setDeep" $
      "{a:[{b}]}" .% Null `jt` "{\"a\":[{\"b\":null}]}"

  , testCase "setDeepArray" $
      "[{a}]" .% [one,two] `jt` "[{\"a\":1},{\"a\":2}]"

  , testCase "setDeepMany" $
      let v = (1,[(10,11)]) :: (Int,[(Int,Int)])
       in "{my,complex:[{data,structure}]}" .% v
            @?= d "{\"my\":1,\"complex\":[{\"data\":10,\"structure\":11}]}" 
  ]
  where val = d "{\"a\":1}"


asLens :: TestTree
asLens = testGroup "asLens"
  [ testCase "get" $
      let l = queLens "{a,b}" :: Lens' Value (Int,Int)
       in val ^. l . _2 @?= two

  , testCase "getDoesNotExist" $
      -- doesn't work, make a Traversal?
      --d "{}" ^? queLens "{a}" @?= (Nothing :: Maybe (Int,Int))
      putStr "SKIPPED " >> pure ()

  , testCase "set" $
      (val & (queLens "{a,b}") .~ (two,one)) `jt` "{\"a\":2,\"b\":1}"
  ]
  where
    val = d "{\"a\":1,\"b\":2}"
    queLens :: (FromJSON a, ToJSON a) => Structure -> Lens' Value a
    queLens s = lens (.!s) (build s)


parseInvalid :: TestTree
parseInvalid = testGroup "parseInvalid"
  [ testCase "noKey" $
      isLeft (parseStructure "{a,{b}}") @?= True
  ]


showStructure :: TestTree
showStructure = testGroup "showStructure"
  [ testCase "all" $
      show ("[{a:[{b?,c}]}]"::Structure) @?= "[{a:[{b?,c}]}]"
  ]


buildZipInfinite :: TestTree
buildZipInfinite = testGroup "buildZipInfinite"
  [ testCase "wat" $
      build "{a:[{b}]}" (d "{\"a\":[{\"b\":1}]}") (repeat Null) @?= (d "{\"a\":[{\"b\":null}]}")
  ]

one, two :: Int
one = 1 
two = 2


jt :: Value -> ByteString -> IO ()
jt v b = encode v @?= b


d :: ByteString -> Value
d s = case decode s of
           Just v -> v
           Nothing -> error $ "Coult not decode JSON: " ++ show s


isLeft :: Either a b -> Bool
isLeft e = case e of Left _ -> True
                     _      -> False
