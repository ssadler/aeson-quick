{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

import Control.Applicative
import Control.Monad

import Data.Aeson.Quick
import Data.Aeson.Quick.Internal
import Data.String

import Lens.Micro

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace


main :: IO ()
main = defaultMain $ testGroup "Tests" 
  [ oneKey
  , multipleKeys
  , deepKey
  , keyInArray
  , complex
  , optionalKey
  , nonExistentKey
  , parseInvalid
  , showQuick
  , quotedKey
  , quasi
  , props
  ]


oneKey :: TestTree
oneKey = testGroup "oneKey"
  [ testCase "get" $
      val .! "{a}" @?= one

  , testCase "set" $
      "{a}" .% Null @?= [jsonlit|{"a":null}|]
  ]
  where val = [jsonlit|{"a":1}|]


multipleKeys :: TestTree
multipleKeys = testGroup "multipleKeys"
  [ testCase "get" $
      multiple .! "{a,b}" @?= (one,two)

  , testCase "set" $
      (.%) "{a,b}" (two,[one,one]) @?= [jsonlit|{"a":2,"b":[1,1]}|]
  ]
  where multiple = [jsonlit|{"a":1,"b":2}|]


deepKey :: TestTree
deepKey = testGroup "deepKey"
  [ testCase "get" $
      multilevel .! "{a:{b}}" @?= Just one

  , testCase "set" $
      (.%) "{a:{b}}" two @?= [jsonlit|{"a":{"b":2}}|]
  ]
  where multilevel = [jsonlit|{"a":{"b":1}}|]


keyInArray :: TestTree
keyInArray = testGroup "keyInArray"
  [ testCase "get" $
      many .! "[{a}]" @?= [one,two]

  , testCase "set" $
      (.%) "[{a}]" [True,False] @?= [jsonlit|[{"a":true},{"a":false}]|]
  ]
  where many = [jsonlit|[{"a":1},{"a":2}]|]


complex :: TestTree
complex = testGroup "complex"
  [ testCase "get" $
      val .! "{a,b:[{a}]}" @?= (one,[[two,one]])
  , testCase "set" $
      (.%) "{a,b:[{a}]}" (two,[[one]]) @?= [jsonlit|{"a":2,"b":[{"a":[1]}]}|]
  ]
  where val = [jsonlit|{"a":1,"b":[{"a":[2,1]}]}|]


optionalKey :: TestTree
optionalKey = testGroup "optionalKey"
  [ testCase "get" $
      val .! "{a?,b?}" @?= (Just one, Nothing :: Maybe Int)
  ]
  where val = [jsonlit|{"a":1}|]


nonExistentKey :: TestTree
nonExistentKey = testGroup "nonExistentKey"
  [ testCase "get" $
      val .? "{b}" @?= (Nothing :: Maybe Int)
  
  , testCase "set" $
      (.%) "{a}" Null @?= [jsonlit|{"a":null}|]

  , testCase "setDeep" $
      "{a:[{b}]}" .% [Null] @?= [jsonlit|{"a":[{"b":null}]}|]

  , testCase "setDeepArray" $
      "[{a}]" .% [one,two] @?= [jsonlit|[{"a":1},{"a":2}]|]

  , testCase "setDeepMany" $
      let v = (1,[(10,11)]) :: (Int,[(Int,Int)])
       in "{my,complex:[{data,structure}]}" .% v
            @?= [jsonlit|{"my":1,"complex":[{"data":10,"structure":11}]}|]

  , testCase "inArray" do
      [jsonlit|[{"foo": 1}, {}, {"foo": 2}]|] .! "[{foo?}]" @?= [Just 1, Nothing, Just (2::Int)]
  ]
  where val = [jsonlit|{"a":1}|]


-- For now, Quick cannot be used as a lens
-- in order to update objects.
-- Perhaps revisit the old "build" function one day.

--asLens :: TestTree
--asLens = testGroup "asLens"
--  [ testCase "get" $
--      let l = queLens "{a,b}" :: Lens' Value (Int,Int)
--       in val ^. l . _2 @?= two
--
--  --, testCase "getDoesNotExist" $
--  --    -- doesn't work, make a Traversal?
--  --    --d "{}" ^? queLens "{a}" @?= (Nothing :: Maybe (Int,Int))
--  --    putStr "SKIPPED " >> pure ()
--
--  , testCase "set" $
--      (val & (queLens "{a,b}") .~ (two,one)) @?= [jsonlit|{"a":2,"b":1}"
--  ]
--  where
--    val = d "{"a":1,"b":2}"
--    queLens :: (FromJSON a, ToJSON a) => Quick -> Lens' Value a
--    queLens s = lens (.!s) ((.%) s)


parseInvalid :: TestTree
parseInvalid = testGroup "parseInvalid"
  [ testCase "noKey" $
      isLeft (parseQuick "{a,{b}}") @?= True
  ]


showQuick :: TestTree
showQuick = testGroup "showQuick"
  [ testCase "all" $
      show ("[{a:[{b?,c}]}]"::Quick) @?= "[{a:[{b?,c}]}]"
  ]

quotedKey :: TestTree
quotedKey = testGroup "quotedKey"
  [ testCase "chars" $
    ("{\"_:,}\"?}") @?= Obj [("_:,}", True, Val)]
  ]

quasi :: TestTree
quasi = testGroup "quasi-quoter"
  [ testCase "quasi" do
      [quick|.|] @?= Val
      [quick|{a:[.]0}|] @?= "{a:[.]0}"
  ]


instance Arbitrary Bounds where
  arbitrary = do
    r <- arbitrary
    case r of
      LT -> pure All
      EQ -> Single <$> choose (0, 100)
      GT -> Range <$> choose (0, 100) <*> (fmap abs <$> arbitrary)


instance Arbitrary Quick where
  arbitrary = sized arbQuick where
    arbKey = do
      i <- choose (1, 10)
      s <- replicateM i (elements ['a'..'z'])
      pure $ fromString s

    arbItem x = (,,) <$> arbKey <*> arbitrary <*> arbQuick x

    arbQuick 0 = pure Val
    arbQuick x
      | even x = Arr <$> arbQuick (quot x 2) <*> arbitrary
      | otherwise = do
          i <- choose (0, 3)
          Obj <$> replicateM i (arbItem (quot x 2))

    

props :: TestTree
props = testGroup "props"
  [
    testProperty "isomorphic parse" $
      \q -> fromString (show (q::Quick)) == q

  , testProperty "isomorphic show" $
      \q -> let s = show (q::Quick) in s == show (fromString s :: Quick)

  , testProperty "iso01" $
      \x -> let q = "[{a,b?}]" in (q .% x) .! q == (x :: [(Int, Maybe Int)])

  , testProperty "iso02" $
      \x -> let q = "[{a:[{d}],b}]" in (q .% x) .! q == (x :: [([Int], Int)])
  ]




one, two :: Int
one = 1 
two = 2


isLeft :: Either a b -> Bool
isLeft e = case e of Left _ -> True
                     _      -> False
