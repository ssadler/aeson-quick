{-# LANGUAGE DeriveGeneric #-}

module Data.Aeson.Quick.Internal where

import Control.DeepSeq
import GHC.Generics (Generic)


data Bounds =
    All
  | Single Int
  | Range Int (Maybe Int)
  deriving (Eq, Ord, Generic, Show)

instance NFData Bounds


