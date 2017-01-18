module Data.Aeson.Quick.Unsafe where


{- |
The __unsafe__ version of '.?'. This can fail very easily, do not use it
in your program.
-}
(.!) :: FromJSON a => Value -> Structure -> a
(.!) v s = either err id $ AT.parseEither (extract s) v
  where err msg = error $ show s ++ ": " ++ msg ++ " in " ++ show v
{-# INLINE (.!) #-}


