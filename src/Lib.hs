{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( BKey
    , buildMap
    , getCounts
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as B (ShortByteString, fromShort, toShort)
import           Data.Char             ()
import qualified Data.HashMap.Strict   as H
import           Data.Hashable         (Hashable)
import           Data.List             (sortOn)
import           Data.Ord              (Down (Down))
import           GHC.Generics          (Generic)
import           Safe                  (atMay)

newtype BKey = BKey { bkey :: B.ShortByteString }
  deriving (Eq, Generic)

instance Hashable BKey
instance Show BKey where
    show = B.unpack . B.fromShort . bkey

getCounts :: H.HashMap BKey Int -> Int -> [(BKey, Int)]
getCounts xs n = take n $ sortOn (Down . snd) $ H.toList xs

getKeyFromLine :: Int -> B.ByteString  -> Maybe BKey
getKeyFromLine n l = gf n . B.words $ l
    where gf n l = BKey . B.toShort <$> atMay l n

buildMap :: Int -> H.HashMap BKey Int -> B.ByteString  -> H.HashMap BKey Int
buildMap n m t = case getKeyFromLine n t of
    Nothing  -> m
    (Just k) -> H.insertWith (+) k 1 m
