{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( getCounts
    , buildMap
    , BKey
    ) where

import           Safe
import           Data.List
import           Data.Ord
import           Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as B
         (ShortByteString, toShort, fromShort)
import           GHC.Generics (Generic)
import           Data.Hashable

data BKey = BKey { bkey :: !B.ShortByteString }
    deriving (Eq, Generic)

instance Hashable BKey
instance Show BKey where
    show = B.unpack . B.fromShort . bkey

getCounts :: HashMap.HashMap BKey Int -> Int -> [(BKey, Int)]
getCounts xs n = take n $ sortOn (Down . snd) $ HashMap.toList xs

getKeyFromLine :: Int -> B.ByteString  -> Maybe BKey
getKeyFromLine n l = gf n . B.words $ l
    where gf n l = BKey <$> B.toShort <$> atMay l n

buildMap :: Int -> HashMap.HashMap BKey Int -> B.ByteString  -> HashMap.HashMap BKey Int
buildMap n m t = case getKeyFromLine n t of
    Nothing -> m
    (Just k) -> HashMap.insertWith (+) k 1 m
