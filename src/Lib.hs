{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( getCounts
    , buildMap
    , BKey (..)
    ) where

import           Safe
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as B
import           GHC.Generics (Generic)
import           Data.Hashable

data BKey = BKey { bkey :: !B.ByteString }
    deriving (Show, Eq, Generic)

instance Hashable BKey

getCounts :: HashMap.HashMap BKey Int -> [(BKey, Int)]
getCounts xs = take 10 $ sortOn (Down . snd) $ HashMap.toList xs

getKeyFromLine :: B.ByteString  -> Maybe BKey
getKeyFromLine l = gf 0 . B.words $ l
    where gf n l = BKey <$> B.copy <$> atMay l n

getKeyFromLine' :: B.ByteString  -> Maybe BKey
getKeyFromLine' l = gf 0 l
    where gf n l = Just $ BKey $ B.copy $ B.takeWhile (not . isSpace) l

buildMap :: HashMap.HashMap BKey Int -> B.ByteString  -> HashMap.HashMap BKey Int
buildMap m t = case getKeyFromLine t of
    Nothing -> m
    (Just k) -> HashMap.insertWith (+) k 1 m
