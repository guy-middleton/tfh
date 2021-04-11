{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( getKeys
    , getCountsMap
    , getCounts
    , getCounts'
    , getCounts''
    , getTop
    , cat
    , buildMap
    , buildMap'
    , buildMap''
    , Key (..)
    , BKey (..)
    ) where

import Safe
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Text.Encoding         as T
-- import qualified Data.Text.ICU as ICU
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as B
import Data.Char



import GHC.Generics (Generic)
import Data.Hashable

data Key = Key { key :: !T.Text }
    deriving (Show, Eq, Generic)

instance Hashable Key

data BKey = BKey { bkey :: !B.ByteString }
    deriving (Show, Eq, Generic)

instance Hashable BKey

getKeys :: T.Text -> [Maybe T.Text]
getKeys ws = map (gf 0 . T.words) $ T.lines ws
    where gf n l = atMay l n

getKeys' :: T.Text -> [Maybe Key]
getKeys' ws = map (gf 0 . T.words) $ T.lines ws
    where gf n l = Key <$> atMay l n

getCountsMap :: [Maybe T.Text] -> HashMap.HashMap T.Text Int
getCountsMap xs = HashMap.fromListWith (+) [(x, 1 :: Int) | x <- catMaybes xs]

getCountsMap' :: [Maybe Key] -> HashMap.HashMap Key Int
getCountsMap' xs = HashMap.fromListWith (+) [(x, 1 :: Int) | x <- catMaybes xs]

getCounts :: HashMap.HashMap T.Text Int -> [(T.Text, Int)]
getCounts xs = take 10 $ sortOn (Down . snd) $ HashMap.toList xs

getCounts' :: HashMap.HashMap Key Int -> [(Key, Int)]
getCounts' xs = take 10 $ sortOn (Down . snd) $ HashMap.toList xs

getCounts'' :: HashMap.HashMap BKey Int -> [(BKey, Int)]
getCounts'' xs = take 10 $ sortOn (Down . snd) $ HashMap.toList xs

getTop :: T.Text -> [(T.Text, Int)]
getTop ws = getCounts $ getCountsMap $ getKeys ws

getTop' :: T.Text -> [(Key, Int)]
getTop' ws = getCounts' $ getCountsMap' $ getKeys' ws

getKeyFromLine :: T.Text -> Maybe T.Text
getKeyFromLine l = gf 0 . T.words $ l
    where gf n l = atMay l n

getKeyFromLine' :: T.Text -> Maybe Key
getKeyFromLine' l = gf 0 . T.words $ l
    where gf n l = Key <$> atMay l n

getKeyFromLine'' :: B.ByteString  -> Maybe BKey
getKeyFromLine'' l = gf 0 . B.words $ l
    where gf n l = BKey <$> B.copy <$> atMay l n

getKeyFromLine''' :: B.ByteString  -> Maybe BKey
getKeyFromLine''' l = gf 0 l
    where gf n l = Just $ BKey $ B.copy $ B.takeWhile (not . isSpace) l

cat :: T.Text -> T.Text -> T.Text
cat l r = case getKeyFromLine r of
    Nothing -> l
    (Just s) -> T.append l $ T.append " " s

buildMap :: HashMap.HashMap T.Text Int -> T.Text -> HashMap.HashMap T.Text Int
buildMap m t = case getKeyFromLine t of
    Nothing -> m
    (Just k) -> HashMap.insertWith (+) k 1 m

buildMap' :: HashMap.HashMap Key Int -> T.Text -> HashMap.HashMap Key Int
buildMap' m t = case getKeyFromLine' t of
    Nothing -> m
    (Just k) -> HashMap.insertWith (+) k 1 m

buildMap'' :: HashMap.HashMap BKey Int -> B.ByteString  -> HashMap.HashMap BKey Int
buildMap'' m t = case getKeyFromLine''' t of
    Nothing -> m
    (Just k) -> HashMap.insertWith (+) k 1 m
