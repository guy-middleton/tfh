module Lib
    ( getKeys
    , getCountsMap
    , getCounts
    , getTop
    ) where

import Safe
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
-- import qualified Data.Text.ICU as ICU
import qualified Data.HashMap.Strict as HashMap

getKeys :: T.Text -> [Maybe T.Text]
getKeys ws = map (gf 0 . T.words) $ T.lines ws
    where gf n l = atMay l n

-- incl :: (Eq k, Hashable k, Num a) => HashMap.HashMap k a -> k -> HashMap.HashMap k a
-- incl m k = HashMap.insertWith (+) k 1 m

-- getCountsMap :: Num a => [Maybe T.Text] -> HashMap.HashMap T.Text a
-- getCountsMap xs = foldl' incl HashMap.empty $ catMaybes xs

getCountsMap :: [Maybe T.Text] -> HashMap.HashMap T.Text Int
getCountsMap xs = HashMap.fromListWith (+) [(x, 1 :: Int) | x <- catMaybes xs]

getCounts :: HashMap.HashMap T.Text Int -> [(T.Text, Int)]
getCounts xs = take 10 $ sortOn (Down . snd) $ HashMap.toList xs

getTop :: T.Text -> [(T.Text, Int)]
getTop ws = getCounts $ getCountsMap $ getKeys ws
