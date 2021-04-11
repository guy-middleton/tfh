{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Data.Semigroup ((<>))
import           System.IO
import qualified Data.HashMap.Strict as HashMap
import           Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString.Char8 as Q
import           Lib

data Sample = Sample
    { filename :: String }

sample :: Parser Sample
sample = do
  filename <- strArgument $
    metavar "FILENAME"
    <> help "input file name"
  pure Sample {..}

main :: IO ()
main = do
    let opts = info (sample <**> helper) $
          fullDesc
          <> progDesc "Read stuff from a file"
          <> header "tfh - read stats from web server logs"
    options <- execParser opts
    out <- count (filename options)
    print $ getCounts out

count :: FilePath -> IO (HashMap.HashMap BKey Int)
count filename = withFile filename ReadMode $ \h -> do
    S.fold_ buildMap HashMap.empty id
    $ mapped Q.toStrict
    $ Q.lines
    $ Q.fromHandle h
