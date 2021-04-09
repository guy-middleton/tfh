{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Control.Monad
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           Lib
import System.IO
import qualified Data.HashMap.Strict as HashMap

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
    -- withFile (filename options) ReadMode $ \h -> do
    --     is <- Streams.handleToInputStream h >>= Streams.lines
    --     os <- Streams.unlines Streams.stdout
    --     Streams.connect is os
    counts <- Streams.withFileAsInput (filename options) $ do
        Streams.lines >=> Streams.decodeUtf8 >=> Streams.fold buildMap' HashMap.empty
    print $ getCounts' counts
