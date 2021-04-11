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
import Streaming
import Streaming.Prelude (yield, next, each, for, with, subst)
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString.Char8 as Q
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B


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

    out <- withFile (filename options) ReadMode $ \h -> do
        -- Q.stdout
        -- $ Q.unlines
        -- $ subst Q.chunk
        S.fold_ buildMap'' HashMap.empty id
        $ mapped Q.toStrict
        $ Q.lines        -- Stream (ByteString m) m () -- divided into Stream layers
        $ Q.fromHandle h -- ByteString m ()            -- raw bytes

    print $ getCounts'' out
    -- print $ getCounts' counts
