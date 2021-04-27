{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    where

import           Control.Monad
import qualified Data.HashMap.Strict        as H
import           Data.Semigroup             ((<>))
import           Lib
import           Options.Applicative
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.IO

data CmdLine = CmdLine { field    :: Int
                       , number   :: Int
                       , filename :: String
                       }

cmdline :: Parser CmdLine
cmdline = do
  field <- option auto $
    long "fields"
    <> short 'f'
    <> metavar "FIELD"
    <> help "Which field to count"
  number <- option auto $
    long "number"
    <> short 'n'
    <> metavar "Number"
    <> value 10
    <> help "Output line count"
  filename <- strArgument $
    metavar "FILENAME"
    <> help "input file name"
  pure CmdLine {..}

main :: IO ()
main = do
    let opts = info (cmdline <**> helper) $
          fullDesc
          <> progDesc "Read stuff from a file"
          <> header "tfh - read stats from web server logs"
    options <- execParser opts
    countsMap <- makeMap (filename options) (field options - 1)
    forM_ (getCounts countsMap $ number options) $ \(k, v) -> do
        putStrLn $ show v ++ " " ++ show k

makeMap :: FilePath -> Int -> IO (H.HashMap BKey Int)
makeMap filename n = withFile filename ReadMode $ \h -> do
    S.fold_ (buildMap n) H.empty id
    $ S.mapped Q.toStrict
    $ Q.lines
    $ Q.fromHandle h
