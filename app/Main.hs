{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Lib

data Sample = Sample
    { fields :: String
    , filename :: String }

sample :: Parser Sample
sample = do
  fields <- strOption $
    long "fields"
    <> short 'f'
    <> metavar "FIELDS"
    <> value "all"
    <> help "Fields to include in key, use entire record if omitted"
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
    content <- T.readFile $ filename options
    let top = getTop content

    forM_ top $ \(name, i) -> do
        putStr $ show i ++ " "
        T.putStrLn name
