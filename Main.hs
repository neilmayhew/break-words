{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import BreakWords

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (value)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Trie
import Options.Applicative
import System.IO

import qualified Data.Text.Lazy.IO as T
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optInputFiles :: [FilePath]
  , optWordListFile :: FilePath
  , optOutputFile :: Maybe FilePath
  , optVerbose :: Bool
  , optDemo :: Bool
  }
  deriving (Show)

main :: IO ()
main = do

  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <- customExecParser
    ( prefs $ columns cols )
    ( info
      ( helper <*> do
          optWordListFile <- strOption $
            short 'w' <> long "word-list" <>
            metavar "WORDS" <>
            value "words.txt" <>
            help "A dictionary containing one word per line" <>
            showDefaultWith id
          optOutputFile <- optional . strOption $
            short 'o' <> long "output" <>
            metavar "FILE" <>
            help "Write output to FILE instead of stdout" <>
            showDefaultWith id
          optVerbose <- switch $
            short 'v' <> long "verbose" <>
            help "Show extra information on stderr"
          optDemo <- switch $
            long "demo" <>
            help "Use the built-in demonstration input"
          optInputFiles <- many . strArgument $
            metavar "FILE ..." <>
            help "Text files to operate on"
          pure Options{..}
      )
      ( fullDesc <> header "Find word breaks in text containing no spaces" )
    )

  wordList <- lines <$> readFile optWordListFile

  let
    dict = fromList . (singles ++) $ filter (\w -> all isLower w && all isAlpha w && notSingle w) wordList
    singles = pure <$> "aio"
    notSingle = not . null . drop 1
    verbose = when optVerbose . hPutStrLn stderr

    process input = do
      let
        blob = filter isAlpha . map toLower $ input
        parses = breakWords dict blob
        numEdges = sum $ map (length . snd) parses
        writeOutput = maybe T.putStr T.writeFile optOutputFile
      parsing <- timeEval numEdges
      verbose $ show numEdges <> " words in " <> show parsing
      displaying <- timeExecute $
        writeOutput . printDotGraph $ treeGraph blob parses
      verbose $ "Output produced in " <> show displaying

  preparation <- timeEval dict
  verbose $ "Dictionary built in " <> show preparation

  when optDemo $
    process "hand edit readability"

  for_ optInputFiles $
    process <=< readFile

treeGraph :: String -> [(Node, [Edge])] -> DotGraph Int
treeGraph blob parses =
  let
    nodes = [(n, show n) | (n, _) <- parses]
    edges = [(n, n + e, substring n e) | (n, es) <- parses, e <- es]
    substring n e = take e (drop n blob)
    params = quickParams
      { globalAttributes =
        [ GraphAttrs [RankDir FromLeft, Margin (DVal 0), Splines SplineEdges]
        , NodeAttrs [Shape Circle, FontName "Helvetica"]
        , EdgeAttrs [FontName "Helvetica"]
        ]
      }
  in
    graphElemsToDot params nodes edges

timeEval :: NFData a => a -> IO NominalDiffTime
timeEval = timeExecute . pure

timeExecute :: NFData a => IO a -> IO NominalDiffTime
timeExecute x = do
  start <- getPOSIXTime
  _ <- evaluate . force =<< x
  end <- getPOSIXTime
  return $ end - start

instance NFData Trie
