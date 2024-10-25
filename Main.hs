{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import BreakWords

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Time
import Data.Time.Clock.POSIX
import Data.Tree
import Data.Trie
import Options.Applicative
import System.IO

import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optInputFiles :: [FilePath]
  , optWordListFile :: FilePath
  , optVerbose :: Bool
  , optTree :: Bool
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
          optVerbose <- switch $
            short 'v' <> long "verbose" <>
            help "Show extra information on stderr"
          optTree <- switch $
            long "tree" <>
            help "Display the parsing results as a tree"
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
        numParses = sum $ map treeSize parses
        paths = concatMap treePaths parses
      parsing <- timeEval numParses
      verbose $ show numParses <> " parses in " <> show parsing
      displaying <- timeExecute $
        if optTree
          then putStr . drawTree $ Node "" parses
          else mapM_ (putStrLn . unwords) paths
      verbose $ "Output produced in " <> show displaying

  preparation <- timeEval dict
  verbose $ "Dictionary built in " <> show preparation

  when optDemo $
    process "hand edit readability"

  for_ optInputFiles $
    process <=< readFile

treePaths :: Tree String -> [[String]]
treePaths = foldTree go
  where
    go :: String -> [[[String]]] -> [[String]]
    go label [] = [[label]]
    go label subpaths = map (label:) $ concat subpaths

treeSize :: Tree String -> Int
treeSize = foldTree $ \_ bs -> sum bs + fromEnum (null bs)

timeEval :: NFData a => a -> IO NominalDiffTime
timeEval = timeExecute . pure

timeExecute :: NFData a => IO a -> IO NominalDiffTime
timeExecute x = do
  start <- getPOSIXTime
  _ <- evaluate . force =<< x
  end <- getPOSIXTime
  return $ end - start

instance NFData Trie
