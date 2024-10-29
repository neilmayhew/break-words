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
import Data.Set (Set)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Tree
import Data.Trie
import Options.Applicative
import System.IO

import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as T
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optInputFiles :: [FilePath]
  , optWordListFile :: FilePath
  , optVerbose :: Bool
  , optOutputStyle :: OutputStyle
  , optDemo :: Bool
  }
  deriving (Show)

data OutputStyle
  = List | Tree | Graph
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

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
          optOutputStyle <-
            flag' List
            (
              long "list" <>
              help "Display the parsing results as a list"
            ) <|>
            flag' Tree
            (
              long "tree" <>
              help "Display the parsing results as a tree"
            ) <|>
            flag' Graph
            (
              long "graph" <>
              help "Display the parsing results as a graph"
            )
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
        case optOutputStyle of
          List -> mapM_ (putStrLn . unwords) paths
          Tree -> putStr . drawTree $ Node "" parses
          Graph -> T.putStr . printDotGraph $ treeGraph parses
      verbose $ "Output produced in " <> show displaying

  preparation <- timeEval dict
  verbose $ "Dictionary built in " <> show preparation

  when optDemo $
    process "hand edit readability"

  for_ optInputFiles $
    process <=< readFile

treeGraph :: Forest String -> DotGraph Int
treeGraph parses =
  let
    edges = Set.toList $ treeEdges parses
    nodes = map (\n -> (n, show n)) . nub' . concatMap (\(n1, n2, _) -> [n1, n2]) $ edges
    nub' = Set.toList . Set.fromList
    params = quickParams
      { globalAttributes =
        [ GraphAttrs [RankDir FromLeft, Margin (DVal 0), Splines SplineEdges]
        , NodeAttrs [Shape Circle, FontName "Helvetica"]
        , EdgeAttrs [FontName "Helvetica"]
        ]
      }
  in
    graphElemsToDot params nodes edges

treeEdges :: Forest String -> Set (Int, Int, String)
treeEdges = Set.unions . map (go 0)
  where
    go i t =
      let l = rootLabel t
       in Set.insert (i, i + length l, l) . Set.unions $ go (i + length l) <$> subForest t

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
