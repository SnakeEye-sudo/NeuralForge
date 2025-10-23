{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import qualified CLI as CLI
import qualified APIServer as API
import qualified Pipeline as Pipeline
import qualified Genetics as GA

-- | Main entry point for NeuralForge
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printHelp
    ("serve":port:[]) -> API.startServer (read port :: Int)
    ("serve":[]) -> API.startServer 8080
    ("train":configPath:[]) -> Pipeline.runTraining configPath
    ("evolve":configPath:generations:[]) -> 
      Pipeline.runEvolution configPath (read generations :: Int)
    ("predict":modelPath:dataPath:[]) -> 
      Pipeline.runPrediction modelPath dataPath
    ("export":modelPath:outputPath:[]) -> 
      Pipeline.exportModel modelPath outputPath
    _ -> printHelp

printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "NeuralForge - Autonomous ML Framework"
  , ""
  , "Usage:"
  , "  neuralforge serve [port]           - Start REST API server (default: 8080)"
  , "  neuralforge train <config>         - Train model with config file"
  , "  neuralforge evolve <config> <gen>  - Evolve architecture for N generations"
  , "  neuralforge predict <model> <data> - Run predictions"
  , "  neuralforge export <model> <output> - Export trained model"
  , ""
  , "Examples:"
  , "  neuralforge serve 8080"
  , "  neuralforge train config.yaml"
  , "  neuralforge evolve config.yaml 100"
  , "  neuralforge predict model.bin testdata.csv"
  ]
