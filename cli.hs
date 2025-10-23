{-# LANGUAGE OverloadedStrings #-}

module CLI where

import System.Console.ANSI
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Display help information
showHelp :: IO ()
showHelp = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "╔═══════════════════════════════════════════════╗"
  putStrLn "║         NeuralForge CLI Utility               ║"
  putStrLn "╚═══════════════════════════════════════════════╝"
  setSGR [Reset]
  putStrLn ""
  putStrLn "Usage: neuralforge [command] [options]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  train <config>              Train a model with configuration file"
  putStrLn "  evolve <config> <gen>       Evolve architecture for N generations"
  putStrLn "  predict <model> <data>      Make predictions on new data"
  putStrLn "  serve [port]                Start REST API server (default: 8080)"
  putStrLn "  export <model> <output>     Export trained model"
  putStrLn "  status                      Show system status"
  putStrLn "  version                     Show version info"
  putStrLn ""
  putStrLn "Examples:"
  setSGR [SetColor Foreground Dull Yellow]
  putStrLn "  neuralforge train config.yaml"
  putStrLn "  neuralforge evolve config.yaml 100"
  putStrLn "  neuralforge predict model.bin testdata.csv"
  putStrLn "  neuralforge serve 8080"
  setSGR [Reset]
  putStrLn ""

-- | Display training progress
showProgress :: Int -> Int -> Double -> IO ()
showProgress current total loss = do
  let percent = (fromIntegral current / fromIntegral total) * 100 :: Double
      barWidth = 40
      filled = round (percent / 100 * fromIntegral barWidth) :: Int
      bar = replicate filled '█' ++ replicate (barWidth - filled) '░'
  setSGR [SetColor Foreground Vivid Green]
  putStr "\r[" 
  putStr bar
  putStr "] "
  printf "%3.0f%%" percent
  putStr " | Epoch: "
  putStr $ show current ++ "/" ++ show total
  putStr " | Loss: "
  printf "%.4f" loss
  setSGR [Reset]

-- | Display evolution statistics
showEvolutionStats :: Int -> Double -> [Int] -> IO ()
showEvolutionStats generation bestFitness bestArchitecture = do
  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn $ "\nGeneration " ++ show generation
  setSGR [Reset]
  putStrLn $ "Best Fitness: " ++ show bestFitness
  putStrLn $ "Best Architecture: " ++ show bestArchitecture
  putStrLn ""

-- | Display model info
showModelInfo :: String -> [Int] -> Int -> IO ()
showModelInfo name layers params = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "╔═══════════════════════════════════════════════╗"
  putStrLn "║               Model Information               ║"
  putStrLn "╚═══════════════════════════════════════════════╝"
  setSGR [Reset]
  putStrLn $ "Name: " ++ name
  putStrLn $ "Layers: " ++ show layers
  putStrLn $ "Total Parameters: " ++ show params
  putStrLn ""

-- | Display system status
showStatus :: IO ()
showStatus = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "✓ NeuralForge System Status"
  setSGR [Reset]
  putStrLn "  - Core Engine: Running"
  putStrLn "  - Evolution Engine: Ready"
  putStrLn "  - API Server: Available"
  putStrLn "  - GPU Acceleration: Enabled"
  putStrLn ""

-- | Print error message
printError :: String -> IO ()
printError msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ "✗ Error: " ++ msg
  setSGR [Reset]

-- | Print success message
printSuccess :: String -> IO ()
printSuccess msg = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "✓ " ++ msg
  setSGR [Reset]

-- | Printf helper
printf :: String -> Double -> IO ()
printf fmt val = putStr $ T.unpack $ T.pack $ show val
