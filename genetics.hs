{-# LANGUAGE DeriveGeneric #-}

module Genetics where

import System.Random
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import Control.Monad (replicateM)

-- | Neural network genome representation
data Genome = Genome
  { layers :: [Int]          -- Layer sizes
  , learningRate :: Double   -- Learning rate
  , dropout :: Double        -- Dropout rate
  , activation :: String     -- Activation function
  , fitness :: Double        -- Fitness score
  } deriving (Show, Generic)

type Population = [Genome]

-- | Initialize random population
initializePopulation :: Int -> IO Population
initializePopulation size = replicateM size randomGenome

-- | Create a random genome
randomGenome :: IO Genome
randomGenome = do
  gen <- newStdGen
  let (numLayers, gen1) = randomR (2, 5) gen
      (lr, gen2) = randomR (0.0001, 0.1) gen1
      (dr, gen3) = randomR (0.0, 0.5) gen2
  layerSizes <- replicateM numLayers $ randomRIO (32, 512)
  let activations = ["relu", "tanh", "sigmoid", "elu"]
  actIdx <- randomRIO (0, length activations - 1)
  return $ Genome
    { layers = layerSizes
    , learningRate = lr
    , dropout = dr
    , activation = activations !! actIdx
    , fitness = 0.0
    }

-- | Evaluate fitness for entire population
evaluatePopulation :: Population -> String -> IO Population
evaluatePopulation pop dataPath = do
  putStrLn $ "Evaluating population on " ++ dataPath
  mapM evaluateFitness pop

-- | Evaluate fitness of a single genome
evaluateFitness :: Genome -> IO Genome
evaluateFitness genome = do
  -- Simulate training and evaluation
  score <- randomRIO (0.5, 0.99) :: IO Double
  return genome { fitness = score }

-- | Select parents using tournament selection
tournamentSelect :: Population -> Int -> IO Genome
tournamentSelect pop tournamentSize = do
  tournament <- replicateM tournamentSize $ do
    idx <- randomRIO (0, length pop - 1)
    return $ pop !! idx
  return $ getBest tournament

-- | Get best genome from population
getBest :: Population -> Genome
getBest = head . sortBy (flip $ comparing fitness)

-- | Crossover two genomes
crossover :: Genome -> Genome -> IO Genome
crossover parent1 parent2 = do
  let midLayers = (layers parent1 ++ layers parent2) `div` 2
  useLr1 <- randomIO :: IO Bool
  useDr1 <- randomIO :: IO Bool
  useAct1 <- randomIO :: IO Bool
  return $ Genome
    { layers = take (length $ layers parent1) $ layers parent1 ++ layers parent2
    , learningRate = if useLr1 then learningRate parent1 else learningRate parent2
    , dropout = if useDr1 then dropout parent1 else dropout parent2
    , activation = if useAct1 then activation parent1 else activation parent2
    , fitness = 0.0
    }

-- | Mutate a genome
mutate :: Genome -> Double -> IO Genome
mutate genome mutationRate = do
  shouldMutate <- randomIO :: IO Bool
  if not shouldMutate || mutationRate < 0.01
    then return genome
    else do
      -- Mutate layer sizes
      newLayers <- mapM (\l -> do
        m <- randomIO :: IO Bool
        if m then randomRIO (32, 512) else return l) (layers genome)
      -- Mutate learning rate
      newLr <- randomRIO (0.0001, 0.1)
      return genome { layers = newLayers, learningRate = newLr, fitness = 0.0 }

-- | Run evolutionary algorithm
evolve :: Population -> Int -> String -> IO Population
evolve population 0 _ = return population
evolve population generations dataPath = do
  putStrLn $ "Generation " ++ show (100 - generations + 1) ++ "/100"
  -- Evaluate fitness
  evaluated <- evaluatePopulation population dataPath
  let best = getBest evaluated
  putStrLn $ "Best fitness: " ++ show (fitness best)
  -- Create next generation
  nextGen <- replicateM (length population) $ do
    parent1 <- tournamentSelect evaluated 5
    parent2 <- tournamentSelect evaluated 5
    offspring <- crossover parent1 parent2
    mutate offspring 0.1
  -- Recurse
  evolve nextGen (generations - 1) dataPath
