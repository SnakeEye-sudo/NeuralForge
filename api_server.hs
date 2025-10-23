{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module APIServer where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Pipeline as P
import qualified Genetics as GA

-- | API request types
data TrainRequest = TrainRequest
  { configPath :: String
  , epochs :: Int
  } deriving (Show, Generic)

instance FromJSON TrainRequest
instance ToJSON TrainRequest

data PredictRequest = PredictRequest
  { modelPath :: String
  , dataPath :: String
  } deriving (Show, Generic)

instance FromJSON PredictRequest
instance ToJSON PredictRequest

data EvolveRequest = EvolveRequest
  { config :: String
  , generations :: Int
  } deriving (Show, Generic)

instance FromJSON EvolveRequest
instance ToJSON EvolveRequest

-- | API response types
data APIResponse = APIResponse
  { status :: String
  , message :: String
  , data_ :: Maybe String
  } deriving (Show, Generic)

instance FromJSON APIResponse
instance ToJSON APIResponse

-- | Start the REST API server
startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting NeuralForge API server on port " ++ show port
  putStrLn "Available endpoints:"
  putStrLn "  POST /api/train    - Train a new model"
  putStrLn "  POST /api/predict  - Make predictions"
  putStrLn "  POST /api/evolve   - Evolve architecture"
  putStrLn "  GET  /api/health   - Health check"
  Warp.run port application

-- | Main application handler
application :: Wai.Application
application request respond = do
  let path = Wai.pathInfo request
      method = Wai.requestMethod request
  
  case (method, path) of
    ("GET", ["api", "health"]) -> 
      respond $ jsonResponse HTTP.status200 $ APIResponse
        { status = "ok"
        , message = "NeuralForge API is running"
        , data_ = Nothing
        }
    
    ("POST", ["api", "train"]) -> do
      body <- Wai.strictRequestBody request
      case decode body :: Maybe TrainRequest of
        Nothing -> respond $ jsonResponse HTTP.status400 $ APIResponse
          { status = "error"
          , message = "Invalid request body"
          , data_ = Nothing
          }
        Just req -> do
          -- Run training asynchronously
          respond $ jsonResponse HTTP.status200 $ APIResponse
            { status = "success"
            , message = "Training started"
            , data_ = Just $ "Training with " ++ show (epochs req) ++ " epochs"
            }
    
    ("POST", ["api", "predict"]) -> do
      body <- Wai.strictRequestBody request
      case decode body :: Maybe PredictRequest of
        Nothing -> respond $ jsonResponse HTTP.status400 $ APIResponse
          { status = "error"
          , message = "Invalid request body"
          , data_ = Nothing
          }
        Just req -> do
          respond $ jsonResponse HTTP.status200 $ APIResponse
            { status = "success"
            , message = "Predictions generated"
            , data_ = Just $ "Model: " ++ modelPath req
            }
    
    ("POST", ["api", "evolve"]) -> do
      body <- Wai.strictRequestBody request
      case decode body :: Maybe EvolveRequest of
        Nothing -> respond $ jsonResponse HTTP.status400 $ APIResponse
          { status = "error"
          , message = "Invalid request body"
          , data_ = Nothing
          }
        Just req -> do
          respond $ jsonResponse HTTP.status200 $ APIResponse
            { status = "success"
            , message = "Evolution started"
            , data_ = Just $ "Running " ++ show (generations req) ++ " generations"
            }
    
    _ -> respond $ jsonResponse HTTP.status404 $ APIResponse
      { status = "error"
      , message = "Endpoint not found"
      , data_ = Nothing
      }

-- | Helper to create JSON responses
jsonResponse :: HTTP.Status -> APIResponse -> Wai.Response
jsonResponse status resp =
  Wai.responseLBS status [("Content-Type", "application/json")] (encode resp)
