
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON, encode)
import System.Environment (getEnv)
import Database.MongoDB    (Action, Document, Value, Query, Pipe, Projector, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)
import Control.Applicative
import qualified Data.Text.Lazy as T
import Data.List.Split (splitOn)
import AesonBson

data LoginRequest = LoginRequest { username :: String, password :: String } deriving (Show, Generic)
instance FromJSON LoginRequest

runQuery :: Pipe -> Query -> IO [Document]
runQuery pipe query = access pipe master "dswacwc" (find query >>= rest)

routes pipe = do
  post "/login" $ do
    request <- jsonData :: ActionM LoginRequest
    doc <- liftIO $ runQuery pipe (select ["username" =: (username request), "password" =: (password request)] "users")
    case doc of
      [] -> json $ map aesonify []
      _ -> do
        churchdocs <- liftIO $ runQuery pipe (select ["region" =: (head $ reverse $ splitOn "_" (username request))] "churches")
        json $ map aesonify churchdocs


main :: IO ()
main = do
  -- read in environment vars
  port <- getEnv "PORT"

  pipe <- connect (host "127.0.0.1")

  putStrLn "Starting server ..."
  scotty (read port :: Int) $ do
    -- apply static asset delivery using the provided path
    middleware $ staticPolicy $ addBase "web/dist"
    routes pipe
