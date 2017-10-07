
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import System.Environment (getEnv)

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
data LoginResponse = LoginResponse { success :: String } deriving (Show, Generic)
data LoginRequest = LoginRequest { username :: String, password :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User
instance ToJSON LoginResponse
instance FromJSON LoginRequest

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

hello :: ActionM ()
hello = text "hello world"

users :: ActionM ()
users = text "users world"

greet = do
  name <- param "name"
  text ("hello " <> name <> " !")

userById = do
  id <- param "id"
  json $ filter (\user -> userId user == id) allUsers

routes :: ScottyM ()
routes = do
  get "/hello" $ hello
  get "/users/:id" $ userById
  get "/greet/:name" $ greet
  get "/allusers" $ json allUsers
  post "/login" $ do
    request <- jsonData :: ActionM LoginRequest
    json (LoginResponse {success = if (username request) == (password request) then "success" else "failure"})

main :: IO ()
main = do
  -- read in environment vars
  port <- getEnv "PORT"

  putStrLn "Starting server ..."
  scotty (read port :: Int) $ do
    -- apply static asset delivery using the provided path
    middleware $ staticPolicy $ addBase "web/dist"
    routes
