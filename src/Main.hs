
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

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

main :: IO ()
main = do
  putStrLn "Starting server ..."
  scotty 3000 $ routes
