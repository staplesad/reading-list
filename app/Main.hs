{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Foldable
import Data.String
import Data.List (sort)
import System.Directory (listDirectory)
import System.Environment (getEnv)

port :: IO String
port = getEnv "PORT"

path :: FilePath
path = "data/"

dataFiles :: IO [FilePath]
dataFiles = listDirectory path

dropEnd :: Int -> String -> String
dropEnd n = reverse . drop n . reverse

fileToRoute :: String -> ScottyM ()
fileToRoute filename = get (fromString ("/" ++  formatFile filename)) $ file (path ++ filename)

formatFile :: FilePath -> String
formatFile = dropEnd 4

toInt :: String -> Int
toInt str = read str :: Int

toString :: Int -> String
toString = show

sortFiles :: [String] -> [String]
sortFiles fileList = map toString ( sort (map toInt fileList))

main :: IO ()
main =
  port >>= \portNumber ->
  dataFiles >>= \fileList ->
  scotty (read portNumber)  $ do
    get "/" $ file "frontend/index.html"
    get "/files.json" $ json $ sortFiles $ map formatFile fileList
    traverse_ fileToRoute fileList

