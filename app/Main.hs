{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Foldable
import Data.String
import Data.List (sort)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)

port :: IO (Maybe String)
port = lookupEnv "PORT"

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
main = do
  portNumber <- port
  fileList <- dataFiles
  scotty (maybe 3000 read portNumber)  $ do
    get "/" $ file "frontend/index.html"
    get "/elm.js" $ file "frontend/elm.js"
    get "/style.css" $ file "frontend/style.css"
    get "/files.json" $ json $ sortFiles $ map formatFile fileList
    get "/olidMap.json" $ file "title_to_olid.json"
    traverse_ fileToRoute fileList

