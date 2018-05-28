{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Control.Lens
import Data.Aeson
import Data.Thread
import Network.Wreq

someFunc :: IO ()
someFunc = do
    r <- get "http://a.4cdn.org/g/catalog.json"
    let mjsonCatalog = decode $ r ^. responseBody :: Maybe [Value]
        threads = mjsonCatalog >>= getAllThreads
    (mapM_ . mapM) print threads
