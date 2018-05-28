{-# LANGUAGE OverloadedStrings #-}

module Data.Thread where

import Control.Monad
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import Data.Text (Text, replace)
import Data.Scientific

data Thread = Thread
    { num :: Int
    , sub :: Maybe Text
    , replies :: Int
    , images :: Int
    , com :: Text
    } deriving Eq

instance Show Thread where
    show (Thread num sub rep img com) =
        show num ++ ", "
        ++ show rep ++ ", "
        ++ show sub ++ ", "
        ++ show com ++ "\n"

-- | replaces HTML code with ANSI escape codes
htmlToANSI :: Text -> Text
htmlToANSI =
      replace "\\r\\n" ""
    . replace "<br>" "\n"
    . replace "<b>" "\27[1m"
    . replace "</b>" "\27[21m"
    . replace "<u>" "\27[4m"
    . replace "</u>" "\27[24m"
    . replace "<i>" "\27[3m"
    . replace "</i>" "\27[23m"
    . replace "&gt;" ">"
    . replace "&lt;" "<"

-- json aux conversion functions
-- | Get Int out of json Value
getInt :: Value -> Maybe Int
getInt (Number n) = toBoundedInteger n
getInt _ = Nothing

-- | Get Text out of json Value
getText :: Value -> Maybe Text
getText (String s) = Just s
getText _ = Nothing

-- TODO find more efficient lensy way of doing this
jsonToThread :: Value -> Maybe Thread
jsonToThread xs = do
    num <- xs ^? key "no" >>= getInt
    let sub = xs ^? key "sub" >>= getText
    rep <- xs ^? key "replies" >>= getInt
    img <- xs ^? key "images" >>= getInt
    com <- htmlToANSI <$> (xs ^? key "com" >>= getText)
    return $ Thread num sub rep img com

-- | Get thread structures from a page
getThreads :: Value -> Maybe [Thread]
getThreads xs = fmap (catMaybes . fmap jsonToThread)
    (^.. values) <$> (xs ^? key "threads")

-- | Get all threads from initial list of pages
getAllThreads :: [Value] -> Maybe [Thread]
getAllThreads = fmap concat . traverse getThreads
