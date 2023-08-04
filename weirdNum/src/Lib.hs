{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
      randomNum
    ) where

import Network.HTTP.Req 
import Data.Aeson
import Data.Text
import Control.Monad.IO.Class (liftIO)
import System.Random


someFunc :: IO ()
someFunc = runReq defaultHttpConfig $ do
    let url = https "www.json.org" /: "json-en.html"
    response <- req GET url NoReqBody bsResponse mempty
    liftIO $ putStrLn $ "Response status poopy: " ++ show (responseStatusCode response)
--    liftIO $ putStrLn $ "Response body: " ++ unpack (responseBody response)


randomNum :: Int->Int->IO Int
randomNum minVal maxVal = do
    gen<-newStdGen
    let (randomVal, _)=randomR(minVal, maxVal) gen
    return randomVal
