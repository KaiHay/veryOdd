{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Control.Concurrent
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Req
import Control.Monad
import Control.Concurrent.Chan
import Control.Exception

loopy :: Chan String->IO()
loopy iterN= do
    randomVal<-randomNum 1 100
    let valStr = show randomVal
    writeChan iterN valStr
    liftIO $ putStrLn $ "Number: " ++ show randomVal
    
loopy_2 iterN=do
    let api= https "api.axiom.co" /:"v1" /:"datasets" /:"veryodd" /:"ingest"
    let prompt::[Value]
        prompt = [object
            [ "String" .= iterN
            ]]

    let authHeader = "Bearer xaat-5d6d1bd8-6a0e-4e19-b15f-f1ca9ee10351"
    let contentTypeHeader = "application/json"

    let headers =
            header "Authorization" (authHeader) <>
            header "Content-Type" (contentTypeHeader)
    _<-runReq defaultHttpConfig $ req POST api (ReqBodyJson prompt) ignoreResponse headers
    return ()
    

numberMaker ::Chan String-> IO ()
numberMaker chanl= loop 1
    where
        loop iterN=do
            loopy chanl
            threadDelay (100000)
            loop(iterN+1)
loggingWorker::Chan String->IO()
loggingWorker a = forever $ catch f catcher
    where
        catcher::SomeException->IO()
        catcher e = do
            print e
     
        f=do  
            b<-readChan a
            loopy_2 b
            threadDelay(1000000)
      
    
       
main::IO()
main=do
    a<-newChan
    forkIO (loggingWorker a) 
    numberMaker a
    
