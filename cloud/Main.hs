{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

replyBack :: (ProcessId,String) -> Process ()
replyBack (sender,msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

main :: IO ()
main = do
  Right t <- createTransport "localhost" "1992" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  _ <- forkProcess node $ do
    echoPid <- spawnLocal $ forever $ 
      receiveWait [match logMessage, match replyBack]
    say "send some messages!"
    send echoPid "hello"
    self <- getSelfPid
    send echoPid (self,"hello")
    m <- expectTimeout 1000000
    case m of
      Nothing -> die "nothing came back"
      (Just s) -> say $ "got " ++ s ++ " back!"
    return ()
  liftIO $ threadDelay (2 * 1000000)
  return ()

{-
import Control.Distributed.Process hiding (Message)
import Control.Monad.IO.Class
import Control.Monad
import Text.Printf
import Control.Concurrent
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

-- <<Message
data Message = Ping (SendPort Message)
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: ProcessM ()
pingServer = do
  Ping chan <- expect
  mypid <- getSelfPid
  sendChannel chan (Pong mypid)
-- >>

-- <<remotable
$( remotable ['pingServer] )
-- >>

-- <<master
master :: ProcessM ()
master = do
  peers <- getPeers

  let workers = findPeerByRole peers "WORKER"

  ps <- forM workers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid pingServer__closure

  mypid <- getSelfPid

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChannel
    send pid (Ping sendport)
    return recvport

  oneport <- mergePortsBiased ports
  waitForPongs oneport ps

  say "All pongs successfully received"
  terminate

waitForPongs :: ReceivePort Message -> [ProcessId] -> ProcessM ()
waitForPongs port [] = return ()
waitForPongs port ps = do
  m <- receiveChannel port
  case m of
    Pong p -> waitForPongs port (filter (/= p) ps)
    _  -> say "MASTER received ping" >> terminate
-- >>

-- <<main
main :: IO ()
main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess

initialProcess :: String -> ProcessM ()
initialProcess "WORKER" = receiveWait []
initialProcess "MASTER" = master
-- >>
-- -}
