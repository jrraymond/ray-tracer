module DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.Environment

distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable
      defaultHost = "localhost"
      defaultPort = "44444"
  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master", host] -> do
      backend <- initializeBackend host defaultPort rtable
      startMaster backend master
    [ "master", host, port ] -> do
      backend <- initializeBackend host port rtable
      startMaster backend master
    [ "slave" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend
    [ "slave", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startSlave backend
    [ "slave", host, port ] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> print "Usage: ray-tracer-cloud <master|slave> [ <ip_address> <port> ]"