{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import DistribUtils
import RayTracer
import HaObj

import qualified Control.Concurrent as C
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Control.Monad.Loops
import Data.List (sortOn)
import System.Random.Mersenne.Pure64

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Message = MsgWorld ProcessId World
             | MsgWork ProcessId (Int,Int)
             | MsgImg ProcessId [Color]
  deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
  MsgWorld from0 world <- expect
  say $ printf "world received from %s" (show from0)
  mypid <- getSelfPid
  rng <- liftIO newPureMT
  let grids = generateGrids rng (round (wImgWd world) + 10) (wAntiAliasing world)
  forever $ do
    MsgWork from (start,step) <- expect
    say $ printf "work %d-%d received from %s" start step (show from)
    let img = renderIxs grids world start step
    send from (MsgImg mypid img)

remotable ['pingServer]

getWorld :: Config -> IO World
getWorld c = do
  -- first we setup scene
  let c = bench6Config
  objs <- case cScene c of
            Nothing -> return []
            Just fname -> do ms <- parseObj fname
                             case ms of
                               Left e -> error (show e)
                               Right mesh -> return $ fromMesh (convertMesh mesh)
  let w = bench6World objs
   -- w = bench4World
  return w

-- <<master
master :: Config -> [NodeId] -> Process ()
master c peers = do
  w <- liftIO (getWorld c)
  let pSteps = getSteps (cImageWidth c) (cImageHeight c) (cChunks c)
  ndone <- liftIO $ C.newMVar 0
  todo <- liftIO C.newChan
  done <- liftIO (C.newChan :: IO (C.Chan (Int,[Color])))
  liftIO $ C.writeList2Chan todo (zip [0 :: Int ..] pSteps)

  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid

  forM_ ps $ \pid -> liftIO $
    C.forkIO $ do
      _ <- whileM_ (liftIO ((< length pSteps) <$> C.readMVar ndone)) $ do
            (wID,(start,stop)) <- liftIO $ C.readChan todo
            say $ printf "sending %d(%d-%d) to %s" wID start stop (show pid)
            send pid (MsgWork mypid (start,stop))
            MsgImg _ result <- expect
            liftIO $ C.writeChan done (wID,result)
            liftIO $ incMVar ndone
            say $ printf "received %d(%d-%d)" wID start stop
      return ()
  whileM_ (liftIO ((< length pSteps) <$> C.readMVar ndone))
    (liftIO (C.threadDelay 1000000))

  say "rendering complete, writing ppm"

  img <- liftIO $ concatMap snd . sortOn fst <$> readNextN done (length pSteps)
  liftIO $ writePPM "img.ppm" (round (wImgWd w)) (round (wImgHt w)) img

  say "all done"
  terminate


readNextN :: C.Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = C.readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)

incMVar :: C.MVar Int -> IO ()
incMVar mv = do
  i <- C.takeMVar mv
  C.putMVar mv (i + 1)

-- <<main
main :: IO ()
main = distribMain (master bench6Config) Main.__remoteTable
-- >>

getSteps :: Int -> Int -> Int -> [(Int,Int)]
getSteps wd ht chunks = go 0
  where
    num_ps = wd * ht
    step = num_ps `div` chunks
    go i | i + step >= num_ps = [(i,num_ps)]
         | otherwise = (i,step) : go (i + step)
