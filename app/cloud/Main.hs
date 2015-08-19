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
import qualified Data.Vector.Unboxed.Mutable as MU

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Message = MsgWorld ProcessId World
             | MsgWork ProcessId (Int,Int,Int,Int)
             | MsgImg ProcessId (Int,Int,[Color])
             | MsgAck ProcessId
  deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
  MsgWorld from0 world <- expect
  mypid <- getSelfPid
  send from0 (MsgAck mypid)
  rng <- liftIO newPureMT
  let grids = generateGrids rng (round (wImgWd world) + 10) (wAntiAliasing world)
  forever $ do
    MsgWork from (i,wID,start,step) <- expect
    let img = renderIxs grids world start step
    send from (MsgImg mypid (i,wID,img))
    say $ printf "completed %d(%d-%d)" wID start step

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

{- The use of MVars in this function is NOT thread safe. The only reason they're
- is because legacy and I'm too lazy to change them to a mutable value, but not
- so lazy as to not write this comment.
-}
master :: Config -> [NodeId] -> Process ()
master c peers = do
  w <- liftIO (getWorld c)
  let pSteps = getSteps (cImageWidth c) (cImageHeight c) (cChunks c)
      ntodo = length pSteps
  ndone <- liftIO $ C.newMVar 0
  nsent <- liftIO $ C.newMVar 0
  todo <- liftIO C.newChan
  done <- liftIO (C.newChan :: IO (C.Chan (Int,[Color])))
  liftIO $ C.writeList2Chan todo (zip [0 :: Int ..] pSteps)
  ready <- liftIO $ MU.replicate (length peers) True
  liftIO $ mapM_ print peers
  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid

  forM_ ps $ \pid -> do
    say $ printf "sending world to %s" (show pid)
    send pid (MsgWorld mypid w)

  waitForAck ps

  sendAll mypid todo ntodo nsent ready ps

  whileM_ (liftIO ((< ntodo) <$> C.readMVar ndone)) $ do
    sendAll mypid todo ntodo nsent ready ps
    MsgImg pid (i,wID,img) <- expect
    liftIO $ MU.write ready i True
    liftIO $ C.writeChan done (wID,img)
    liftIO $ incMVar ndone
    nc <- liftIO $ C.readMVar ndone
    say $ printf "received %d from %s" wID (show pid)
    say $ printf "%d / %d" nc ntodo

  say "rendering complete, writing ppm"

  img <- liftIO $ concatMap snd . sortOn fst <$> readNextN done ntodo
  liftIO $ writePPM "img.ppm" (round (wImgWd w)) (round (wImgHt w)) img

  say "all done"
  terminate

{- blocks until all processes have sent an acknowledge response. Required 
- since occasianaly work will be sent before the world arrives -}
waitForAck :: [ProcessId] -> Process ()
waitForAck [] = return ()
waitForAck ps = do
  m <- expect
  case m of
    MsgAck p -> waitForAck (filter (/= p) ps)
    _  -> say "MASTER received notack" >> terminate

{- sends work to every worker that is not occupied
- we only send a worker work if it is idle and there is work to be done
-}
sendAll :: ProcessId
        -> C.Chan (Int,(Int,Int))
        -> Int
        -> C.MVar Int
        -> MU.IOVector Bool
        -> [ProcessId]
        -> Process ()
sendAll mypid todo ntodo nsent ready ps = do
  numsent0 <- liftIO $ C.readMVar nsent
  say $ printf "ntodo %d nsent %d" ntodo numsent0
  forM_ (zip [0..] ps) $ \(i,pid) -> do
    numsent <- liftIO $ C.readMVar nsent
    idle <- liftIO $ MU.read ready i
    when (idle && numsent < ntodo) $ do
      (wID,(start,stop)) <- liftIO $ C.readChan todo
      say $ printf "sending %d(%d-%d) to %s" wID start stop (show pid)
      send pid (MsgWork mypid (i,wID,start,stop))
      liftIO $ incMVar nsent
      liftIO $ MU.write ready i False

readNextN :: C.Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = C.readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)

incMVar :: C.MVar Int -> IO ()
incMVar mv = do
  i <- C.takeMVar mv
  C.putMVar mv (i + 1)

getSteps :: Int -> Int -> Int -> [(Int,Int)]
getSteps wd ht chunks = go 0
  where
    num_ps = wd * ht
    step = num_ps `div` chunks
    go i | i + step >= num_ps = [(i,num_ps)]
         | otherwise = (i,step) : go (i + step)

main :: IO ()
main = distribMain (master bench6Config) Main.__remoteTable
