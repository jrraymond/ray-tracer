{-# LANGUAGE OverloadedStrings #-}
module Main where

import RayTracer
import HaObj
import Codec

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops (whileM_)
import Data.List (sortOn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Options.Applicative
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple

main :: IO ()
main = execParser opts >>= masterMain
  where opts = info (helper <*> configure)
                    (fullDesc <> header "Ray Tracer")

masterMain :: Config -> IO ()
masterMain _ = withSocketsDo $ do
  logH <- fileHandler "master.log" INFO >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
  updateGlobalLogger rootLoggerName (setLevel INFO . addHandler logH)
  let c = bench6Config
  objs <- case cScene c of
            Nothing -> return []
            Just fname -> do ms <- parseObj fname
                             case ms of
                               Left e -> error (show e)
                               Right mesh -> return $ fromMesh (convertMesh mesh)
  let w = bench6World objs
   -- w = bench4World
      pSteps = getSteps (cImageWidth c) (cImageHeight c) (cChunks c)
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
  listen sock 5
  infoM rootLoggerName "Listening on port 3000"
  img <- run w pSteps sock
  writePPM "img.ppm" (cImageWidth c) (cImageHeight c) img

getSteps :: Int -> Int -> Int -> [(Int,Int)]
getSteps wd ht chunks = go 0
  where
    num_ps = wd * ht
    step = num_ps `div` chunks
    go i | i + step >= num_ps = [(i,num_ps)]
         | otherwise = (i,step) : go (i + step)

run :: World -> [(Int,Int)] -> Socket -> IO [Color]
run w pSteps sock = do
  let ntodo = length pSteps
  ndone <- newMVar 0
  todo <- newChan :: IO (Chan (Int,(Int,Int)))
  done <- newChan :: IO (Chan (Int,[Color]))
  writeList2Chan todo (zip [0 :: Int ..] pSteps)
  sender w ntodo ndone todo done sock
  concatMap snd . sortOn fst <$> readNextN done ntodo


{- ntodo is the number of todos to be evaluated and should not change
- ndone is the number of dones completed can goes from 0 to ntodo
- todo is a channel containing all the todos yet to be done
- done is a channel containing all the dones completed
-}
sender :: World
       -> Int
       -> MVar Int
       -> Chan (Int,(Int,Int))
       -> Chan (Int,[Color])
       -> Socket
       -> IO ()
sender world ntodo ndone todo done sock0 = 
  whileM_ ((< ntodo) <$> readMVar ndone) $ do
    (skt,_) <- accept sock0
    infoM rootLoggerName "Connection accepted!"
    _ <- forkIO $ runLoop skt
    infoM rootLoggerName (show ntodo ++ " completed")
  where
    runLoop :: Socket -> IO ()
    runLoop sock = do
      bs0 <- sendMsg sock world
      infoM rootLoggerName $ "sent world " ++ show bs0
      whileM_ ((< ntodo) <$> readMVar ndone) $ do
        ps <- readChan todo
        result <- try $ sendPixels sock ps
        case result :: Either SomeException (Int,[Color]) of
          Right ps' -> writeChan done ps' >>
                       incMVar ndone >>
                       infoM rootLoggerName (show ps ++ " succesful")
          Left e -> infoM rootLoggerName
                          (show e ++ ": " ++ show ps ++ " failed") >>
                    writeChan todo ps


incMVar :: MVar Int -> IO ()
incMVar mv = do
  i <- takeMVar mv
  putMVar mv (i + 1)

sendPixels :: Socket -> (Int,(Int,Int)) -> IO (Int,[Color])
sendPixels sock ps = do
  bs0 <- sendMsg sock ps
  infoM rootLoggerName $ "sent work " ++ show bs0
  result <- recMsg sock :: IO (Maybe (Int,[Color]))
  infoM rootLoggerName  "received result"
  case result of
    Nothing -> throwIO (UnexpectedEnd "result was Nothing")
    Just cs -> return cs


readNextN :: Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)
