{-# LANGUAGE OverloadedStrings #-}
module Main where

import RayTracer
import HaObj

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.List (sortOn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Options.Applicative
import System.IO hiding (hPutStrLn)
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
  logH <- fileHandler "Master.log" INFO >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler logH)
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
sender world ntodo ndone todo done sock0 = withSocketsDo $ loop sock0
  where
    loop :: Socket -> IO ()
    loop sock = do
      (sck,_) <- accept sock
      infoM rootLoggerName "Connection accepted!"
      sHandle <- socketToHandle sck ReadWriteMode
      _ <- forkIO $ runLoop sHandle
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone (loop sock)
    runLoop :: Handle -> IO ()
    runLoop sHandle = do
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone $ do
        ps <- readChan todo
        result <- try $ sendPixels sHandle world ps
        case result :: Either SomeException (Int,[Color]) of
          Right ps' -> writeChan done ps' >>
                       incMVar ndone >>
                       runLoop sHandle
          Left _ -> writeChan todo ps >>
                    runLoop sHandle


incMVar :: MVar Int -> IO ()
incMVar mv = do
  i <- takeMVar mv
  putMVar mv (i + 1)

sendPixels :: Handle -> World -> (Int,(Int,Int)) -> IO (Int,[Color])
sendPixels sHandle world ps = do
  req <- B.hGetSome sHandle 1
  when (strictDecode req) $ do
    infoM rootLoggerName "received world request"
    let wstr = strictEncode world
    B.hPut sHandle (strictEncode (B.length wstr))
    B.hPut sHandle wstr
    infoM rootLoggerName "sent world"
  let pstr = strictEncode ps
  B.hPut sHandle (strictEncode (B.length pstr))
  B.hPut sHandle pstr
  putStrLn $ "sent work " ++ show (B.length pstr)
  rb <- B.hGetSome sHandle 4
  let rbytes = strictDecode rb
  resp <- B.hGet sHandle rbytes
  putStrLn $ "received " ++ show rbytes
  return $ strictDecode resp


strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode
strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict


readNextN :: Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)
