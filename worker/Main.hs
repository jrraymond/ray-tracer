module Main where

import RayTracer
import Codec

import Control.Exception
import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Control.Concurrent
import Options.Applicative
import System.IO.Error
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Random.Mersenne.Pure64


main :: IO ()
main = execParser opts >>= startWorker
  where
    opts = info (helper <*> optsParser)
                (fullDesc <> 
                 progDesc "works on raytracing part of an image" <> 
                 header "ray-tracer-worker" )


startWorker :: Opts -> IO ()
startWorker opts = withSocketsDo $ do
  logH <- fileHandler (_logF opts) INFO >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
  updateGlobalLogger rootLoggerName (setLevel INFO . addHandler logH)
  infoM rootLoggerName "Worker starting up"
  loop opts (_retries opts) (_retries opts) 1


-- t is how long to wait before retrying in seconds
loop :: Opts -> Int -> Int -> Int -> IO ()
loop opts nmax 0 _ = warningM rootLoggerName (show opts ++ " not responding . . . sleeping") >>
                     threadDelay (10 ^ (6 :: Int) * 2 ^ (9 :: Int)) >>
                     loop opts nmax nmax 1
loop opts nmax n t = catch (loopListen opts) handler
  where
    handler e
      | isDoesNotExistError e = warningM rootLoggerName (show e ++ " trying again in " ++ show t) >>
                                threadDelay (10 ^ (6 :: Int) * t) >>
                                loop opts nmax (n - 1) (t*2)
      | otherwise = noticeM rootLoggerName (show e) >> 
                    loop opts nmax nmax 1

loopListen :: Opts -> IO ()
loopListen (Opts ip port _ _) = do
    infos <- getAddrInfo Nothing (Just ip) (Just port)
    sock <- socket AF_INET Stream 0
    connect sock (addrAddress . head $ infos)
    runConn sock
    --todo socket closing and stuff

runConn :: Socket -> IO ()
runConn skt = do
  infoM rootLoggerName "waiting for world"
  worldM <- recMsg skt
  case worldM of
    Nothing -> throwIO (UnexpectedEnd "world was Nothing")
    Just world -> do
      rng <- newPureMT
      let grids = generateGrids rng (round (wImgWd world) + 10) (wAntiAliasing world)
      forever $ do
        workM <- recMsg skt :: IO (Maybe (Int,(Int,Int)))
        infoM rootLoggerName $ "received work " ++ show workM
        case workM of
          Nothing -> void (infoM rootLoggerName "workM nothing")
          Just (wID,(start,step)) -> do
            infoM rootLoggerName $ "rendering " ++ show start ++ " to " ++ show (start + step)
            let img = renderIxs grids world start step
            bs2 <- sendMsg skt (wID,img)
            infoM rootLoggerName $ "sent image " ++ show bs2




data Opts = Opts { _ip :: String
                 , _port :: String 
                 , _logF :: String
                 , _retries :: Int }

instance Show Opts where
  show (Opts ip port _ _) = ip ++ ':' : port

optsParser :: Parser Opts
optsParser = 
    Opts <$> strOption (short 'm' <> long "master" <>
                       metavar "MASTER_IP" <> value "localhost" <>
                       help "IP of master, default localhost")
         <*> strOption (short 'p' <> long "port" <>
                       metavar "MASTER_PORT" <> value "3000" <>
                       help "Port master is listening on, default 3000")
         <*> strOption (short 'l' <> long "log" <>
                       metavar "FILE" <> value "worker.log" <>
                       help "File to log messages to, default worker.log")
         <*> option auto (short 'r' <> long "retries" <>
                         metavar "RETRIES" <> value 10 <>
                         help "Retries after connection refused")


