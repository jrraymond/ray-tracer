module Main where

import RayTracer

import Data.Binary
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Control.Concurrent
import Options.Applicative
import System.IO hiding (hPutStrLn)
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
loop opts nmax 0 t = warningM rootLoggerName (show opts ++ " not responding . . . sleeping") >>
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
    sHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering sHandle NoBuffering
    runConn sHandle 
    hClose sHandle

runConn :: Handle -> IO ()
runConn sHandle = do
  infoM rootLoggerName "sending initial request"
  B.hPut sHandle (strictEncode True)
  infoM rootLoggerName "waiting for number of world bytes"
  nbytes <- strictDecode <$> B.hGetSome sHandle 4
  infoM rootLoggerName "received number of number of bytes"
  wbs <- B.hGet sHandle nbytes
  infoM rootLoggerName "received world"
  rng <- newPureMT
  let w = strictDecode wbs
      grids = generateGrids rng (round (wImgWd w) + 10) (wAntiAliasing w)
  forever $ do
    infoM rootLoggerName "sending work request"
    B.hPut sHandle (strictEncode False)
    workBytes <- B.hGetSome sHandle 4
    work <- B.hGet sHandle (strictDecode workBytes)
    let (i,(start,step)) = strictDecode work :: (Int,(Int,Int))
        ps = map (mapT fromIntegral . fromIx (round (wImgWd w)) (round (wImgHt w)) 1) [start..step]
        img = map (colorPixel w) (zip ps grids)
        resp = strictEncode (i,img)
        respBytes = B.length resp
    B.hPut sHandle (strictEncode respBytes)
    B.hPut sHandle resp
    infoM rootLoggerName $ "sent completed (" ++ show respBytes ++ ") bytes"

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode

strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict

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


