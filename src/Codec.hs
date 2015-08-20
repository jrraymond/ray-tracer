module Codec where

import Control.Monad
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word32)
import Data.Binary (Binary,encode,decode)
import Data.Typeable
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as NSBS


data XmitException = LengthException String
                   | UnexpectedEnd String
                   deriving (Show, Typeable)

instance Exception XmitException

newtype MsgLen = MsgLen Word32 deriving (Read,Show,Eq)

maxLen :: Int64
maxLen = 0x0FFFFFFFF


lenLen :: Int64
lenLen = 4

msgLenEOT :: MsgLen
msgLenEOT = MsgLen 0

isLenEOT :: Int64 -> Bool
isLenEOT = (== 0)

validateMsgLen :: Int64 -> Maybe MsgLen
validateMsgLen len
  | len <= maxLen && len > 0 = Just . MsgLen . fromIntegral $ len
  | otherwise = Nothing

encodeLen :: MsgLen -> ByteString
encodeLen (MsgLen w) = encode w

decodeLen :: ByteString -> Int64
decodeLen bs = assert (BL.length bs == lenLen) $ fromIntegral (decode bs :: Word32)

sendMsg :: (Show a, Binary a) => Socket -> a -> IO (Int64,Int64)
sendMsg skt myData =
  let bsMsg = encode myData
      len = BL.length bsMsg
  in case validateMsgLen len of
       Nothing -> throwIO (LengthException "msg len out of range")
       Just msgLen -> do
         print $ "send: " ++ show (len :: Int64,msgLen :: MsgLen)
         print (encodeLen msgLen)
         bs0 <- NSBS.send skt (encodeLen msgLen)
         bytes <- NSBS.send skt bsMsg
         when (bytes /= len) $
           throwIO (LengthException "msg lengths do not match")
         return (bs0,bytes)

sendEOT :: Socket -> IO ()
sendEOT skt = do
  _ <- NSBS.send skt (encodeLen msgLenEOT)
  return ()

withSocket :: Family -> SocketType -> ProtocolNumber -> (Socket -> IO a) -> IO a
withSocket family sktType protocol =
  bracket (socket family sktType protocol)
          (\skt -> shutdown skt ShutdownBoth >> close skt)

withAccept :: Socket -> ((Socket,SockAddr) -> IO a) -> IO a
withAccept skt =
  bracket (accept skt)
          (\(connSkt,_) -> shutdown connSkt ShutdownBoth >> close connSkt)
                                                  

recMsg :: (Show a, Binary a) => Socket -> IO (Maybe a)
recMsg skt = do
  lenmsg <- NSBS.recv skt lenLen
  print lenmsg
  let len = decodeLen lenmsg
  print $ "rec: " ++ show (len :: Int64, fromIntegral len :: Word32)
  if isLenEOT len
    then return Nothing
    else do
      bs <- NSBS.recv skt len
      return $ Just (decode bs)
