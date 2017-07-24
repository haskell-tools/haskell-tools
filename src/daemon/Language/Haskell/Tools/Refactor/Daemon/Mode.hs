module Language.Haskell.Tools.Refactor.Daemon.Mode where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import Control.Concurrent.Chan
import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import Data.Maybe

import Language.Haskell.Tools.Refactor.Daemon.Protocol

data WorkingMode a = WorkingMode { daemonConnect :: [String] -> IO a
                                 , daemonDisconnect :: a -> IO ()
                                 , daemonSend :: a -> ResponseMsg -> IO ()
                                 , daemonReceive :: a -> IO [Either String ClientMessage]
                                 }

socketMode :: WorkingMode (Socket,Socket)
socketMode = WorkingMode sockConn sockDisconnect sockSend sockReceive
  where
    sockConn finalArgs = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet (read (finalArgs !! 0)) iNADDR_ANY)
      listen sock 1
      (conn, _) <- accept sock
      return (sock,conn)
    sockDisconnect (sock,conn) = close conn >> close sock
    sockSend (_,conn) = sendAll conn . (`BS.snoc` '\n') . encode
    sockReceive (_,conn) = do
      msg <- recv conn 2048
      if not $ BS.null msg -- null on TCP means closed connection
        then do -- when (not isSilent) $ putStrLn $ "message received: " ++ show (unpack msg)
          let msgs = BS.split '\n' msg
           in return $ catMaybes $ map decodeMsg msgs
        else return []
      where decodeMsg :: ByteString -> Maybe (Either String ClientMessage)
            decodeMsg mess
              | BS.null mess = Nothing
              | otherwise = case decode mess of
                Nothing -> Just $ Left $ "MALFORMED MESSAGE: " ++ unpack mess
                Just req -> Just $ Right req

channelMode :: WorkingMode (Chan ResponseMsg, Chan ClientMessage)
channelMode = WorkingMode chanConn chanDisconnect chanSend chanReceive
  where
    chanConn _ = (,) <$> newChan <*> newChan
    chanDisconnect _ = return ()
    chanSend (send,_) resp = writeChan send resp
    chanReceive (_,recv) = (:[]) . Right <$> readChan recv
