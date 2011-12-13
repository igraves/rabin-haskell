{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Client where
import Prelude hiding (catch)
import Numeric
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Socket (setSocketOption, SocketOption(..))
import Data.Word
import Data.IORef
import System.IO
import Control.Concurrent
import System.Posix.Signals
import System.Exit
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Protocol
import Rabin hiding (main)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.MD5
import System.IO.Unsafe
import Debug.Trace
import Control.Exception


main addr tmsg = do
                   withSocketsDo $ conn addr tmsg 

conn host tmsg = do
                (p,q) <- getKeys
                sock <- socket AF_INET Stream 0
                setSocketOption sock ReuseAddr 1
                haddr <- inet_addr host
                connect sock (SockAddrInet 5555 haddr)
                (Hello key name) <- getFrame sock
                msgbytes <- encryptMsg p q tmsg 
                send sock msgbytes 
--                let (Message newmsg) = runGet (get::Get Protocol) $ repacklbs bytes
                
--                results <- processMsg p q newmsg 
                --putStr $ show bytes
                return ()


                               



testmessage = "Hello, there!"

