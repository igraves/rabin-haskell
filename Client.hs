module Client where

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


main = do
         withSocketsDo $ conn "127.0.0.1"

conn host = do
                (p,q) <- getKeys
                --sock <- socket AF_INET Stream 0
                --setSocketOption sock ReuseAddr 1
                --haddr <- inet_addr host
                --connect sock (SockAddrInet 5555 haddr)
                --(Hello key name) <- getFrame sock
                --Prepare the message to send here
                let prpmsg = encodemsg testmessage 
                epyld <- encrypt (p*q) $ prpmsg
                let pmsg = Message $ epyld
                let bytes = repackbs $ runPut $ put pmsg
                let size = repackbs $ runPut $ putWord32le ((fromIntegral $ BS.length bytes)::Word32)
--                send sock size
--                send sock bytes 
                let (Message newmsg) = runGet (get::Get Protocol) $ repacklbs bytes
                results <- processMsg p q newmsg 
                return ()




testmessage = "Hello, there!"

processMsg p q ct = do
                       msgs <- decrypt p q (ct)
                       let !res = findbss msgs 
                       putStr "Found a match!"
                       return res 
     where
        testbs (d,m) x = let enstr = runPut $ put x 
                          in
                           unsafePerformIO $ (return (d == md5 enstr)) `catch` \_ -> return False
 
        findbss [] = error "No valid message received"
        findbss (x:xs) = let (d,m) = decodemsg x 
                          in
                            if testbs (d,m) x then (trace m m) else findbss xs                
