module Server where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Socket (setSocketOption, SocketOption(..))
import Data.Word
import Data.IORef
import Data.List
import System.IO
import System.IO.Unsafe
import Control.Concurrent
import System.Posix.Signals
import System.Exit
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Protocol
import Rabin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.MD5
import Debug.Trace




smain = do
         withSocketsDo $ listn 

reportSignal sock tid ref = do 
                              putStrLn "\nShutting down."
                              killThread tid
                              sClose sock 
                              writeIORef ref True

listn = do
           term <- newIORef False
           sock <- socket AF_INET Stream 0 
           setSocketOption sock ReuseAddr 1
           bindSocket sock (SockAddrInet 5555 iNADDR_ANY)
           listen sock 2
           tid <- forkIO $ serveloop sock
           installHandler keyboardSignal (Catch $ reportSignal sock tid term) Nothing
           waitloop term

waitloop term = do
                  end <- readIORef term
                  if end
                   then return ()
                   else waitloop term

serveloop sock = do
                   conn <- accept sock
                   serveCon conn []
                   serveloop sock

--The connection procedure is here
serveCon (sock,_) rem = do
                          (p,q) <- getKeys 
                          sendKey sock (p*q)
                          (Message msg) <- getFrame sock
                          result <- processMsg p q msg 
                          --putStr $ "Message received: " ++ result ++ "\n"
                          return ()
                          
--
processMsg p q ct = do
                      msgs <- decrypt p q (roll $ LBS.unpack ct)
                      let !res = findbss msgs 
                      putStr "Found a match!"
                      return res 
    where
       testbs (d,m) x = let enstr = runPut $ put x 
                         in
                          unsafePerformIO $ (return (d == md5 enstr)) `catch` \_ -> return False

       findbss [] = error "No valid message received"
       findbss (x:xs) = let (d,m) = trace "Foo" $ decodemsg x 
                         in
                           if testbs (d,m) x then (trace m m) else findbss xs
