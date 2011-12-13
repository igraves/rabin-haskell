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
import Control.Monad
import Debug.Trace




main = do
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
                          --(Message msg) <- getFrame sock
                          --result <- processMsg p q msg 
                          --putStr $ "Message received: " ++ result ++ "\n"
                          handleMsg sock p q 
                          return ()
                          

handleMsg sock p q = do
                       first <- getFrame sock
                       case first of
                            (Message msg)   -> do
                                                 result <- processMsg p q msg
                                                 putStr $ "Message received: " ++ result ++ "\n"
                            (Dossier count) -> do
                                                  frames <- replicateM (fromIntegral count) (getFrame sock)
                                                  msgs <- mapM (\(Message msg) -> processMsg p q msg) frames
                                                  let final = concat msgs
                                                  putStr $ "Message received: " ++ final ++ "\n"


