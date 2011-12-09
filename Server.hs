module Server where

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
import Data.Binary
import Data.ByteString hiding (putStrLn)






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


serveCon (sock,_) rem = error "not implemented"


parseMsg sock hbytes = do
                         lbytes <- recv sock 4096
                         let bs = append hbytes lbytes                        
                         return bs
