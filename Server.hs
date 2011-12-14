module Server where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.IORef
import Data.List
import System.IO
import Control.Concurrent
import System.Posix.Signals
import Protocol
import Rabin
import Control.Monad

main :: IO ()
main = do
         withSocketsDo $ listn 

reportSignal :: Socket -> ThreadId -> IORef Bool -> IO ()
reportSignal sock tid ref = do 
                              putStrLn "\nShutting down."
                              killThread tid
                              sClose sock 
                              writeIORef ref True
listn :: IO ()
listn = do
           term <- newIORef False
           sock <- socket AF_INET Stream 0 
           setSocketOption sock ReuseAddr 1
           bindSocket sock (SockAddrInet 5555 iNADDR_ANY)
           listen sock 2
           tid <- forkIO $ serveloop sock
           _ <- installHandler keyboardSignal (Catch $ reportSignal sock tid term) Nothing
           waitloop term

waitloop :: IORef Bool -> IO ()
waitloop term = do
                  end <- readIORef term
                  if end
                   then return ()
                   else waitloop term

serveloop :: Socket -> IO ()
serveloop sock = do
                   conn <- accept sock
                   serveCon conn
                   serveloop sock

--The connection procedure is here
serveCon :: (Socket,SockAddr) -> IO ()
serveCon (sock,_) = do
                          (p,q) <- getKeys 
                          sendKey sock (p*q)
                          --(Message msg) <- getFrame sock
                          --result <- processMsg p q msg 
                          --putStr $ "Message received: " ++ result ++ "\n"
                          handleMsg sock p q 
                          return ()
                          
handleMsg :: Socket -> Integer -> Integer -> IO ()
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
                            _ -> error "Received the wrong packet!"


