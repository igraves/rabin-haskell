{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Client where
import Prelude hiding (catch)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Protocol

main :: String -> String -> IO ()
main addr tmsg = do
                   withSocketsDo $ conn addr tmsg 

conn :: String -> String -> IO ()
conn host tmsg = do
                sock <- socket AF_INET Stream 0
                setSocketOption sock ReuseAddr 1
                haddr <- inet_addr host
                connect sock (SockAddrInet 5555 haddr)
                (Hello key _) <- getFrame sock
                msgbytes <- encryptMsg key tmsg 
                _ <- send sock msgbytes 
                return ()


