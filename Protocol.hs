module Protocol where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import System.IO.Unsafe
import System.Random
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Socket (setSocketOption, SocketOption(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS 
import Data.Digest.Pure.MD5
import Debug.Trace


type Key = Integer
type Name = String
type Messages = Word32
type Payload = Integer


keysize = 2048
keybytes = (keysize * 2) `div` 8
msgsize = 255

delimiter = 0xDEADBEEF::Word32

data Protocol = Hello Key Name   | --0xFFFA
                Dossier Messages | --0xFFFB
                Message Payload  | --0xFFFC Payload will always be 255 bytes
                Done             | --0xFFFD
                Ack                --0xFFFE
                 deriving Show


instance Binary Protocol where
     get = do 
             flag <- getWord16le
             case (flag) of
                    0xFFFA -> do
                                key  <- get
                                name <- get 
                                return $ Hello key name
                    0xFFFB -> do
                                msg <- getWord32le 
                                return $ Dossier msg
                    0xFFFC -> do
                                msg <- get                              
                                return $ Message msg
                    0xFFFD -> do 
                                return Done 
                    0xFFFE -> do 
                                return Ack
                    _ -> error "Unknown frame received."
     put (Hello key name) = do
                              putWord16le (0xFFFA::Word16)
                              put key
                              put name
     put (Dossier msgs)   = do
                              putWord16le (0xFFFB::Word16)
                              putWord32le msgs
     put (Message pyld)   = do
                              putWord16le (0xFFFC::Word16)
                              put pyld
     put (Done)           = do
                              putWord16le (0xFFFD::Word16)
     put (Ack)            = do
                              putWord16le (0xFFFE::Word16)

--Testing
--
randWord8 :: Int -> [Word8]
randWord8 0 = []
randWord8 n = let r = unsafePerformIO $ getStdRandom (randomR (0,255::Int))
               in
                (fromIntegral r) : (randWord8 (n-1))

--Network helpers
getFrame :: Socket -> IO Protocol
getFrame sock = do
                lenbytes <- recv sock 4
                let l = runGet getWord32le $ repacklbs lenbytes 
                putStr $ "Frame size: " ++ (show l) ++ "\n"
                msgbytes <- recv sock (fromIntegral l)
                let msg = runGet (get::Get Protocol) $ repacklbs msgbytes
                return msg


sendKey sock n = do
                let msg = Hello n "Rabin-Server"
                    smsg = runPut (put msg)
                    len = (fromIntegral $ LBS.length smsg)::Word32
                    lbytes = runPut $ putWord32le len
                send sock $ repackbs lbytes
                send sock $ repackbs smsg


--rebuildMessage :: Int -> IO

--Repack the bytestring to a lazy bytestring
repacklbs = LBS.pack . BS.unpack

repackbs = BS.pack . LBS.unpack

--Encoding and decoding messages for the message payload
encodemsg :: String -> LBS.ByteString
encodemsg m = let bs = runPut $ put m
                  digst = md5 bs
                  msg = runPut $ put (digst,m)
               in 
                 msg


decodemsg :: LBS.ByteString -> (MD5Digest,String)
decodemsg m = unsafePerformIO $ do
                                  let (digst,m',_) = runGetState (get::Get MD5Digest) m 0
                                      (msg,_,_) = runGetState (get::Get String) m' 0
                                  d <- (return digst)
                                  msg' <- (return msg) `catch` \_ -> putStr "OH GOD\n" >> return ""
                                  return (d,msg')

