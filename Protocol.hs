{-# LANGUAGE ScopedTypeVariables #-}

module Protocol where
import Prelude hiding (catch)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import System.IO.Unsafe
import System.Random
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS 
import Data.Digest.Pure.MD5
import Rabin


type Key = Integer
type Name = String
type Messages = Word32
type Payload = Integer

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
                --putStr $ "Frame size: " ++ (show l) ++ "\n"
                msgbytes <- recv sock (fromIntegral l)
                let msg = runGet (get::Get Protocol) $ repacklbs msgbytes
                return msg


sendKey :: Socket -> Integer -> IO ()
sendKey sock n = do
                let msg = Hello n "Rabin-Server"
                    smsg = runPut (put msg)
                    len = (fromIntegral $ LBS.length smsg)::Word32
                    lbytes = runPut $ putWord32le len
                _ <- send sock $ repackbs lbytes
                _ <- send sock $ repackbs smsg
                return ()


--Repack the bytestring to a lazy bytestring
repacklbs :: BS.ByteString -> LBS.ByteString
repacklbs = LBS.pack . BS.unpack

repackbs :: LBS.ByteString -> BS.ByteString
repackbs = BS.pack . LBS.unpack

--Encoding and decoding messages for the message payload
encodemsg :: String -> LBS.ByteString
encodemsg m = let bs = runPut $ put m
                  digst = md5 bs
                  msg = runPut $ put (digst,m)
               in 
                 msg


decodemsg :: LBS.ByteString -> IO (Maybe (MD5Digest,String))
decodemsg m = (return $ Just $ (\z y -> runGet y z) m $ do
                                                        (digest,msg) <- get
                                                        return (digest,msg))


processMsg :: Integer -> Integer -> Integer -> IO String
processMsg p q ct = do
                       (ms) <- (decrypt p q ct)
                       res <- findbss ms 
                       return res 
     where
        findbss :: [LBS.ByteString] -> IO String
        findbss [] = error "No valid message received"
        findbss (x:xs) = do
                           valid <- bigtest x
                           case valid of
                              True -> do 
                                              Just (_,m) <- (decodemsg x) 
                                              (return m) 
                              False -> findbss xs
       
        bigtest bs = do
                        let (a,bs', _) = runGetState (get::Get MD5Digest) bs 0
                        return $ a == md5 bs' 

encryptMsg ::  Integer -> String -> IO BS.ByteString
encryptMsg n msg = if length msg > 200 
                      then do
                             encryptLongMsg n msg
                      else do
                             encryptPackageMsg n msg

encryptPackageMsg :: Integer -> String -> IO BS.ByteString
encryptPackageMsg n msg = do
                              let prpmsg = encodemsg msg 
                              epyld <- encrypt n $ prpmsg
                              let pmsg = Message $ epyld
                              let bytes = repackbs $ runPut $ put pmsg
                              let size = repackbs $ runPut $ putWord32le ((fromIntegral $ BS.length bytes)::Word32)
                              return $ BS.append size bytes

encryptLongMsg :: Integer -> String -> IO BS.ByteString
encryptLongMsg n msg = do
                           let strs = chunkStr msg
                           msgs <- mapM (encryptPackageMsg n) strs
                           let doss = repackbs $ runPut $ put $ Dossier (fromIntegral $ length msgs)
                           let dsize = repackbs $ runPut $ putWord32le ((fromIntegral $ BS.length doss))
                           let doss' = BS.append dsize doss
                           let msgs' = foldr BS.append BS.empty msgs
                           return $ BS.append doss' msgs' 

chunkStr :: String -> [String]
chunkStr str = chunk str []
  where chunk [] r = r
        chunk s r = let s' = drop 200 s
                        r' = take 200 s
                     in
                      chunk s' (r ++ [r'])
