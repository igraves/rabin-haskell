module Protocol where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy
import Data.Word
import System.IO.Unsafe
import System.Random

import Debug.Trace


type Key = [Word8] 
type Name = String
type Messages = Word32
type Payload = [Word8] 


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
                                getWord32le
                                return $ Hello key name
                    0xFFFB -> do
                                msg <- getWord32le 
                                getWord32le
                                return $ Dossier msg
                    0xFFFC -> do
                                msg <- get                              
                                getWord32le
                                return $ Message msg
                    0xFFFD -> do 
                                getWord32le
                                return Done 
                    0xFFFE -> do 
                                getWord32le 
                                return Ack
                    _ -> error "Unknown frame received."
     put (Hello key name) = do
                              putWord16le (0xFFFA::Word16)
                              put key
                              put name
                              putWord32le delimiter
     put (Dossier msgs)   = do
                              putWord16le (0xFFFB::Word16)
                              putWord32le msgs
                              putWord32le delimiter
     put (Message pyld)   = do
                              putWord16le (0xFFFC::Word16)
                              put pyld
                              putWord32le delimiter
     put (Done)           = do
                              putWord16le (0xFFFD::Word16)
                              putWord32le delimiter
     put (Ack)            = do
                              putWord16le (0xFFFE::Word16)
                              putWord32le delimiter

--Testing
--
randWord8 :: Int -> [Word8]
randWord8 0 = []
randWord8 n = let r = unsafePerformIO $ getStdRandom (randomR (0,255::Int))
               in
                (fromIntegral r) : (randWord8 (n-1))

