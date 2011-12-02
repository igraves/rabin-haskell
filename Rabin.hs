module Rabin where

import System.IO
import Primes
import Data.Word
import Data.Bits
import Data.List
import qualified Data.Binary as B
import qualified Data.Binary.Get
import qualified Data.ByteString.Lazy as BS


getKeys :: IO (Integer,Integer)
getKeys = do
            filecontent <- readFile "keys.txt"
            return $ read filecontent


testmsg :: [Word8]
testmsg = [0..255] 

main = do
          (p,q) <- getKeys
          let n = p * q
          let m = roll testmsg
          let ciphertext = m^2 `mod` n
          putStr $ show ciphertext




--Taken from Data.Binary Internal source code
roll :: [Word8] -> Integer
roll = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)
---End take
--


--Extended Euclidian from the Handbook of Applied Cryptography
--This is a gnarly fat tuple way of doing it without using my brain
--a -> b -> (d,x,y)
--
eeuclid :: Integer -> Integer -> (Integer,Integer,Integer)
eeuclid a 0 = (a,1,0)
eeuclid a b = let start = (0,0,0,1,1,0,a,b) 
                  (_,_,_,x2',_,y2',a',_) = eproc start
                  in (a',x2',y2')
  where 
    eproc (x,y,x1,x2,y1,y2,a,0) = (x,y,x1,x2,y1,y2,a,0)
    eproc (x,y,x1,x2,y1,y2,a,b) = let   q = a `div` b 
                                        r = a - q*b
                                        x' = x2 - q * x1
                                        y' = y2 - q * y1
                                        a'  = b
                                        b' = r
                                        x2' = x1
                                        x1' = x'
                                        y2' = y1
                                        y1' = y'
                                    in
                                      eproc (x',y',x1',x2',y1',y2',a',b')
                                      
