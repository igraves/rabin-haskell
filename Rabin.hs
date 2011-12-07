module Rabin where

import System.IO
import Primes
import Data.Word
import Data.Bits
import Data.List
import Debug.Trace
import qualified Data.Binary as B
import qualified Data.Binary.Get
import qualified Data.ByteString.Lazy as BS
import System.Random
import Math.NumberTheory.Moduli

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


msqrt :: Integer -> Integer -> IO (Integer,Integer)
msqrt p a = do
              d <- getB
              let a' = a `mod` p 
              let p1 = powerMod a' ((p+1) `div` 4) p::Integer
              let xp5 = powerMod a' ((p+3) `div` 8) p::Integer
              let cp5 = powerMod xp5 (2::Integer) p::Integer
              let xp5r = xp5 * powerMod 2 ((p-1) `div` 4) p::Integer
              let (twos,odd) = twofactor (p-1)
              let capA = powerMod a' odd p
              let capD = powerMod d odd p 
              let m = 0
              let for m i s = if i == s
                                 then (a^((odd+1) `div` 2) * capD^(m `div` 2)) `mod` p
                                 else if powerMod (capA * capD^m) (2^(s-1-i)::Integer) p == p-1
                                   then for (m + 2^i) (i+1) s
                                   else for m (i+1) s

              if p `mod` 8 == 3 || p `mod` 8 == 7
                then return (p1, -1 * p1)
                else if p `mod` 8 == 5 then
                       if cp5 /= a then return (xp5r, -1*xp5r) else return (xp5,-1*xp5)
                       else return (1,1)
            
    where
      getB = do
              genEntry <- getStdRandom (randomR (2::Integer,p-1))
              if jacobi (genEntry^2 - 4*a) p == -1 
                then return genEntry 
                else trace (show $ (genEntry)) getB

twofactor :: Integer -> (Integer,Integer)
twofactor i = twofactor' (0::Integer,i)

twofactor' :: (Integer,Integer) -> (Integer,Integer)
twofactor' (p,t) = if testBit t 0
                    then (p,t) 
                    else twofactor' (p+1,t `div` 2)
                      
