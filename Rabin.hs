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


msqrt :: Integer -> Integer -> IO Integer
msqrt p a = do
              if jacobi a p == 1 
               then do 
                      d <- getB
                      let (i,q) = twofactor (p-1) 
                      let z = d^q `mod` p
                          y = z
                          r = i
                          x = a^((q-1) `div` 2) `mod` p
                          b = a*x^2 `mod` p
                          x' = a*x
                      lpin (y,r,x',b) 
               else error "Square provided is not a quadratic residue of the supplied prime."
    where
      lpin (y,r,x,b) = if b `mod` p == 1
                          then return x
                          else if (getM b r) == r
                            then error "Quadratic residue failure."
                            else 
                               let m = getM b r
                                   t = (y^(2^(r-m-1))) `mod` p
                                   y' = (t^2) `mod` p
                                   r' = m `mod` p
                                   x' = (x*t) `mod` p
                                   b' = (b * y') `mod` p
                               in do
                                   lpin (y',r',x',b')
      getM b r = let nums = (delete r [1..2^r]) ++ [r]
                     pr x = (b^(2^x)::Integer) `mod` p == 1
                  in
                   srch pr nums
      srch pred (x:xs) = if (pred x) then x else srch pred xs              
      getB = do
              genEntry <- getStdRandom (randomR (3,2^30))
              if jacobi genEntry p == -1 
                then do 
                      --putStr $ "Using random n: " ++ show genEntry
                      return genEntry 
                else getB

--
twofactor :: Integer -> (Integer,Integer)
twofactor i = twofactor' (0::Integer,i)

twofactor' :: (Integer,Integer) -> (Integer,Integer)
twofactor' (p,t) = if testBit t 0
                    then (p,t) 
                    else twofactor' (p+1,t `div` 2)
                      
