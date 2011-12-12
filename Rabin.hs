{-# LANGUAGE BangPatterns #-}
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
import Numeric (showHex)

getKeys :: IO (Integer,Integer)
getKeys = do
            filecontent <- readFile "keys.txt"
            return $ read filecontent


testmsg :: [Word8]

testmsg = [0..4] 



--31,43
main = do
          (p,q) <- getKeys
          let n = p * q
          let m = roll testmsg
          if m > n then error "M is too large." else return ()
          putStr $ "Message: " ++ (showHex m "") ++ "\n\n"
          let ciphertext = m^2 `mod` n
          putStr $ "Ciphertext: " ++ (showHex ciphertext "") ++ "\n\n"
          roots <- squareroots ciphertext p q
          putStr $ foldr (\x y -> y ++ (showHex x "") ++ "\n\n\n") "" roots
          putStr "Complete."

encrypt :: Integer -> BS.ByteString -> IO Integer
encrypt n msg = do
                  let m = roll $ BS.unpack msg 
                  if m > n then error "M is too large." else return ()
                  let ciphertext = m^2 `mod` n
                  return ciphertext

decrypt :: Integer -> Integer -> Integer -> IO [BS.ByteString]
decrypt p q ct = do
                  roots <- squareroots ct p q
                  return $ map (BS.pack . unroll) roots
                  
--Algorithm 3.44 from the book
squareroots m p q = do
                        let n = (p*q)^2
                        (r,r',s,s') <- sqrts m p q
                        let (z,c,d) = eeuclid p q
                        let x = (r*d*q + s*c*p) `mod` n
                        let y = (r*d*q - s*c*p) `mod` n
                        let !x1 = x `mod` n
                        let !x2 = -1 * x1 + n
                        let !y1 = y `mod` n
                        let y2 = -1 * y1 + n
                        return [x1,x2,y1,y2]
                      
              

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

sqrts ::  Integer -> Integer -> Integer -> IO (Integer,Integer,Integer,Integer)
sqrts a p q  = do
                   let n = (p*q)^2
                   let r =  msqrt3 a p 
                   let s =  msqrt3 a q 
                   let s' = q - s
                   let r' = p - r
                   return (r,r',s,s')



--For chosen p's and q's === 3 `mod` 4
--Requires that your keys are Gaussian Primes
msqrt3 :: Integer -> Integer -> Integer
msqrt3 c p = smpow c ((p+1) `div` 4) p --c^((p+1) `div` 4) `mod` p


msqrt :: Integer -> Integer -> IO Integer
msqrt a p = do
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
                      trace ("Loop Entry") $ lpin (y,r,x',b) 
               else error "Square provided is not a quadratic residue of the supplied prime."
    where
      lpin (y,r,x,b) = if trace ("Entry If") $ (trace ("trying the mod") $ (trace ("trying b") b) `mod` (trace ("trying p") p)) == 1
                          then trace ("Returning x") $ return x
                          else if trace ("GetM") $ (getM (trace "Trying b" b)  (trace "Trying R" r)) == r
                            --This is probably unreachable given we check the legendre symbol at the beginning
                            then error "Quadratic residue failure."
                            else 
                               let m = trace ("m getM") $ getM b r
                                   t = trace ("t calc") $ (y^(2^(r-m-1))) `mod` p
                                   y' = trace ("t^2") $ (t^2) `mod` p
                                   r' = trace ("m mod p") $ m `mod` p
                                   x' = trace ("x * t") $ (x*t) `mod` p
                                   b' = trace ("b * y'") $ (b * y') `mod` p
                               in do
                                   trace ("Loop") $ lpin (y',r',x',b')
      getM b r = let nums = [1..2^r]
                     pr x = (b^(2^x)::Integer) `mod` p == 1
                  in
                   trace ("Internal getM") $ srch pr nums
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


--Powermodulus schnier style
smpow :: Integer -> Integer -> Integer -> Integer
smpow !b !e !m = let e' = binNot e
                  in
                   mpow b e' 
    where
      mpow b es = mpow' b es 1
      mpow' b [] r = r
      mpow' b (x:xs) r = if x 
                          then mpow' (b^2 `mod` m) xs (r*b `mod` m)
                          else mpow' (b^2 `mod` m) xs r 
      binNot ns = binNot' 0 ns
      binNot' i 0 = []
      binNot' i n = if testBit n 0 
                    then (True) : binNot' (i+1) (shiftR n 1)
                    else (False) : binNot' (i+1) (shiftR n 1)
