{-# LANGUAGE BangPatterns #-}
module Rabin where

import System.IO
import Primes
import Data.Word
import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as BS
import System.Directory
import Numeric (showHex)

getKeys :: IO (Integer,Integer)
getKeys = do
            exists <- doesFileExist "keys.txt"
            if exists
              then getKeys'
              else do
                     putStr "Keyfile does not exist.  Creating...\n"
                     writeKeys "keys.txt"
                     getKeys'

  where
    getKeys' = do
                  filecontent <- readFile "keys.txt"
                  return $ read filecontent


testmsg :: [Word8]

testmsg = [0..4] 



--31,43
main :: IO ()
main = do
          (p,q) <- getKeys
          let n = p * q
          let m = roll testmsg
          if m > n then error "M is too large." else return ()
          putStr $ "Message: " ++ (showHex m "") ++ "\n\n"
          let ciphertext = m^(2::Integer) `mod` n
          putStr $ "Ciphertext: " ++ (showHex ciphertext "") ++ "\n\n"
          roots <- squareroots ciphertext p q
          putStr $ foldr (\x y -> y ++ (showHex x "") ++ "\n\n\n") "" roots
          putStr "Complete."

encrypt :: Integer -> BS.ByteString -> IO Integer
encrypt n msg = do
                  let m = roll $ BS.unpack msg 
                  if m > n then error "M is too large." else return ()
                  let ciphertext = m^(2::Integer) `mod` n
                  return ciphertext

decrypt :: Integer -> Integer -> Integer -> IO [BS.ByteString]
decrypt p q ct = do
                  roots <- squareroots ct p q
                  return $ map (BS.pack . unroll) roots
                  
--Algorithm 3.44 from the book
squareroots :: Integer -> Integer -> Integer -> IO [Integer]
squareroots m p q = do
                        let n = (p*q)
                        (r,_,s,_) <- sqrts m p q
                        let (_,c,d) = eeuclid p q
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
eeuclid a b = let start = ((0::Integer),(0::Integer),(0::Integer),(1::Integer),(1::Integer),(0::Integer),a,b) 
                  (_,_,_,x2',_,y2',a',_) = eproc start 
                  in (a',x2',y2')
  where 
    eproc :: (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer) ->  (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer) 
    eproc (x,y,x1,x2,y1,y2,alpha,0) = (x,y,x1,x2,y1,y2,alpha,0)
    eproc (_,_,x1,x2,y1,y2,alpha,beta) = let    q = alpha `div` beta
                                                r = alpha - q*beta
                                                x' = x2 - q * x1
                                                y' = y2 - q * y1
                                                a'  = beta
                                                b' = r
                                                x2' = x1
                                                x1' = x'
                                                y2' = y1
                                                y1' = y'
                                            in
                                              eproc (x',y',x1',x2',y1',y2',a',b')

sqrts ::  Integer -> Integer -> Integer -> IO (Integer,Integer,Integer,Integer)
sqrts a p q  = do
                   let r =  msqrt3 a p 
                   let s =  msqrt3 a q 
                   let s' = q - s
                   let r' = p - r
                   return (r,r',s,s')



--For chosen p's and q's === 3 `mod` 4
--Requires that your keys are Gaussian Primes
msqrt3 :: Integer -> Integer -> Integer
msqrt3 c p = smpow c ((p+1) `div` 4) p --c^((p+1) `div` 4) `mod` p



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
      mpow beta es = mpow' beta es 1
      mpow' _ [] r = r
      mpow' beta (x:xs) r = if x 
                              then mpow' (beta^(2::Integer) `mod` m) xs (r*beta `mod` m)
                              else mpow' (beta^(2::Integer) `mod` m) xs r 
      binNot ns = binNot' (0::Integer) ns
      binNot' _ 0 = []
      binNot' i n = if testBit n 0 
                    then (True) : binNot' (i+1) (shiftR n 1)
                    else (False) : binNot' (i+1) (shiftR n 1)
