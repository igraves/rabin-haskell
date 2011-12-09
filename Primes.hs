module Primes where
import Data.Ratio
import Numeric
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.State
import System.IO.Unsafe
import Data.Bits
import Debug.Trace
import System.Random
import Math.NumberTheory.Primes.Testing


type SieveWrt = Writer [Integer]


-----------------------------------
-------Sieve of Eratosthenes-------
-----------------------------------
sieveE :: Integer -> [Integer]
sieveE i = execWriter $ sieveE' is 
    where is = [2..i]

sieveE' :: [Integer] -> SieveWrt [Integer]
sieveE' [] = return []
sieveE' (i:is) = do
                   tell [i]
                   sieveE' remn 
     where fltr = \x -> x `mod` i /= 0
           remn = filter fltr is

------------------------------------
------------------------------------
------------------------------------

--sieve a list by the first primes up to 65,000 
ssieve :: [Integer] -> [Integer]
ssieve ins = siv sprimes ins  
  where siv [] lg = lg
        siv (i:is) lg = siv is $ filter (flt i) lg
        flt = \x y -> y `mod` x /= 0

--Load the small primes from the accompanying file
sprimes :: [Integer]
sprimes = read $ unsafePerformIO $ readFile $ "primes.txt"


--
--Primality Test
--Simple, dumb
{-
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime i = proc nums
  where
    proc (x:xs) =  tracep $ (x*x > i) || i `mod` x /= 0 && proc xs
    nums = [2..i]
-}
tracep p = traceShow p p


generatePrime :: IO Integer
generatePrime = do
                  genEntry <- getStdRandom (randomR (2^2048,2^2049-100000::Integer))
                  let genList = ssieve [genEntry..genEntry+100000]
                  testPrime genList $ length genList
  where 
    testPrime genList len = do
                              genPick <- getStdRandom (randomR (0,(len)))
                              if isPrime (genList !! genPick)
                               then return (genList !! genPick) 
                               else testPrime genList len


generateGPrime :: IO Integer
generateGPrime = do
                  genEntry <- getStdRandom (randomR (2^2048,2^2049-100000::Integer))
                  let genList = filter isGaussian $ ssieve [genEntry..genEntry+100000]
                  testPrime genList $ length genList
  where 
    testPrime genList len = do
                              genPick <- getStdRandom (randomR (0,(len)))
                              let pick = (genList !! genPick)
                              if isPrime pick 
                               then return (genList !! genPick) 
                               else testPrime genList len
    isGaussian x = x `mod` 4 == 3 

------------------------------------------------------------------------------------
------------------Miller-Rabin Test From the Haskell Wiki---------------------------
--http://www.haskell.org/haskellwiki/Testing_primality#Miller-Rabin_Primality_Test--
------------------------------------------------------------------------------------
-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
 
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

------------------------------------------------------------------------------------------
-----------------------------End code segment from the Haskell Wiki-----------------------
------------------------------------------------------------------------------------------

generateKeyPair :: IO (Integer,Integer)
generateKeyPair = do
                    key1 <- generateKeyMod3 --generatePrime
                    key2 <- generateKeyMod3 --generatePrime
                    return (key1,key2)

generateKeyMod3 :: IO (Integer)
generateKeyMod3 = do
                    generateGPrime

writeKeys :: String -> IO ()
writeKeys fn = do
                 keys <- generateKeyPair
                 writeFile fn $ show keys 
