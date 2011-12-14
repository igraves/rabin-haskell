module Primes where
import Control.Monad.Writer
import System.IO.Unsafe
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

lbound :: Integer
lbound = let base = 2::Integer
             power = 2048::Integer
           in
             base^power

ubound :: Integer
ubound = let base = 2::Integer
             power = 2049::Integer
             diff = 100000::Integer
           in
             base^(power-diff)

generatePrime :: IO Integer
generatePrime = do
                  genEntry <- getStdRandom (randomR ((lbound,ubound)))
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
                  genEntry <- getStdRandom (randomR (lbound,ubound))
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
