module Primes where
import Control.Monad.Writer
import Control.Monad.Writer.Class
import System.IO.Unsafe


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
