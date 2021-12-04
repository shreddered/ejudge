module Main where

import Control.Applicative
import Control.Monad (unless)

import Data.Array.Unboxed
import Data.Bits (Bits, shiftL)
import Data.Bool (bool)
import Data.Word

import System.IO (isEOF)

-- list of prime numbers (evaluated lazily)
primes :: Integral a => [a]
primes = sieve [2..]

-- sieve of Eratosthenes
sieve :: Integral a => [a] -> [a]
sieve (x:xs) = let multof x n = n `rem` x == 0
                in x : (sieve (filter (not . multof x) xs))

-- 31st Mersenne number (2^31 - 1)
mersenne31 :: (Num a, Bits a) => a
mersenne31 = (1 `shiftL` 31) - 1

-- generate hash functions
mkHashes :: Integer -> Integer -> [Integer -> Integer]
mkHashes m k = let hash i p = \x -> ((i * x + p) `mod` mersenne31) `mod` m
                in zipWith hash [1..k] (primes)

-- Bloom filter
-- Unboxed `Bool` arrays in `array` package use memory efficient implementation
data BloomFilter a b = BloomFilter [a -> b] (UArray b Bool)

instance (Ix b) => Show (BloomFilter a b) where
  show (BloomFilter _ arr) = [bool '0' '1' (arr ! i) | i <- indices arr]

-- create Bloom filter
bloomFilter :: (Integral b, Ix b)
            => b                       -- size of a bit vector
            -> [a -> b]                -- hash functions
            -> BloomFilter a b
bloomFilter m hashes = let arr = array (0, m - 1) [(i, False) | i <- [0..m - 1]]
                        in BloomFilter hashes arr

-- insert element into a Bloom filter
insert :: (Integral b, Ix b) => a -> BloomFilter a b -> BloomFilter a b
insert x (BloomFilter hashes arr) =
  let arr' = arr // [(i, True) | i <- (hashes <*> pure x)]
   in BloomFilter hashes arr'

-- search for an element in a Bloom filter
search :: (Integral b, Ix b) => a -> BloomFilter a b -> Bool
search x (BloomFilter hashes arr) = all id [arr ! i | i <- (hashes <*> pure x)]

main :: IO ()
main = processLineByLine Nothing

-- Parsing routine
-- data type for command: set n p; add k; search k; print
data Command = Set Integer Double | Add Integer | Search Integer | Print | ErrorCommand

-- main loop of a program
processLineByLine :: Maybe (BloomFilter Integer Integer) -> IO ()
processLineByLine bf = do
  done <- isEOF
  unless done $ do
    line <- getLine
    if (null line)
       then processLineByLine bf -- skip empty line
       else do
         let cmd = toCommand line
             (output, bf') = execute cmd bf
         unless (null output) (putStrLn output)
         processLineByLine bf'

toCommand :: String -> Command
toCommand str = let (cmd:args) = words str
                    n = read (args !! 0)
                    p = read (args !! 1)
                    k = read (args !! 0)
                 in case cmd of
                      "set"    -> Set n p
                      "add"    -> Add k
                      "search" -> Search k
                      "print"  -> Print
                      _        -> ErrorCommand

execute :: Command
        -> Maybe (BloomFilter Integer Integer)
        -> (String, Maybe (BloomFilter Integer Integer))
execute cmd Nothing   = case paramsFromSet cmd of
                          (Just (m, k)) -> ( (show m) ++ " " ++ (show k)
                                           , Just (bloomFilter m (mkHashes m k))
                                           )
                          Nothing -> ("error", Nothing)
execute cmd (Just bf) = case cmd of
                          (Set _ _)    -> ("error", Just bf)
                          ErrorCommand -> ("error", Just bf)
                          (Add k)      -> ("", Just (insert k bf))
                          (Search k)   -> ( bool "0" "1" (search k bf)
                                          , Just bf
                                          )
                          Print        -> (show bf, Just bf)

paramsFromSet :: Command -> Maybe (Integer, Integer)
paramsFromSet (Set n p) = let m = round (-fromIntegral(n) * (logBase 2 p) / log 2)
                              k = round (-(logBase 2 p))
                           in if p > 0 && p < 1 && m /= 0 && k /= 0
                                 then Just (m, k)
                                 else Nothing
paramsFromSet _         = Nothing
