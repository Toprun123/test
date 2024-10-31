module Main where
import System.IO (hFlush, stdout)
import Data.List

factors :: Integer -> Integer -> [Integer]
factors x d
    | x == d = [d]
    | x < d ^ 2 = [x]
    | (x `mod` d) == 0 = [d] ++ (factors (x `div` d) d)
    | otherwise = factors x (d+1)


format_p :: Integer -> [Integer] -> String
format_p x p =   "(" ++ (show x) ++ (if y> 1
                                then "**" ++ show y
                                else "")
                            ++ ")*"
            where y = length (filter (==x) p)

prime_factors :: Integer -> String
prime_factors n = intercalate "" (
                    nub [ format_p x p | x<-p])
    where p = factors n 2

multiples :: Integer -> Integer -> [Integer]
multiples x y
    | x == 1001 = []
    | x < 1000 = [z] ++ (multiples z y)
    | x >= 1000 = []
    where z = if x+y < 1000 then x+y else 1001

format_mul :: Integer -> Integer -> String
format_mul x y = show . sum . nub . filter (/= 1001) $ multiples 0 x ++ multiples 0 y

fibonacci :: Integer -> Integer -> [Integer]
fibonacci x y
    | x > 4000000 = []
    | otherwise = [x] ++ (fibonacci y (x+y))

main :: IO()
main = do
    -- putStr "Enter a number: "
    -- hFlush stdout
    -- input <- getLine
    -- let num = read input :: Integer
    -- putStrLn . init $ prime_factors $ num
    -- putStrLn (format_mul 3 5)
    -- print (sum . filter even $ fibonacci 1 2)
    putStrLn . show . last $ factors 600851475143 2
