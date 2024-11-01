module Main where
import System.IO
import Data.List
import Data.Char
import Data.Ord (comparing)

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
                    nub [format_p x p | x<-p])
    where p = factors n 2

likes :: [String] -> String
likes li
    | (length li) == 0 = "no one likes this"
    | (length li) == 1 = li!!0++" likes this"
    | (length li) == 2 = li!!0++" and "++li!!1++" like this"
    | (length li) == 3 = li!!0++", "++li!!1++" and "++li!!2++" like this"
    | otherwise = li!!0++", "++li!!1++" and "++(show (length li-2))++" others like this"

isSquare :: Integral n => n -> Bool
isSquare n = (sqrt$fromIntegral n)==(fromIntegral.floor.sqrt$fromIntegral n)

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder []=[]
uniqueInOrder [x]=[x]
uniqueInOrder (x:xs)
    | x==head xs=uniqueInOrder xs
    | otherwise=x:uniqueInOrder xs

getCountVowel :: String -> Int
getCountVowel s=length[0 |x<-s,x`elem`"aeiou"]

getUnique :: [Int] -> Int
getUnique (x:y:xs)=[e |e<-x:y:xs,e/=p]!!0 where p=if(length$filter(==x)(x:y:xs))>1 then x else y

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

is_palindrome :: Integer -> Bool
is_palindrome x = (show x) == reverse (show x)

get_calibration_val :: String -> Integer
get_calibration_val inp = read (firstChar : [lastChar]) :: Integer
    where
        tmp = filter isDigit (get_val inp)
        firstChar = tmp !! 0
        lastChar = last tmp

final_calib :: [String] -> Integer
final_calib inp
    | (length inp) == 0 = 0
    | otherwise = (get_calibration_val (inp!!0)) + final_calib (tail inp)

findPos :: String -> String -> Maybe Int
findPos sub str = findIndex (isPrefixOf sub) (tails str)

get_pos :: String -> String -> String -> (Int, Int, String)
get_pos str sub rep =
    case findPos sub str of
        Just pos -> (pos, length sub, rep)
        Nothing  -> (1000, 0, "")

lowestIndex :: [(Int, Int, String)] -> (Int, Int, String)
lowestIndex xs = minimumBy (comparing (\(x, _, _) -> x)) xs

get_val :: String -> String
get_val str
    | x == 1000 = str
    | otherwise = get_val (take (x+1) str ++ z ++ drop (x+1) str)
    where
        (x, y, z) = lowestIndex [
                        get_pos str "one" "1",
                        get_pos str "two" "2",
                        get_pos str "three" "3",
                        get_pos str "four" "4",
                        get_pos str "five" "5",
                        get_pos str "six" "6",
                        get_pos str "seven" "7",
                        get_pos str "eight" "8",
                        get_pos str "nine" "9"
                    ]

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let num = read input :: Integer
    putStrLn.init$prime_factors$num
    putStrLn$likes ["Peter", "Cow", "me", "he"]
    putStrLn.show$isSquare 25
    putStrLn.show$uniqueInOrder "AABBCCBB"
    putStrLn.show$getCountVowel "alpha"
    putStrLn.show$getUnique [0, 0, 1, 0, 0]
    -- #1
    putStrLn (format_mul 3 5)
    -- #2
    print.sum.filter even$fibonacci 1 2
    -- #3
    putStrLn.show.last$factors 600851475143 2
    -- #4
    putStrLn.show.maximum$[prod | x<-(reverse [100..999]), y<-(reverse [100..999]), let prod = x*y, is_palindrome prod]
    -- Advent of Code 2023#1
    handle <- openFile "a-of-cod2023#1" ReadMode
    contents <- hGetContents handle
    print (final_calib (lines contents))
