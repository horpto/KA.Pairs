import Data.Array.IO
import System.IO
--import 

intToDigit :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"

intToStr :: Int -> String
intToStr 0 = "0"
intToStr i = map intToDigit (fi i)
    where
        --is = [ x | x <- (fi i) ]
        fi 0 = []
        fi x = (x `mod` 10) : (fi $ x `div` 10)

isDigit c =  c >= '0'   &&  c <= '9'

digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"

strToInt :: String -> Int
strToInt s =  foldl f 0 $ map digitToInt s
    where
        f z x = 10*z + x
--toInt :: String -> Int
--toInt "1" = 1
--toInt "0" = 0
--toInt _   = Nothing

main = do
    fin <- openFile "in.txt" ReadMode
    k' <- hGetLine fin
    l' <- hGetLine fin
    content <- hGetContents fin
    let
        k = strToInt k'
        l = strToInt l'
    --adj     <- newArray (1, k) (newArray (1, l) 0) :: IO (IOArray Int (IO (IOArray Int Int)))
    let a'' = lines content
        a' = map words a''
        a'' = map (map strToInt) a'''
        a' = map (map intToStr) a''
        adjIO = unlines $ map unwords a'
        flow = map (map (\x -> 0)) adj

    fout <- openFile "out.txt" WriteMode
    --hPutStr fout content
    --print b    
    --a' <- readArray adj 1
    --a  <- readArray a' 1
    hPutStr fout adj

    hClose fin
    hClose fout

