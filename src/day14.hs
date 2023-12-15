import Prelude (print)
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Text as Text

load :: [Char] -> Int
load [] = 0
load l = partLoad + load (List.drop 1 ys)
    where
    (xs, ys) = List.break (== '#') l
    count = length $ List.filter (== 'O') xs
    partLoad = (2 * length l - count + 1) * count `div` 2

loadWithoutTilt :: [Char] -> Int
loadWithoutTilt l = sum $ do
    (i, iLoad) <- zip l [length l, length l - 1 ..]
    guard $ i == 'O'
    pure iLoad

tilt :: [Char] -> [Char]
tilt l = case ys of
    [] -> as <> bs
    _ -> as <> bs <> "#" <> tilt (List.drop 1 ys)
    where
    (xs, ys) = List.break (== '#') l
    (as, bs) = List.partition (== 'O') xs

spin :: Int -> HashMap [[Char]] Int -> [[Char]] -> [[Char]]
spin 0 _ platform = platform
spin n cache platform = case HashMap.lookup platform cache of
    Just lastN -> spin (n `mod` (lastN - n)) mempty platform
    Nothing -> spin (n - 1) (HashMap.insert platform n cache) platform'
    where
    platform' = platform  -- 从左到右，从上到下
        & List.transpose & map tilt  -- 从上到下，从左到右
        & List.transpose & map tilt  -- 从左到右，从上到下
        & List.reverse & List.transpose & map tilt  -- 从下到上，从左到右
        & List.reverse & List.transpose & map tilt  -- 从右到左，从下到上
        & List.reverse & map List.reverse  -- 从左到右，从上到下

main :: IO ()
main = do
    text <- readFileUtf8 "data/day14.txt"
    print $ sum $ map load $ List.transpose $ lines $ Text.unpack text
    print $ sum $ map loadWithoutTilt $ List.transpose $ spin 1000000000 mempty $ lines $ Text.unpack text
