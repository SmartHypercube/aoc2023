import Prelude (print)
import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as List'
import qualified RIO.Text as Text

main :: IO ()
main = do
    text <- readFileUtf8 "data/day11.txt"
    let galaxies = do
            (i, line) <- zip [0..] $ lines $ Text.unpack text
            (j, char) <- zip [0..] line
            guard $ char == '#'
            pure (i, j)
        galaxyCount = length galaxies
        windowIndexed xs = List.zip3 [0..] xs $ List'.tail xs
        totalLength k xs = sum $ do
            (i, a, b) <- windowIndexed $ List.sort xs
            pure $ (i + 1) * (galaxyCount - i - 1) * if a == b then 0 else (b - a) * k - k + 1
    print $ totalLength 2 (map fst galaxies) + totalLength 2 (map snd galaxies)
    print $ totalLength 1000000 (map fst galaxies) + totalLength 1000000 (map snd galaxies)
