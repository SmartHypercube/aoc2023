import Prelude (print)
import RIO
import qualified RIO.Char as Char
import qualified RIO.HashMap as HashMap
import qualified RIO.List.Partial as List'
import qualified RIO.Partial as RIO'
import qualified RIO.Text as Text

main :: IO ()
main = do
    text <- readFileUtf8 "data/day3.txt"
    let charMap = HashMap.fromList $ do
            (i, line) <- zip [0..] $ lines $ Text.unpack text
            (j, char) <- zip [0..] line
            pure ((i, j), char)
        charAt (i :: Int) (j :: Int) = HashMap.lookupDefault '.' (i, j) charMap
        numbers = do
            (row, start) <- HashMap.keys charMap
            guard $ Char.isDigit (charAt row start) && not (Char.isDigit (charAt row (start - 1)))
            let end = List'.head $ do
                    i <- [start..]
                    guard $ not $ Char.isDigit $ charAt row (i + 1)
                    pure i
                value = RIO'.read @Integer $ do
                    i <- [start..end]
                    pure $ charAt row i
            pure (row, start, end, value)
    print $ sum $ do
        (row, start, end, value) <- numbers
        guard $ or $ do
            i <- [row - 1..row + 1]
            j <- [start - 1..end + 1]
            pure $ not (Char.isDigit (charAt i j)) && charAt i j /= '.'
        pure value
    print $ sum $ do
        values <- HashMap.elems $ HashMap.fromListWith (<>) $ do
            (row, start, end, value) <- numbers
            i <- [row - 1..row + 1]
            j <- [start - 1..end + 1]
            guard $ charAt i j == '*'
            pure ((i, j), [value])
        case values of
            [a, b] -> [a * b]
            _ -> []
