import Prelude (print)
import RIO
import RIO.List.Partial ((!!))
import qualified RIO.List as List

import qualified Data.Attoparsec.Text as Parser

data Thing = Ash | Rock
    deriving stock (Eq, Show)

parser :: Parser.Parser [[[Thing]]]
parser = Parser.sepBy1' (do
    Parser.many1' $ do
        row <- Parser.many1' $ do
            Parser.choice
                [ "." $> Ash
                , "#" $> Rock
                ]
        void "\n"
        pure row) "\n"

findMirror :: [[Thing]] -> Int
findMirror rows = sum $ do
    position <- [1 .. length rows - 1]
    guard $ and $ do
        i <- [0 .. position - 1]
        let j = position * 2 - i - 1
        guard $ j < length rows
        pure $ rows !! i == rows !! j
    pure position

findMirrorWithSmudge :: [[Thing]] -> Int
findMirrorWithSmudge rows = sum $ do
    position <- [1 .. length rows - 1]
    guard $ position /= oldMirror
    guard $ (== 1) $ length $ do
        i <- [0 .. position - 1]
        let j = position * 2 - i - 1
        guard $ j < length rows
        (a, b) <- zip (rows !! i) (rows !! j)
        guard $ a /= b
        pure ()
    pure position
    where
    oldMirror = findMirror rows

main :: IO ()
main = do
    text <- readFileUtf8 "data/day13.txt"
    let patterns = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ do
        pattern_ <- patterns
        pure $ findMirror pattern_ * 100 + findMirror (List.transpose pattern_)
    print $ sum $ do
        pattern_ <- patterns
        pure $ findMirrorWithSmudge pattern_ * 100 + findMirrorWithSmudge (List.transpose pattern_)
