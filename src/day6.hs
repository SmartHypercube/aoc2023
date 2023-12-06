import Prelude (print)
import RIO
import qualified RIO.Text as Text

import qualified Data.Attoparsec.Text as Parser

parser :: Parser.Parser [(Integer, Integer)]
parser = pure zip
    <* "Time:" <* Parser.skipSpace <*> Parser.many1' (Parser.decimal <* Parser.skipSpace)
    <* "Distance:" <* Parser.skipSpace <*> Parser.many1' (Parser.decimal <* Parser.skipSpace)

main :: IO ()
main = do
    text <- readFileUtf8 "data/day6.txt"
    do
        let records = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
        print $ product $ flip map records $ \(time, distance) -> length $ filter (\i -> i * (time - i) > distance) [0..time]
    do
        let records = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) $ Text.filter (/= ' ') text
        print $ product $ flip map records $ \(time, distance) -> length $ filter (\i -> i * (time - i) > distance) [0..time]
