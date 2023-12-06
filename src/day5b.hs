import Prelude (print)
import RIO
import qualified RIO.List.Partial as List'
import qualified RIO.Map as Map

import qualified Data.Attoparsec.Text as Parser

-- (seeds, maps)
-- seeds: [(start, end)]
-- maps: [events]
-- events: [(start, delta)]
parser :: Parser.Parser ([(Integer, Integer)], [[(Integer, Integer)]])
parser = do
    void "seeds:"
    seeds <- Parser.many1' $ do
        start <- Parser.skipSpace *> Parser.decimal
        length_ <- Parser.skipSpace *> Parser.decimal
        pure (start, start + length_ - 1)
    void "\n"
    maps <- Parser.many1' $ do
        void $ "\n" *> Parser.takeWhile1 (Parser.inClass "a-z") *> "-to-" *> Parser.takeWhile1 (Parser.inClass "a-z") *> " map:\n"
        parts <- Parser.many1' $ do
            destStart <- Parser.decimal <* Parser.skipSpace
            sourceStart <- Parser.decimal <* Parser.skipSpace
            length_ <- Parser.decimal <* "\n"
            pure $ Map.fromList [(sourceStart, destStart - sourceStart), (sourceStart + length_, 0)]
        pure $ Map.toList $ Map.unionsWith (+) parts
    pure (seeds, maps)

mapRange :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
mapRange events (start, end) = do
    ((rangeStart, rangeDelta), rangeEnd) <- zip ([(start, 0)] <> events) (map ((- 1) . fst) events <> [end])
    let rangeStart' = max rangeStart start
        rangeEnd' = min rangeEnd end
    guard $ rangeStart' <= rangeEnd'
    pure (rangeStart' + rangeDelta, rangeEnd' + rangeDelta)

chainLToRM :: (Foldable t, Monad m) => t (a -> m a) -> a -> m a
chainLToRM = foldr (>=>) pure

main :: IO ()
main = do
    text <- readFileUtf8 "data/day5.txt"
    let (seeds, maps) = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ List'.minimum $ do
        (start, end) <- seeds
        (start', _) <- chainLToRM (map mapRange maps) (start, end)
        pure start'
