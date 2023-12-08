import Prelude (print)
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import qualified RIO.Partial as RIO'

import qualified Data.Attoparsec.Text as Parser

parser :: Parser.Parser ([(Text, Text) -> Text], Map Text (Text, Text))
parser = do
    instructions <- fmap List.cycle $ Parser.many1' $ Parser.choice ["L" $> fst, "R" $> snd]
    void "\n\n"
    map_ <- fmap Map.fromList $ Parser.many1' $ do
        key <- Parser.takeWhile1 (/= ' ')
        void " = ("
        value1 <- Parser.takeWhile1 (/= ',')
        void ", "
        value2 <- Parser.takeWhile1 (/= ')')
        void ")\n"
        pure (key, (value1, value2))
    pure (instructions, map_)

main :: IO ()
main = do
    text <- readFileUtf8 "data/day8.txt"
    let (instructions, map_) = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
        positions (i:is) pos = i (RIO'.fromJust $ Map.lookup pos map_) : positions is (i (RIO'.fromJust $ Map.lookup pos map_))
        positions _ _ = error "impossible"
        positions' = List.takeWhile (/= "ZZZ") $ positions instructions "AAA"
    print $ length positions' + 1
    let startPositions = List.filter (\i -> Text.isSuffixOf "A" i) $ Map.keys map_
        values = do
            start <- startPositions
            let positions'' = List.takeWhile (\i -> not $ Text.isSuffixOf "Z" i) $ positions instructions start
            pure $ length positions'' + 1
    print $ foldl' lcm 1 values
