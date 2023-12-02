import Prelude (print)
import RIO
import qualified RIO.Char as Char
import qualified RIO.HashMap as HashMap

import qualified Data.Attoparsec.Text as Parser

data Game = Game
    { id :: !Integer
    , cubesList :: ![HashMap Text Integer]
    }
    deriving stock (Eq, Show)

limit :: HashMap Text Integer
limit = HashMap.fromList
    [ ("red", 12)
    , ("green", 13)
    , ("blue", 14)
    ]

withinLimit :: HashMap Text Integer -> Bool
withinLimit cubes = all (>= 0) $ HashMap.unionWith (-) limit cubes

parser :: Parser.Parser [Game]
parser = Parser.many' $ do
    void "Game "
    gameId <- Parser.decimal
    void ": "
    cubesList <- flip Parser.sepBy1 "; " $ do
        colors <- flip Parser.sepBy1 ", " $ do
            count <- Parser.decimal
            void " "
            color <- Parser.takeWhile1 Char.isAlpha
            pure $ HashMap.singleton color count
        pure $ foldl' (HashMap.unionWith (+)) mempty colors
    Parser.endOfLine
    pure $ Game gameId cubesList

main :: IO ()
main = do
    text <- readFileUtf8 "data/day2.txt"
    let games = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ flip map games $ \game ->
        if all withinLimit game.cubesList then game.id else 0
    print $ sum $ flip map games $ \game ->
        product $ HashMap.elems $ foldl' (HashMap.unionWith max) mempty game.cubesList
