import Prelude (print)
import RIO
import RIO.HashMap.Partial ((!))
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.List as List
import qualified RIO.List.Partial as List'

import qualified Data.Array.ST as Array
import qualified Data.Attoparsec.Text as Parser

data Card = Card
    { id :: !Integer
    , winningNumbers :: !(HashSet Integer)
    , myNumbers :: ![Integer]
    }
    deriving stock (Eq, Show)

winCount :: Card -> Integer
winCount card = toInteger $ length $ filter (`HashSet.member` card.winningNumbers) card.myNumbers

parser :: Parser.Parser [Card]
parser = Parser.many' $ Card
    <$> ("Card" *> Parser.skipSpace *> Parser.decimal <* ":")
    <*> (HashSet.fromList <$> Parser.many' (Parser.skipSpace *> Parser.decimal) <* Parser.skipSpace <* "|")
    <*> (Parser.many' (Parser.skipSpace *> Parser.decimal) <* Parser.endOfLine)

main :: IO ()
main = do
    text <- readFileUtf8 "data/day4.txt"
    let cards = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ flip map cards $ \card -> 2 ^ winCount card `div` 2 :: Integer
    do
        print $ runST $ do
            cardCountMap <- Array.newArray ((List'.head cards).id, (List'.last cards).id) 1 :: ST s (Array.STArray s Integer Integer)
            forM_ cards $ \card -> do
                thisCardCount <- Array.readArray cardCountMap card.id
                forM_ [card.id + 1 .. card.id + winCount card] $ \i -> do
                    oldValue <- Array.readArray cardCountMap i
                    Array.writeArray cardCountMap i $ oldValue + thisCardCount
            sum <$> Array.getElems cardCountMap
    do
        let cardCountMap = foldl'
                (\lastMap card -> flip HashMap.mapWithKey lastMap $ \k v -> if
                    | k >= card.id + 1 && k <= card.id + winCount card -> v + lastMap ! card.id
                    | otherwise -> v :: Integer
                )
                (HashMap.fromList [(card.id, 1) | card <- cards])
                cards
        print $ sum $ HashMap.elems cardCountMap
    do
        let cardCountMap = List.foldl
                (\lastMap card k -> if
                    | k >= card.id + 1 && k <= card.id + winCount card -> lastMap k + lastMap card.id
                    | otherwise -> lastMap k :: Integer
                )
                (const 1)
                cards
        print $ sum $ flip map cards $ \card -> cardCountMap card.id
