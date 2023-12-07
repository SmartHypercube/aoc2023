import Prelude (print)
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

import qualified Data.Attoparsec.Text as Parser

data Card = Card2 | Card3 | Card4 | Card5 | Card6 | Card7 | Card8 | Card9 | CardT | CardJ | CardQ | CardK | CardA
    deriving stock (Eq, Ord, Show)

cardParser :: Parser.Parser Card
cardParser = Parser.choice
    [ "2" $> Card2
    , "3" $> Card3
    , "4" $> Card4
    , "5" $> Card5
    , "6" $> Card6
    , "7" $> Card7
    , "8" $> Card8
    , "9" $> Card9
    , "T" $> CardT
    , "J" $> CardJ
    , "Q" $> CardQ
    , "K" $> CardK
    , "A" $> CardA
    ]

newtype Hand = Hand [Card]
    deriving stock (Eq, Show)

handParser :: Parser.Parser Hand
handParser = Hand <$> Parser.count 5 cardParser

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving stock (Eq, Ord, Show)

handType :: Hand -> HandType
handType (Hand cards) = case List.sort $ Map.elems $ Map.fromListWith (+) $ map (, 1) cards of
    [5] -> FiveOfAKind
    [1, 4] -> FourOfAKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfAKind
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    (_ :: [Integer]) -> error "impossible"

instance Ord Hand where
    compare (Hand cards1) (Hand cards2) = compare (handType $ Hand cards1) (handType $ Hand cards2) <> compare cards1 cards2

parser :: Parser.Parser [(Hand, Integer)]
parser = Parser.many1' $ (,) <$> handParser <* Parser.skipSpace <*> Parser.decimal <* Parser.endOfLine

main :: IO ()
main = do
    text <- readFileUtf8 "data/day7.txt"
    let hands = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ map (\((_, bid), rank) -> bid * rank) $ zip (List.sort hands) [1..]
