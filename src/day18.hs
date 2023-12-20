import Prelude (print)
import RIO hiding (Down, Left, Right)
import qualified RIO.ByteString as ByteString
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.State as State

import qualified Data.Attoparsec.Text as Parser

data Direction = Up | Right | Down | Left
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

step :: Direction -> Int -> (Int, Int) -> (Int, Int)
step Up n (i, j) = (i - n, j)
step Right n (i, j) = (i, j + n)
step Down n (i, j) = (i + n, j)
step Left n (i, j) = (i, j - n)

data Plan = Plan
    { direction :: !Direction
    , steps :: !Int
    }
    deriving stock (Eq, Show)

parser1 :: Parser.Parser [Plan]
parser1 = Parser.many1' $ do
    direction <- Parser.choice
        [ Up <$ "U"
        , Right <$ "R"
        , Down <$ "D"
        , Left <$ "L"
        ]
    Parser.skipSpace
    steps <- Parser.decimal
    Parser.skipSpace
    void "("
    void $ Parser.takeWhile1 (/= ')')
    void ")"
    Parser.endOfLine
    pure $ Plan direction steps

parser2 :: Parser.Parser [Plan]
parser2 = Parser.many1' $ do
    void $ Parser.anyChar
    Parser.skipSpace
    void $ Parser.decimal
    Parser.skipSpace
    void "(#"
    steps_ <- Parser.take 5
    steps <- either fail pure $ Parser.parseOnly (Parser.hexadecimal <* Parser.endOfInput) steps_
    direction <- Parser.choice
        [ Right <$ "0"
        , Down <$ "1"
        , Left <$ "2"
        , Up <$ "3"
        ]
    void ")"
    Parser.endOfLine
    pure $ Plan direction steps

calcAreaAndCircumference :: (Int, Int) -> Int -> Int -> [Plan] -> (Int, Int)
calcAreaAndCircumference _ area circumference [] = (abs area, circumference)
calcAreaAndCircumference pos area circumference (plan:plans) =
    let pos' = step plan.direction plan.steps pos
        circumference' = circumference + plan.steps
        area' = case plan.direction of
            Up -> area + plan.steps * snd pos
            Down -> area - plan.steps * snd pos
            _ -> area
    in  calcAreaAndCircumference pos' area' circumference' plans

main :: IO ()
main = do
    text <- readFileUtf8 "data/day18.txt"
    do
        let plans = fromRight (error "parse error") $ Parser.parseOnly (parser1 <* Parser.endOfInput) text
            (area, circumference) = calcAreaAndCircumference (0, 0) 0 0 plans
        print $ area + circumference `div` 2 + 1
    do
        let plans = fromRight (error "parse error") $ Parser.parseOnly (parser2 <* Parser.endOfInput) text
            (area, circumference) = calcAreaAndCircumference (0, 0) 0 0 plans
        print $ area + circumference `div` 2 + 1
