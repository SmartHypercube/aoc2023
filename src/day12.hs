import Prelude (print, putStrLn)
import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as List'

import qualified Data.Attoparsec.Text as Parser

data Spring = Operational | Damaged | Unknown
    deriving stock (Eq, Show)

parser :: Parser.Parser [([Spring], [Integer])]
parser = Parser.many1' $ do
    springs <- Parser.many1' (
        ("." $> Operational) <|>
        ("#" $> Damaged) <|>
        ("?" $> Unknown)) <* " "
    runs <- Parser.sepBy1' Parser.decimal "," <* "\n"
    pure (springs, runs)

springsToRuns :: [Spring] -> [Integer]
springsToRuns springs = do
    g <- List.group springs
    guard $ List'.head g == Damaged
    pure $ fromIntegral $ length g

possibilities :: [Spring] -> [[Spring]]
possibilities (Unknown:xs) = do
    x' <- [Operational, Damaged]
    xs' <- possibilities xs
    pure $ x' : xs'
possibilities (x:xs) = do
    xs' <- possibilities xs
    pure $ x : xs'
possibilities [] = [[]]

main :: IO ()
main = do
    text <- readFileUtf8 "data/day12.txt"
    let records = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ do
        (springs, runs) <- records
        possibility <- possibilities springs
        guard $ springsToRuns possibility == runs
        pure (1 :: Integer)
    putStrLn "Run `stack run day12b` to run the second part."
