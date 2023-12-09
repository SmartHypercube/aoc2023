import Prelude (print)
import RIO
import qualified RIO.List.Partial as List'

import qualified Data.Attoparsec.Text as Parser

parser :: Parser.Parser [[Integer]]
parser = Parser.many1' $ Parser.sepBy1' (Parser.signed Parser.decimal) " " <* "\n"

extrapolate :: [Integer] -> Integer
extrapolate l
    | all (== 0) l = 0
    | otherwise = List'.last l + extrapolate (zipWith (-) (List'.tail l) l)

main :: IO ()
main = do
    text <- readFileUtf8 "data/day9.txt"
    let histories = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ map extrapolate histories
    print $ sum $ map (extrapolate . reverse) histories
