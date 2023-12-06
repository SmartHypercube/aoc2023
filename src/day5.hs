import Prelude (print, putStrLn)
import RIO hiding (Map)
import qualified RIO.List as List
import qualified RIO.List.Partial as List'

import qualified Data.Attoparsec.Text as Parser

data Map = Map
    { from :: !Text
    , to :: !Text
    , items :: ![(Integer, Integer, Integer)]
    }
    deriving stock (Eq, Show)

mapLookup :: Map -> Integer -> Integer
mapLookup m i = fromMaybe i $ List.headMaybe $ do
    (destStart, sourceStart, length_) <- items m
    guard $ i >= sourceStart && i < sourceStart + length_
    pure $ i - sourceStart + destStart

parser :: Parser.Parser ([Integer], [Map])
parser = (,)
    <$> ("seeds: " *> Parser.sepBy1' Parser.decimal Parser.skipSpace <* "\n\n")
    <*> Parser.sepBy1' (Map
        <$> Parser.takeWhile1 (Parser.inClass "a-z") <* "-to-"
        <*> Parser.takeWhile1 (Parser.inClass "a-z") <* " map:\n"
        <*> Parser.many1' ((,,)
            <$> Parser.decimal <* Parser.skipSpace
            <*> Parser.decimal <* Parser.skipSpace
            <*> Parser.decimal <* "\n")) "\n"

main :: IO ()
main = do
    text <- readFileUtf8 "data/day5.txt"
    let (seeds, maps) = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ List'.minimum $ map (foldr (flip (.)) id $ map mapLookup maps) seeds
    putStrLn "Run `stack run day5b` to run the second part."
