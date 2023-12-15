import Prelude (print)
import RIO
import qualified RIO.ByteString as ByteString
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List

import qualified Data.Attoparsec.ByteString.Char8 as Parser

hash :: ByteString -> Integer
hash = toInteger . ByteString.foldl' (\acc x -> (acc + x) * 17) 0

type State = HashMap Integer (HashMap ByteString (Integer, Integer))

data Instruction
    = Remove ByteString
    | Set ByteString Integer
    deriving stock (Eq, Show)

parser :: Parser.Parser [Instruction]
parser = Parser.sepBy1'
    (   (Remove <$> Parser.takeWhile1 Parser.isAlpha_ascii <* "-")
    <|> (Set <$> Parser.takeWhile1 Parser.isAlpha_ascii <* "=" <*> Parser.decimal)
    ) "," <* "\n"

step :: State -> (Integer, Instruction) -> State
step state (_, Remove k) = HashMap.adjust (HashMap.delete k) (hash k) state
step state (t, Set k v) = HashMap.insertWith (HashMap.unionWith updateItem) (hash k) (HashMap.singleton k (t, v)) state
    where
    updateItem (_, v') (t', _) = (t', v')

focusingPower :: (Integer, HashMap ByteString (Integer, Integer)) -> Integer
focusingPower (boxId, box) = sum $ map (\(i, v) -> (boxId + 1) * i * v) $ zip [1..] $ map snd $ List.sort $ HashMap.elems box

main :: IO ()
main = do
    bytes <- readFileBinary "data/day15.txt"
    print $ sum $ map hash $ ByteString.split 44 $ ByteString.filter (/= 10) bytes
    let instructions = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) bytes
    print $ sum $ map focusingPower $ HashMap.toList $ foldl' step mempty $ zip [0..] instructions
