import Prelude (print)
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.State as State

import qualified Data.Attoparsec.Text as Parser

data Spring = Operational | Damaged
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

parser :: Parser.Parser [([Maybe Spring], [Integer])]
parser = Parser.many1' $ do
    springs <- Parser.many1' (
        ("." $> Just Operational) <|>
        ("#" $> Just Damaged) <|>
        ("?" $> Nothing)) <* " "
    runs <- Parser.sepBy1' Parser.decimal "," <* "\n"
    pure (springs, runs)

data State = WaitForRuns ![Integer] | InARun !Integer ![Integer]
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

step :: State -> Spring -> Maybe State
step (WaitForRuns runs) Operational = Just $ WaitForRuns runs
step (WaitForRuns []) Damaged = Nothing
step (WaitForRuns (run:runs)) Damaged = Just $ InARun (run - 1) runs
step (InARun 0 runs) Operational = Just $ WaitForRuns runs
step (InARun 0 _) Damaged = Nothing
step (InARun _ _) Operational = Nothing
step (InARun n runs) Damaged = Just $ InARun (n - 1) runs

success :: State -> Bool
success (WaitForRuns []) = True
success (InARun 0 []) = True
success _ = False

countPossibilities' :: Maybe State -> [Maybe Spring] -> State.State (HashMap (State, [Maybe Spring]) Integer) Integer
countPossibilities' Nothing _ = pure 0
countPossibilities' (Just state) l = do
    cache <- State.get
    case HashMap.lookup (state, l) cache of
        Just result -> pure result
        Nothing -> do
            result <- case l of
                Nothing:xs -> do
                    result1 <- countPossibilities' (step state Operational) xs
                    result2 <- countPossibilities' (step state Damaged) xs
                    pure $ result1 + result2
                Just x:xs -> countPossibilities' (step state x) xs
                [] -> pure $ if success state then 1 else 0
            State.modify' $ HashMap.insert (state, l) result
            pure result

main :: IO ()
main = do
    text <- readFileUtf8 "data/day12.txt"
    let records = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ do
        (springs, runs) <- records
        let springs' = List.intercalate [Nothing] $ replicate 5 springs
            runs' = mconcat $ replicate 5 runs
        pure $ State.evalState (countPossibilities'  (Just $ WaitForRuns runs') springs') mempty
