import Prelude (print)
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Partial as RIO'

import qualified Data.Attoparsec.Text as Parser

type PartRange = HashMap Char (Int, Int)

data Pred = GreaterThan !Char !Int | LessThan !Char !Int | Always
    deriving stock (Eq, Show)

splitPartRange :: PartRange -> Pred -> [(PartRange, Bool)]
splitPartRange partRange pred = case pred of
    GreaterThan key value -> do
        let range = RIO'.fromJust $ HashMap.lookup key partRange
        (subRange, result) <- [((fst range, value), False), ((value + 1, snd range), True)]
        guard $ fst subRange <= snd subRange
        pure (HashMap.insert key subRange partRange, result)
    LessThan key value -> do
        let range = RIO'.fromJust $ HashMap.lookup key partRange
        (subRange, result) <- [((fst range, value - 1), True), ((value, snd range), False)]
        guard $ fst subRange <= snd subRange
        pure (HashMap.insert key subRange partRange, result)
    Always -> pure (partRange, True)

partRangeSize :: PartRange -> Int
partRangeSize partRange = product $ do
    range <- HashMap.elems partRange
    pure $ snd range - fst range + 1

data Action = Goto !Text | Reject | Accept
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

data Rule = Rule
    { pred :: !Pred
    , action :: !Action
    }

parser :: Parser.Parser (HashMap Text [Rule])
parser = do
    workflows <- fmap HashMap.fromList $ Parser.many1' $ do
        name <- Parser.takeWhile1 $ \c -> c >= 'a' && c <= 'z'
        void "{"
        let actionParser = Parser.choice
                [ Reject <$ "R"
                , Accept <$ "A"
                , Goto <$> Parser.takeWhile1 (\c -> c >= 'a' && c <= 'z')
                ]
            compareRule = do
                key <- Parser.anyChar
                op <- Parser.choice [GreaterThan <$ ">", LessThan <$ "<"]
                value <- Parser.decimal
                void ":"
                action <- actionParser
                pure $ Rule (op key value) action
            constantRule = do
                action <- actionParser
                pure $ Rule Always action
        rules <- Parser.sepBy1' (compareRule <|> constantRule) ","
        void "}"
        Parser.endOfLine
        pure (name, rules)
    Parser.endOfLine
    void $ Parser.takeText
    pure workflows

runWorkflow :: PartRange -> [Rule] -> [(PartRange, Action)]
runWorkflow _ [] = error "impossible"
runWorkflow partRange (rule:rules) = do
    (partRange', result) <- splitPartRange partRange rule.pred
    if result
        then pure (partRange', rule.action)
        else runWorkflow partRange' rules

runWorkflows :: PartRange -> HashMap Text [Rule] -> Text -> [PartRange]
runWorkflows partRange workflows name = do
    let rules = RIO'.fromJust $ HashMap.lookup name workflows
    (partRange', action) <- runWorkflow partRange rules
    case action of
        Reject -> []
        Accept -> pure partRange'
        Goto name' -> runWorkflows partRange' workflows name'

main :: IO ()
main = do
    text <- readFileUtf8 "data/day19.txt"
    let workflows = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ do
        let partRange = HashMap.fromList [(key, (1, 4000)) | key <- ['x', 'm', 'a', 's']]
        partRange' <- runWorkflows partRange workflows "in"
        pure $ partRangeSize partRange'
