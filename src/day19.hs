import Prelude (print, putStrLn)
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Partial as RIO'

import qualified Data.Attoparsec.Text as Parser

type Part = HashMap Char Int

data Action = Goto !Text | Reject | Accept
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

data Rule = Rule
    { pred :: !(Part -> Bool)
    , action :: !Action
    }

parser :: Parser.Parser (HashMap Text [Rule], [Part])
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
                op <- Parser.choice [(>) <$ ">", (<) <$ "<"]
                value <- Parser.decimal
                void ":"
                action <- actionParser
                pure $ Rule (\part -> RIO'.fromJust (HashMap.lookup key part) `op` value) action
            constantRule = do
                action <- actionParser
                pure $ Rule (const True) action
        rules <- Parser.sepBy1' (compareRule <|> constantRule) ","
        void "}"
        Parser.endOfLine
        pure (name, rules)
    Parser.endOfLine
    parts <- Parser.many1' $ do
        void "{"
        kv <- Parser.sepBy1' (do
            key <- Parser.anyChar
            void "="
            value <- Parser.decimal
            pure (key, value)
            ) ","
        void "}"
        Parser.endOfLine
        pure $ HashMap.fromList kv
    pure (workflows, parts)

runWorkflow :: Part -> [Rule] -> Action
runWorkflow _ [] = error "impossible"
runWorkflow part (rule:rules)
    | pred rule part = action rule
    | otherwise = runWorkflow part rules

runWorkflows :: Part -> HashMap Text [Rule] -> Text -> Bool
runWorkflows part workflows name =
    let rules = RIO'.fromJust $ HashMap.lookup name workflows
        action = runWorkflow part rules
    in  case action of
            Reject -> False
            Accept -> True
            Goto name' -> runWorkflows part workflows name'

main :: IO ()
main = do
    text <- readFileUtf8 "data/day19.txt"
    let (workflows, parts) = fromRight (error "parse error") $ Parser.parseOnly (parser <* Parser.endOfInput) text
    print $ sum $ do
        part <- parts
        guard $ runWorkflows part workflows "in"
        pure $ sum $ HashMap.elems part
    putStrLn "Run `stack run day19b` to run the second part."
