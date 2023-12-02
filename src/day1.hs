import Prelude (print)
import RIO
import qualified RIO.Char as Char
import qualified RIO.List.Partial as List'
import qualified RIO.Partial as RIO'
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text'

digitPatterns :: [(Text, Integer)]
digitPatterns =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("0", 0)
    , ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    ]

main :: IO ()
main = do
    text <- readFileUtf8 "data/day1.txt"
    print $ sum $ flip map (Text.lines text) $ \line ->
        let digits = Text.filter Char.isDigit line
            firstDigit = Text'.head digits
            lastDigit = Text'.last digits
        in RIO'.read @Integer [firstDigit, lastDigit]
    print $ sum $ flip map (Text.lines text) $ \line ->
        let firstDigit = snd $ List'.minimum $ flip map digitPatterns $ \(pattern_, digit) ->
                (Text.length $ fst $ Text'.breakOn pattern_ line, digit)
            lastDigit = snd $ List'.minimum $ flip map digitPatterns $ \(pattern_, digit) ->
                (Text.length $ snd $ Text'.breakOnEnd pattern_ line, digit)
        in firstDigit * 10 + lastDigit
