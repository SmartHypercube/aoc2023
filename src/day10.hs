import Prelude (print)
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.List.Partial as List'
import qualified RIO.Partial as RIO'
import qualified RIO.Text as Text

data Direction = North | East | South | West
    deriving stock (Eq, Show)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

step :: Direction -> (Integer, Integer) -> (Integer, Integer)
step North (i, j) = (i - 1, j)
step East (i, j) = (i, j + 1)
step South (i, j) = (i + 1, j)
step West (i, j) = (i, j - 1)

data Tile = Ground | Pipe !Direction !Direction | Start
    deriving stock (Eq, Show)

charToTile :: Char -> Tile
charToTile '.' = Ground
charToTile '|' = Pipe North South
charToTile '-' = Pipe East West
charToTile 'L' = Pipe North East
charToTile 'J' = Pipe North West
charToTile '7' = Pipe South West
charToTile 'F' = Pipe South East
charToTile 'S' = Start
charToTile _ = error "invalid tile"

main :: IO ()
main = do
    text <- readFileUtf8 "data/day10.txt"
    let tileMap = HashMap.fromList $ do
            (i :: Integer, line) <- zip [0..] $ lines $ Text.unpack text
            (j :: Integer, char) <- zip [0..] line
            pure ((i, j), charToTile char)
        tileAt pos = HashMap.lookupDefault Ground pos tileMap
        go (!c :: Integer, !a :: Integer) pos direction = case tileAt pos of
            Pipe d1 d2
                | d1 == opposite direction -> go (c + 1, a + deltaA) (step d2 pos) d2
                | d2 == opposite direction -> go (c + 1, a + deltaA) (step d1 pos) d1
            Start -> Just (c + 1, abs $ a + deltaA)
            _ -> Nothing
            where
            deltaA = case direction of
                East -> fst pos
                West -> negate $ fst pos
                _ -> 0
        start = List'.head $ do
            (k, v) <- HashMap.toList tileMap
            guard $ v == Start
            pure k
        (circumference, area) = RIO'.fromJust $ asum $ flip map [North, East, South, West] $ \i -> go (0, 0) (step i start) i
    print $ div circumference 2
    print $ area - div circumference 2 + 1
