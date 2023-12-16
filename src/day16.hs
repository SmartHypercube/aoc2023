import Prelude (print)
import RIO hiding (Down, Left, Right)
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.List.Partial as List'
import qualified RIO.State as State
import qualified RIO.Text as Text

data Direction = Up | Right | Down | Left
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

step :: Direction -> (Int, Int) -> (Int, Int)
step Up (i, j) = (i - 1, j)
step Right (i, j) = (i, j + 1)
step Down (i, j) = (i + 1, j)
step Left (i, j) = (i, j - 1)

type Tile = Direction -> [Direction]

charToTile :: Char -> Tile
charToTile '.' d = [d]
charToTile '/' Up = [Right]
charToTile '/' Right = [Up]
charToTile '/' Down = [Left]
charToTile '/' Left = [Down]
charToTile '\\' Up = [Left]
charToTile '\\' Right = [Down]
charToTile '\\' Down = [Right]
charToTile '\\' Left = [Up]
charToTile '|' Up = [Up]
charToTile '|' Down = [Down]
charToTile '|' _ = [Up, Down]
charToTile '-' Right = [Right]
charToTile '-' Left = [Left]
charToTile '-' _ = [Left, Right]
charToTile _ _ = error "impossible"

data TileState = TileState
    { tile :: !Tile
    , visited :: !(HashSet Direction)
    }

enter :: Direction -> (Int, Int) -> State.State (HashMap (Int, Int) TileState) ()
enter direction pos = do
    maybeTileState <- State.gets $ HashMap.lookup pos
    case maybeTileState of
        Just tileState | not $ HashSet.member direction tileState.visited -> do
            State.modify' $ HashMap.insert pos $ tileState {visited = HashSet.insert direction tileState.visited}
            for_ (tileState.tile direction) $ \direction' -> enter direction' $ step direction' pos
        _ -> pure ()

main :: IO ()
main = do
    text <- readFileUtf8 "data/day16.txt"
    let tileStates = HashMap.fromList $ do
            (i, line) <- zip [0..] $ lines $ Text.unpack text
            (j, char) <- zip [0..] line
            pure ((i, j), TileState (charToTile char) mempty)
        height = length $ lines $ Text.unpack text
        width = length $ List'.head $ lines $ Text.unpack text
    print $ length $ filter (\i -> not $ null i.visited) $ HashMap.elems $ State.execState (enter Right (0, 0)) $ tileStates
    print $ List'.maximum $ do
        (direction, pos) <- mconcat
            [ [(Down, (0, i)) | i <- [0..width - 1]]
            , [(Right, (i, 0)) | i <- [0..height - 1]]
            , [(Up, (height - 1, i)) | i <- [0..width - 1]]
            , [(Left, (i, width - 1)) | i <- [0..height - 1]]
            ]
        pure $ length $ filter (\i -> not $ null i.visited) $ HashMap.elems $ State.execState (enter direction pos) $ tileStates
