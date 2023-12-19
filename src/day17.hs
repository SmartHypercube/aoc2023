import Prelude (print)
import RIO hiding (Down, Left, Right)
import qualified RIO.HashMap as HashMap
import qualified RIO.List.Partial as List'
import qualified RIO.Partial as RIO'
import qualified RIO.Set as Set
import qualified RIO.State as State
import qualified RIO.Text as Text

data Direction = Up | Right | Down | Left
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

step :: Direction -> Int -> (Int, Int) -> (Int, Int)
step Up n (i, j) = (i - n, j)
step Right n (i, j) = (i, j + n)
step Down n (i, j) = (i + n, j)
step Left n (i, j) = (i, j - n)

data Node = Node
    { pos :: !(Int, Int)
    , nextDirections :: ![Direction]
    }
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

data Candidates = Candidates
    { nodeToHeatLoss :: !(HashMap Node Int)
    , heatLossToNodes :: !(Set (Int, Node))
    }

insertCandidate :: Node -> Int -> State.State Candidates ()
insertCandidate node heatLoss = do
    candidates <- State.get
    case HashMap.lookup node candidates.nodeToHeatLoss of
        Nothing -> do
            State.put $ candidates
                { nodeToHeatLoss = HashMap.insert node heatLoss candidates.nodeToHeatLoss
                , heatLossToNodes = Set.insert (heatLoss, node) candidates.heatLossToNodes
                }
        Just oldHeatLoss | oldHeatLoss <= heatLoss -> pure ()
        Just oldHeatLoss -> do
            State.put $ candidates
                { nodeToHeatLoss = HashMap.insert node heatLoss candidates.nodeToHeatLoss
                , heatLossToNodes = Set.insert (heatLoss, node) $ Set.delete (oldHeatLoss, node) candidates.heatLossToNodes
                }

deleteCandidate :: Node -> Int -> State.State Candidates ()
deleteCandidate node heatLoss = do
    State.modify' $ \candidates -> candidates
        { nodeToHeatLoss = HashMap.delete node candidates.nodeToHeatLoss
        , heatLossToNodes = Set.delete (heatLoss, node) candidates.heatLossToNodes
        }

calc :: Int -> Int -> [Int] -> ((Int, Int) -> Int) -> State.State Candidates Int
calc height width stepRange heatLossAt = do
    (heatLoss, minNode) <- State.gets $ RIO'.fromJust . Set.lookupMin . (.heatLossToNodes)
    if minNode.pos == (0, 0)
        then pure heatLoss
        else do
            deleteCandidate minNode heatLoss
            for_ minNode.nextDirections $ \direction -> do
                for_ stepRange $ \steps -> do
                    let pos = step direction steps minNode.pos
                        node = Node pos $ case direction of
                            Up -> [Left, Right]
                            Down -> [Left, Right]
                            _ -> [Up, Down]
                    when (fst pos >= 0 && fst pos < height && snd pos >= 0 && snd pos < width) $ do
                        insertCandidate node $ heatLoss + sum [heatLossAt $ step direction i minNode.pos | i <- [1..steps]]
            calc height width stepRange heatLossAt

main :: IO ()
main = do
    text <- readFileUtf8 "data/day17.txt"
    let heatLossMap = HashMap.fromList $ do
            (i, line) <- zip [0..] $ lines $ Text.unpack text
            (j, char) <- zip [0..] line
            pure ((i, j), RIO'.read [char])
        heatLossAt pos = RIO'.fromJust $ HashMap.lookup pos heatLossMap
        height = length $ lines $ Text.unpack text
        width = length $ List'.head $ lines $ Text.unpack text
        solve stepRange = flip State.evalState (Candidates mempty mempty) $ do
            let destination = (height - 1, width - 1)
            insertCandidate (Node destination [Up, Left]) (heatLossAt destination)
            result <- calc height width stepRange heatLossAt
            pure $ result - heatLossAt (0, 0)
    print $ solve [1..3]
    print $ solve [4..10]
