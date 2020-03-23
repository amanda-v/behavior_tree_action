-- module Main where

import ActionFw
import ActionFw.Utils
import ActionFw.DataStore
import ActionFw.BehaviourLib
  ( moveBehaviour ) 
import ActionFw.BehaviourTrees2
  ( Event(..)
  , RunTree
  , SpawnRunTree
  , GenNode(..)
  , runMoveToCloserNode
  , runFromCloserSeqNode
  , runBehNode
  , runSelNode
  , runSeqNode
  , runParNode
  , runFsmNode
  , handleEvent
  , moveSpawnList
  , shootIdBehaviour
  , followIdBehaviour
  , Transition )
import Data.Map.Lazy as Map (insert)
import Data.Tree (Tree(Node), drawTree)
import Data.ByteString.Char8 (pack)
import Control.Monad (foldM)

-- keep patrol, if fails....
-- start harass, if succeeds.....
-- patrol

patrolPoints = [(0,0), (3,3), (8,8), (10,10)]
moveToCloserSpawn = runMoveToCloserNode "moveToCloser" patrolPoints
-- Fix patrol node: the patrolBehaviours needs to start at the closest node...
-- ...and continue in sequence
prePatrolNodeSpawn = runFromCloserSeqNode "prepatrol" patrolPoints 
cycleMoveNodeSpawn = runSeqNode "patrol" $ moveSpawnList $ cycle patrolPoints
patrolNodeSpawn = runSeqNode "complete_patrol" [prePatrolNodeSpawn, cycleMoveNodeSpawn]

rootNodeSpawn :: SpawnRunTree
rootNodeSpawn = runSeqNode "alert_loop" rootSpawnList
  where
    shootIdNodeSpawn = runBehNode "shootId" (shootIdBehaviour $ pack "someAgent")
    followIdNodeSpawn = runBehNode "followId" (followIdBehaviour $ pack "someAgent")
    harassList2 = [followIdNodeSpawn, shootIdNodeSpawn]
    harassNodeSpawn2 = runParNode "harass2" harassList2
    alertSpawnList = cycle [patrolNodeSpawn, harassNodeSpawn2]
    alertNodeSpawn = runSelNode "alert" alertSpawnList
    rootSpawnList = repeat alertNodeSpawn


iterateBT :: Int -> RunTree -> (InternalMem, BlackBoard) -> Event -> IO (RunTree, [Event], [Behaviour])
iterateBT n stateTree env evt = do
    print $ "- " ++ (show n) ++ " -"
    let (newStateTree, newEvtList, newBehList) = handleEvent stateTree env evt
    print $ drawTree $ fmap (show) newStateTree
    print $ show newEvtList
    print $ show newBehList
    print "-----"
    print ""
    return (newStateTree, newEvtList, newBehList)

isNotNullNode (Node NullNode []) = False
isNotNullNode _ = True


trace1 :: (InternalMem, BlackBoard) -> IO ()
trace1 env = do
    
    let (runTree, behs) = rootNodeSpawn env
    let evts = []
    print $ show runTree
    print $ show behs
    let evtList = [(Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "move-(0,0)" "success"),
                   (Event "move-(3,3)" "success"),
                   (Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "move-(0,0)" "success"),
                   (Event "move-(3,3)" "fail"),
                   (Event "followId" "success")];
    let numEvtList = zip [1..(length evtList)] evtList
    (runTree, evts, behs) <- foldM (\(r, e, b) (n, evt) -> iterateBT n r env evt) (runTree, evts, behs) numEvtList

    print "~~~~~~~~~~~ Utils Test ~~~~~~~~~~~~~~~"    
    print $ show $ crack (encode (10) "haskell is fun")
    print $ show $ crack (encode (10) "What is haskell")
    print "~~~~~~~~~~~ The End of Utils Test ~~~~"

    return ()


harassStateNodeSpawn :: SpawnRunTree
harassStateNodeSpawn = runFsmNode "harass" harassStateTransition [followIdNodeSpawn, shootIdNodeSpawn]
  where
    shootIdNodeSpawn = runBehNode "shootId" (shootIdBehaviour $ pack "someAgent")
    followIdNodeSpawn = runBehNode "followId" (followIdBehaviour $ pack "someAgent")
    
-- type Transition = Event -> (InternalMem, BlackBoard) -> RunTree -> (RunTree, [Event], [Behaviour])
harassStateTransition :: Transition
harassStateTransition (Event _ "success") _ self = (self, [], [])
harassStateTransition (Event _ "progress") _ self = (self, [], [])
harassStateTransition (Event _ "fail") env _ = (hideStateTree, [], behList)
  where
    (hideStateTree, behList) = hideStateNodeSpawn env
    
hideStateNodeSpawn :: SpawnRunTree
hideStateNodeSpawn = runFsmNode "hide" hideStateTransition [moveNodeSpawn]
  where
    moveNodeSpawn = runBehNode "hide" (moveBehaviour (20,20))

hideStateTransition :: Transition
hideStateTransition (Event "hide" "fail") _ self = (self, [], [])
hideStateTransition (Event "hide" "success") env _ = (hideStateTree, [], behList)
  where
    (hideStateTree, behList) = harassStateNodeSpawn env
hideStateTransition self env evt = (Node NullNode [], [], [])

trace2 :: (InternalMem, BlackBoard) -> IO ()
trace2 env = do
    let (runTree, behs) = harassStateNodeSpawn env
    let evts = []
    print $ show runTree
    print $ drawTree $ fmap (show) runTree
    print $ show behs
    let evtList = [(Event "shootId" "progress"),
                   (Event "shootId" "progress"),
                   (Event "followId" "progress"),
                   (Event "followId" "progress"),
                   (Event "shootId" "progress"),
                   (Event "shootId" "fail"),
                   (Event "hide" "fail"),
                   (Event "hide" "fail"),
                   (Event "hide" "success")];
    let numEvtList = zip [1..(length evtList)] evtList
    (runTree, evts, behs) <- foldM (\(r, e, b) (n, evt) -> iterateBT n r env evt) (runTree, evts, behs) numEvtList
    return ()


trace4 :: (InternalMem, BlackBoard) -> IO ()
trace4 env = do
    let (runTree, behs) = rootNodeSpawn env
    let evts = []
    print $ show runTree
    print $ show behs
    let evtList = [(Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "move-(0,0)" "success"),
                   (Event "move-(3,3)" "success"),
                   (Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "move-(0,0)" "success"),
                   (Event "move-(3,3)" "fail"),
                   (Event "followId" "success")];
    let numEvtList = zip [1..(length evtList)] evtList
    (runTree, evts, behs) <- foldM (\(r, e, b) (n, evt) -> iterateBT n r env evt) (runTree, evts, behs) numEvtList
    return ()

trace3 :: (InternalMem, BlackBoard) -> IO ()
trace3 env = do
    let (runTree, behs) = rootNodeSpawn env
    let evts = []
    print $ show runTree
    print $ show behs
    let evtList = [(Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "shootId" "progress"),
                   (Event "move-(3,3)" "success"),
                   (Event "move-(8,8)" "success"),
                   (Event "move-(10,10)" "success"),
                   (Event "move-(0,0)" "success"),
                   (Event "move-(3,3)" "fail"),
                   (Event "followId" "success")];
    let numEvtList = zip [1..(length evtList)] evtList
    (runTree, evts, behs) <- foldM (\(r, e, b) (n, evt) -> iterateBT n r env evt) (runTree, evts, behs) numEvtList
    
    return ()

main :: IO ()
main = do
    let myMem = emptyMem
    let addFoeId = Map.insert "foeId" ["someFoeId"]
    let bb = BlackBoard [] 3 50 (7,7) 0 0 0 -- [AgentPos] -- List of other seen agents Grenades Bullets Pos -- Own pos Orientation SoundLvl LightLvl
    let env = (myMem, bb)
    print "-------------------------"
    
    -- trace1 env

    trace3 env

