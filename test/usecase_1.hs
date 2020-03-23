-- module Main where

import ActionFw
import ActionFw.DataStore
import ActionFw.BehaviourLib
  ( moveBehaviour )
import ActionFw.BehaviourTrees21
  ( Event(..)
  , EventMsg(..)
  , RunTree
  , RunResult
  , IdRunResult
  , SpawnRunTree
  , GenNode(..)
  , FsmNode(..)
  , PlannerNode(..)
  , BehaviourNode(..)
  , runMoveToCloserNode
  , runFromCloserSeqNode
  , runNode
  , runBehNode
  , runSelNode
  , runSeqNode
  , runParNode
  , runPrioNode
  , runFsmNode
  , runPlannerNode
  , handleEvent
  , moveSpawnList
  , moveBehaviour
  , shootIdBehaviour
  , followIdBehaviour
  , Transition
  , handle
  , handleGen
  , BNode
  , Success(..) )
import Data.Map.Lazy as Map (insert)
import Data.Tree (Tree(Node), drawTree)
import Data.ByteString.Char8 (pack)
import Control.Monad (foldM, Monad, mzero)
-- import Control.Monad.Trans.Maybe (MaybeT(..))
-- import Control.Monad.Trans.Class (lift)
import Data.Dynamic (Dynamic(..), toDyn, fromDynamic)

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

harassNodeSpawn2 = runParNode "harass2" [followIdNodeSpawn, shootIdNodeSpawn]
shootIdNodeSpawn = runBehNode "shootId" (BehaviourNode "shoot" $ shootIdBehaviour $ pack "someAgent")
followIdNodeSpawn = runBehNode "followId" (BehaviourNode "follow" $ followIdBehaviour $ pack "someAgent")

rootNodeSpawn :: SpawnRunTree
rootNodeSpawn = runSeqNode "alert_loop" rootSpawnList
  where
    rootSpawnList = repeat alertNodeSpawn
    alertNodeSpawn = runSelNode "alert" alertSpawnList
    alertSpawnList = cycle [patrolNodeSpawn, harassNodeSpawn2]


{--
maybeFoldM' :: (Monad m) => (a -> b -> MaybeT m a) -> a -> [b] -> m (Maybe a)
maybeFoldM' f = (runMaybeT .) . foldM f
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evts) numEvtList

go :: Int -> Int -> MaybeT IO Int
go s i = do
  lift $ putStrLn $ "i = " ++ show i
  if i <= 4 then return (s+i) else mzero

test n = do
  myfoldM' go 0 [1..n] >>= print      
--}


-- type RunResult = (Maybe (RunTree, [Behaviour]), [Event])
iterateBT :: Int -> RunTree -> (InternalMem, BlackBoard) -> EventMsg -> IO IdRunResult
iterateBT n stateTree env evt = do
    putStrLn $ "- " ++ (show n) ++ " -"
    putStrLn $ drawTree $ fmap (show) stateTree
    putStrLn $ show evt
    let res@(maybeTreeAndBeh, newEvtList) = handleEvent stateTree evt env
    let (newStrTree, newStrBehList) = translate maybeTreeAndBeh
    putStrLn newStrTree
    print newEvtList
    print "<---------------------------------------->"
    return res
      where
        translate maybeTreeAndBeh = case maybeTreeAndBeh of
          Just (newStateTree, newBehList) -> (drawTree $ fmap (show) newStateTree, show newBehList)
          Nothing -> ("<none>", "<none>")

moveId = (++) "move-" . show
extractId (Node (GenNode id1 _ _) _) = id1

trace0_1 :: (InternalMem, BlackBoard) -> IO ()
trace0_1 env = do
    let x = (0, 0)
    let (Just (runTree, behs), evts) = runBehNode (moveId x) (BehaviourNode (moveId x) (moveBehaviour x) ) env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show behs
    print $ toDyn Success
    let (Node gn forest) = runTree
    let someEvt = (EventMsg ["move-(0,0)"] (toDyn Success))
    let evtList = [someEvt]
    let (EventMsg [nid] et) = someEvt
    let someRes = handleGen gn forest (nid, et) env
    print "........"
    print runTree    
    print someRes
    print "........"
    let numEvtList = zip [1..(length evtList)] evtList
    result <- foldM (\tree (n, evt) -> case tree of
        (Just (r, b), e) -> iterateBT n r env evt
        (Nothing, e) -> mzero ) (Just (runTree, behs), evtMsgs) numEvtList
    return ()

trace0 :: (InternalMem, BlackBoard) -> IO ()
trace0 env = do
    let (Just (runTree, behs), evts) = cycleMoveNodeSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ show behs
    print $ toDyn Success
    let evtList = [(EventMsg ["patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["patrol", "move-(3,3)"] (toDyn Success)),
                   (EventMsg ["patrol", "move-(8,8)"] (toDyn Success)),
                   (EventMsg ["patrol", "move-(10,10)"] (toDyn Success)),
                   (EventMsg ["patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["patrol", "move-(3,3)"] (toDyn Failure))];
    let numEvtList = zip [1..(length evtList)] evtList
    result <- foldM (\tree (n, evt) -> case tree of
        (Just (r, b), e) -> iterateBT n r env evt
        (Nothing, e) -> mzero ) (Just (runTree, behs), evtMsgs) numEvtList
    return ()

trace1 :: (InternalMem, BlackBoard) -> IO ()
trace1 env = do
    let (Just (runTree, behs), evts) = rootNodeSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ show behs
    let evtList = [(EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "move-(8,8)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "move-(10,10)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(3,3)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(8,8)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(10,10)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(3,3)"] (toDyn Failure)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "followId"] (toDyn Success))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()


-- type (Transition state event) = event -> FsmNode event state -> [RunTree] -> (Maybe (RunTree, [Behaviour]), [Event])
-- data FsmNode event state = FsmNode state (Transition state event)
-- runFsmNode :: (Show state, Typeable event) => FsmNode event state -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- handleFsmNode :: FsmNode event state -> [RunTree] -> (NodeId, event) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- handle :: n -> [RunTree] -> (NodeId, et) -> SpawnRunTree
-- type SpawnRunTree = (InternalMem, BlackBoard) -> RunResult
type HarassFsmNode = FsmNode Success ()

harassStateNodeSpawn :: SpawnRunTree
harassStateNodeSpawn = runFsmNode "harassFsmId" (FsmNode () harassStateTransition) [followIdNodeSpawn, shootIdNodeSpawn]
  where
    shootIdNodeSpawn = runBehNode "shootId" (BehaviourNode "shoot" $ shootIdBehaviour $ pack "someAgent")
    followIdNodeSpawn = runBehNode "followId" (BehaviourNode "follow" $ followIdBehaviour $ pack "someAgent")

-- type (Transition state event) = event -> state -> (Maybe (RunTree, [Behaviour]), [Event])
-- data FsmNode event state = FsmNode state (Transition state event)
-- type HarassTrans... nah, not a good name :-/
harassStateTransition :: Transition () Success
harassStateTransition Success id self tree _ = (Just (Node (GenNode id [] $ self) tree, []), [])
harassStateTransition Running id self tree _ = (Just (Node (GenNode id [] $ self) tree, []), [])
harassStateTransition Failure id _ _ env = hideStateNodeSpawn env

-- type RunTree = Tree GenNode -- Node GenNode [RunTree]
-- type RunResult = (Maybe (RunTree, [Behaviour]), [Event])
-- type SpawnRunTree = (InternalMem, BlackBoard) -> RunResult
    
hideStateNodeSpawn :: SpawnRunTree
hideStateNodeSpawn = runFsmNode "hideFsmId" (FsmNode () hideStateTransition) [moveNodeSpawn]
  where
    moveNodeSpawn = runBehNode "hideId" (BehaviourNode "hide" (moveBehaviour (20,20)))

hideStateTransition :: Transition () Success
hideStateTransition Success id _ _ env = harassStateNodeSpawn env
hideStateTransition Failure id self tree _ = (Just (Node (GenNode id [] $ self) tree, []), [])
hideStateTransition Running id self tree _ = (Just (Node (GenNode id [] $ self) tree, []), [])

-- type (Transition state event) = event -> NodeId -> FsmNode event state -> [RunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- type RunTree = Tree GenNode -- Node GenNode [RunTree]
-- type RunResult = (Maybe (RunTree, [Behaviour]), [Event])
-- data GenNode = forall n et. (Show n, BNode n et) => GenNode NodeId [NodeId] n

trace2 :: (InternalMem, BlackBoard) -> IO ()
trace2 env = do
    let (Just (runTree, behs), evts) = harassStateNodeSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ drawTree $ fmap (show) runTree
    print $ show behs
    let evtList = [(EventMsg ["harassFsmId"] (toDyn Running)),
                   (EventMsg ["harassFsmId"] (toDyn Running)),
                   (EventMsg ["harassFsmId"] (toDyn Running)),
                   (EventMsg ["harassFsmId"] (toDyn Running)),
                   (EventMsg ["harassFsmId"] (toDyn Running)),
                   (EventMsg ["harassFsmId", "shootId"] (toDyn Failure)),
                   (EventMsg ["hideFsmId", "hideId"] (toDyn Failure)),
                   (EventMsg ["hideFsmId", "hideId"] (toDyn Failure)),
                   (EventMsg ["hideFsmId", "hideId"] (toDyn Success))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()


trace4 :: (InternalMem, BlackBoard) -> IO ()
trace4 env = do
    let (Just (runTree, behs), evts) = rootNodeSpawn env
    let evts = []
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ show behs
    let evtList = [(EventMsg ["move-(8,8)"] (toDyn Success)),
                   (EventMsg ["move-(10,10)"] (toDyn Success)),
                   (EventMsg ["move-(0,0)"] (toDyn Success)),
                   (EventMsg ["move-(3,3)"] (toDyn Success)),
                   (EventMsg ["move-(8,8)"] (toDyn Success)),
                   (EventMsg ["move-(10,10)"] (toDyn Success)),
                   (EventMsg ["move-(0,0)"] (toDyn Success)),
                   (EventMsg ["move-(3,3)"] (toDyn Failure)),
                   (EventMsg ["followId"] (toDyn Success))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()

trace3 :: (InternalMem, BlackBoard) -> IO ()
trace3 env = do
    let (Just (runTree, behs), evts) = rootNodeSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ show behs
    let evtList = [(EventMsg ["move-(8,8)"] (toDyn Success)),
                   (EventMsg ["move-(10,10)"] (toDyn Success)),
                   (EventMsg ["move-(0,0)"] (toDyn Success)),
                   (EventMsg ["move-(3,3)"] (toDyn Success)),
                   (EventMsg ["move-(8,8)"] (toDyn Success)),
                   (EventMsg ["move-(10,10)"] (toDyn Success)),
                   (EventMsg ["move-(0,0)"] (toDyn Success)),
                   (EventMsg ["move-(3,3)"] (toDyn Failure)),
                   (EventMsg ["followId"] (toDyn Success))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()
    
data ExampleState = StateA | StateB | StateC
data ExampleEvent = Evt1 | Evt2 | Evt3 | Evt4 | Evt5
{--
Let's say that:
StateA:
  Evt1 -> State B
  Evt2 -> State C
  _    -> State A
State B:
  Evt3 -> State C
  Evt5 -> State A
  _    -> State B
State C:
  Evt4 -> State B
  Evt5 -> State A
  _    -> State C
--}
-- Let's  
-- type (Transition state event) = event -> state -> (Maybe (RunTree, [Behaviour]), [Event])
-- data FsmNode event state = FsmNode state (Transition state event)

data ExTrans = Transition ExampleState ExampleEvent
-- Example of an FSM that initiates with an env

rootFsmSpawn :: SpawnRunTree
rootFsmSpawn = runSeqNode "alert_loop" rootSpawnList
  where
    shootIdNodeSpawn = runBehNode "shootId" (BehaviourNode "shootId" $ shootIdBehaviour $ pack "someAgent")
    followIdNodeSpawn = runBehNode "followId" (BehaviourNode "followId" $ followIdBehaviour $ pack "someAgent")
    harassList2 = [followIdNodeSpawn, shootIdNodeSpawn]
    harassNodeSpawn2 = runParNode "harass2" harassList2
    alertSpawnList = cycle [patrolNodeSpawn, harassNodeSpawn2]
    alertNodeSpawn = runSelNode "alert" alertSpawnList
    rootSpawnList = repeat alertNodeSpawn

trace5 env = do
    let (Just (runTree, behs), evts) = rootFsmSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    print $ show runTree
    print $ show behs
    let evtList = [(EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "move-(8,8)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "move-(10,10)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(3,3)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(8,8)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(10,10)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "patrol", "move-(3,3)"] (toDyn Failure)),
                   (EventMsg ["alert_loop", "alert", "complete_patrol", "prepatrol", "followId"] (toDyn Success))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()
-- type RunResult = (Maybe (RunTree, [Behaviour]), [Event])
-- iterateBT :: Int -> RunTree -> (InternalMem, BlackBoard) -> EventMsg -> IO RunResult
    
trace6 env = do
    let planSpawn = runPlannerNode "planId" (PlannerNode "some_constraints")
    let patrolTillPlanSpawn = runPrioNode "patrolTillPlanId" [planSpawn, cycleMoveNodeSpawn]
    let (Just (runTree, behs), evts) = patrolTillPlanSpawn env
    let id1 = extractId runTree
    let evtMsgs = map (EventMsg [id1]) evts
    let id1 = extractId runTree
    print $ show runTree
    print $ show behs
    let evtList = [(EventMsg ["patrolTillPlanId", "patrol", "move-(0,0)"] (toDyn Success)),
                   (EventMsg ["patrolTillPlanId", "planId"] (toDyn (Just harassNodeSpawn2)))];
    let numEvtList = zip [1..(length evtList)] evtList
    (Just (runTree, behs), evts) <- foldM (\(Just (r, b), e) (n, evt) -> iterateBT n r env evt) (Just (runTree, behs), evtMsgs) numEvtList
    return ()
    


main :: IO ()
main = do
    let myMem = emptyMem
    let addFoeId = Map.insert "foeId" ["someFoeId"]
    let bb = BlackBoard [] 3 50 (7,7) 0 0 0
    let env = (myMem, bb)
    print "---"
    trace0_1 env
    trace0 env
    trace2 env -- Check intermediate <<Success>> results...how?
    -- Also, the FSM is quite unassuming :-/ Try something else
    trace6 env
    

