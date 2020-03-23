{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}

module ActionFw.BehaviourTrees3 where

import ActionFw
import ActionFw.DataStore
import ActionFw.BehaviourLib (moveBehaviour, haveBullets)
import Data.Tree (Tree(..), Forest)
import Data.List (cycle, filter, null, foldl1', unfoldr)
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Lazy as Map

{--
"Class" of nodes; running nodes may have other state;
--}

--------------------------------------------------------------------
-- BEHAVIOUR NODES -------------------------------------------------
--------------------------------------------------------------------


type NodeId = String
type RunTree = Tree GenNode
type EventType = String
-- a parameterized payload?
data EventMsg = EventMsg [NodeId] Event
  deriving (Show, Typeable)
type Event = Dynamic

type RunResult = (Maybe (RunTree, [Behaviour]), [Event])



data Tree up = forall down. Node (GenNode up down) [Tree GenNode down] 




type SpawnRunTree = (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
type RunTree = Tree GenNode

-- class (Typeable et) => BNode n et | n -> et where
--     handle :: n -> [RunTree] -> (NodeId, et) -> SpawnRunTree
class BNode n up down | n -> up where
    handle :: n -> [RunTree] -> (NodeId, down) -> SpawnRunTree

{--
type Forest n = [Tree n]
data Tree n = Node n (Forest n)
data Tree up = Node (GenNode up down) [Tree GenNode down] 
--}

-- type RunTree up = Tree GenNode
data GenNode up = forall n down. (Show n, BNode n up down) => GenNode n
--     handle :: n -> [RunTree] -> (NodeId, et) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])


same :: RunTree -> SpawnRunTree
same rt = \_ -> (Just (rt, []), [])

data GenNode = forall n. (Show n, BNode n u d) => GenNode n

data GenNode = forall n et. (Show n, BNode n et) => GenNode NodeId [NodeId] n
-- own id, parent ids
instance Show GenNode where
    show (GenNode id _ n) = show id ++ ":" ++ show n
instance BNode GenNode Event where
    handle gn@(GenNode id _ n) runTree (node, devt) = case (fromDynamic devt) of
      Nothing -> same (Node gn runTree)
      Just evt -> handle n runTree (node, evt)
    -- run (GenNode id _ n) = run n















{--

handleEvent :: RunTree -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
handleEvent (Node bn forest) env evt = (Node newBn newStateTree, newEvtList, newBehList)
  where
    (subTreeList, subEvtListList, subBehListList) = unzip3 $ map (\t -> handleEvent t env evt) forest
    subEvtList = concat subEvtListList
    evtList = case forest of
                [] -> [evt]
                _  -> subEvtList
    isNotNullNode (Node NullNode []) = False
    isNotNullNode _ = True
    purgedSubTreeList = [t | t <- subTreeList, isNotNullNode t]
    processEvent (Node n f, oldevt, oldbeh) x = (Node newn newf, oldevt ++ newevt, oldbeh ++ newbeh)
      where
        (Node newn newf, newevt, newbeh) = handleWithTree f n env x
    (Node newBn newStateTree, newEvtList, thisBehList) =
      foldl processEvent (Node bn purgedSubTreeList, [], []) evtList
    newBehList = thisBehList ++ concat subBehListList


{-----------------------------------}

data Success = Success | Failure

data SeqNode = SeqNode NodeId [SpawnRunTree]

runSeqNode :: NodeId -> [SpawnRunTree] -> SpawnRunTree
runSeqNode id (firstSp:restSp) env = (runTree, behaviours)
  where
    runTree = Node (GenNode $ SeqNode id restSp) [nextTree]
    (nextTree, behaviours) = firstSp env
instance Show SeqNode where
    show (SeqNode id list) = "SEQ[" ++ id ++ "] - " -- ++ (show $ firstItems) ++ restDots
instance BNode SeqNode where
    -- handle = handleSeqNode
    handleWithTree _ = handleSeqNode
    getId = getIdSeq
getIdSeq :: SeqNode -> String
getIdSeq (SeqNode id _) = id
handleSeqNode :: SeqNode -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
handleSeqNode (SeqNode id []) env (Event idEvent "success") = (Node NullNode [], [(Event id "success")], [])
handleSeqNode (SeqNode id (firstSp:restSp)) env (Event idEvent "success") = (Node (GenNode $ SeqNode id restSp) [nextTree], [], behaviours)
  where
    (nextTree, behaviours) = firstSp env
handleSeqNode (SeqNode id _) _ (Event idEvent "fail") = ((Node NullNode []), [(Event id "fail")], [])
handleSeqNode (SeqNode id _) _ _ = ((Node NullNode []), [(Event id "fail")], [])

{-----------------------------------}

data SelNode = SelNode NodeId [SpawnRunTree]
runSelNode :: NodeId -> [SpawnRunTree] -> SpawnRunTree
runSelNode id (firstSp:restSp) env = (runTree, behaviours)
  where
    runTree = Node (GenNode $ SelNode id restSp) [nextTree]
    (nextTree, behaviours) = firstSp env
instance Show SelNode where
    show (SelNode id list) = "SEL[" ++ id ++ "] - " -- ++ (show $ firstItems) ++ restDots
instance BNode SelNode where
    -- handle = handleSelNode
    handleWithTree _ = handleSelNode
    getId = getIdSel
getIdSel :: SelNode -> String
getIdSel (SelNode id _) = id
handleSelNode :: SelNode -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
handleSelNode (SelNode id []) env (Event _ "fail") = (Node NullNode [], [(Event id "fail")], [])
handleSelNode (SelNode id (firstSp:restSp)) env (Event _ "fail") = (Node (GenNode $ SelNode id restSp) [nextTree], [], behaviours)
  where
    (nextTree, behaviours) = firstSp env
handleSelNode (SelNode id _) _ (Event _ "success") = ((Node NullNode []), [(Event id "success")], [])
handleSelNode (SelNode id _) _ _ = ((Node NullNode []), [(Event id "fail")], [])

{-----------------------------------}

data ParNode = ParNode NodeId
runParNode :: NodeId -> [SpawnRunTree] -> SpawnRunTree
runParNode id spawnList env = (runTree, concat nextBehsList)
  where
    (nextTreeList, nextBehsList) = unzip $ map ($ env) spawnList
    runTree = Node (GenNode $ ParNode id) nextTreeList
instance Show ParNode where
    show (ParNode id) = "PAR[" ++ id ++ "] - " -- ++ (show $ firstItems) ++ restDots
instance BNode ParNode where
    handleWithTree = handleParNode
    getId = getIdPar
getIdPar :: ParNode -> String
getIdPar (ParNode id) = id
handleParNode :: [RunTree] -> ParNode -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
-- We'll assume that a parallel node fails if *all*
-- child nodes fail, and succeeds if *any* suceeds
-- FIXME find an appropriate definition for "fail" and "success" for
-- parallel nodes
handleParNode [] (ParNode id) env (Event _ "fail") = (Node NullNode [], [(Event id "fail")], [])
handleParNode treeNodeList (ParNode idNode) env (Event idEvt "fail") = (Node newNode newTreeNodeList, newEvents, [])
  where
    (newNode, newEvents) = if (null newTreeNodeList)
                             then (GenNode $ ParNode idNode, [])
                             else (NullNode, [(Event idNode "fail")])
    newTreeNodeList = [treeNode | treeNode <- treeNodeList, treeNodeId treeNode /= idEvt]
    treeNodeId (Node node _) = getId node
handleParNode _ (ParNode idNode) env (Event idEvt "success") = (Node newNode [], newEvents, [])
  where
    (newNode, newEvents) = (NullNode, [(Event idNode "success")])
-- handleParNode (ParNode id _) _ (Event _ "success") = ((Node NullNode []), [(Event id "success")], [])
-- handleParNode (ParNode id _) _ _ = ((Node NullNode []), [(Event id "fail")], [])

{-----------------------------------}

-- THE FSM NODE TRANSFORMS ITSELF INTO ANOTHER STATE
-- FSM[Running] -> RunAwayBehaviour
--   v
-- FSM[Attacking] -> AttackBehaviour, ApproachBehaviour

-- These are states; is there any generic FSM we could use?
-- type SpawnRunTree = (InternalMem, BlackBoard) -> (RunTree, [Behaviour])

type Transition = Event -> (RunTree, [Event], [Behaviour])

{--
tr1 "a" = (self, [], [])
tr1 "b" = (tree, ["upperB"], behList)
  where
    (tree, behList) = spawn env
    
A fun that takes an event, and either a runtree or a spawnruntree,
and returns a result
--}

data FsmNode = FsmNode NodeId Transition
runFsmNode :: NodeId -> Transition -> SpawnRunTree -> SpawnRunTree
runFsmNode id transition spawn env = (runTree, nextBehsList)
  where
    (nextTreeList, nextBehsList) = spawn env
    runTree = Node (GenNode $ FsmNode id transition) nextTreeList
instance Show FsmNode where
    show (FsmNode id _) = "FSM[" ++ id ++ "] - "
instance BNode FsmNode where
    handleWithTree = handleFsmNode
    getId = getIdFsm
getIdFsm :: FsmNode -> String
getIdFsm (FsmNode id _) = id
handleFsmNode :: [RunTree] -> FsmNode -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
handleFsmNode _ fsm@(FsmNode id transition) env evt = (transition env evt) env

-- TODO How can we specify a "self" transition?
-- Either a spawn env, or the existing RunTree

{-----------------------------------}

-- [Atomic] Behaviour node
data BehaviourNode = BehaviourNode NodeId
runBehNode :: NodeId -> Behaviour -> SpawnRunTree
runBehNode id b env = (runTree, behaviours)
  where
    runTree = Node (GenNode $ BehaviourNode id) []
    behaviours = [b]
instance Show BehaviourNode where
    show (BehaviourNode id) = "BEHAVIOUR[" ++ id ++ "]"
instance BNode BehaviourNode where
    handleWithTree _ = handleBehNode
    -- handle = handleBehNode
    getId = getIdBeh
getIdBeh :: BehaviourNode -> String
getIdBeh (BehaviourNode id) = id
handleBehNode :: BehaviourNode -> (InternalMem, BlackBoard) -> Event -> (RunTree, [Event], [Behaviour])
handleBehNode (BehaviourNode id1) env (Event id2 "fail")
    | id1 == id2 = (Node NullNode [], [(Event id1 "fail")], [])
handleBehNode (BehaviourNode id1) env (Event id2 "success")
    | id1 == id2 = (Node NullNode [], [(Event id1 "success")], [])
handleBehNode (BehaviourNode id1) env (Event id2 "progress")
    | id1 == id2 = (Node (GenNode $ BehaviourNode id1) [], [(Event id1 "progress")], [])
handleBehNode (BehaviourNode id) _ _ = ((Node (GenNode $ BehaviourNode id) []), [], [])

{-----------------------------------}

runMoveToCloserNode :: NodeId -> [Pos] -> SpawnRunTree
runMoveToCloserNode id posList (mem, bb) | length posList > 0
  = (runTree, behaviours)
  where
    BlackBoard _ _ _ curPos _ _ _ = bb
    runTree = Node (GenNode $ BehaviourNode id) []
    closeList = [(p, d p curPos) | p <- posList]
    d (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
    (closest, _) = foldl1' (\tmin@(pmin,dmin) t@(p,d) -> if d<dmin then t else tmin) closeList
    behaviours = [moveBehaviour closest]

runFromCloserSeqNode id posList env@(mem, bb) | length posList > 0
  = (runTree, behaviours)
  where
    BlackBoard _ _ _ curPos _ _ _ = bb
    d (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
    -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    pointSubListDistList = unfoldr (\x -> case x of [] -> Nothing; l@(p:xs) -> Just ((p,l,d p curPos),xs)) posList
    -- (a -> a -> a) -> [a] -> a 
    closestSubList :: [Pos]
    (closest, closestSubList, _) = foldl1' (\tmin@(pmin,smin,dmin) t@(p,s,d) -> if d<dmin then t else tmin) pointSubListDistList
    spawnList = moveSpawnList closestSubList
    firstSp = head spawnList
    restSp = tail spawnList
    runTree = Node (GenNode $ SeqNode id restSp) [nextTree]
    (nextTree, behaviours) = firstSp env

moveSpawnList posList = moveBehavioursList
  where
    moveId = (++) "move-" . show
    moveBehavioursList = map (\x -> runBehNode (moveId x) (moveBehaviour x)) posList

--------------------------------------------------------------------
-- BEHAVIOURS ------------------------------------------------------
--------------------------------------------------------------------

followIdPre :: [Precond]
followIdPre = []
-- Problem!!!! The only postcondition we can assure is that we will be closer to the selected ID
-- So, a postcond may not be a new BB, but rather a *condition* on the new BB: BB -> Bool (maybe???)
followIdPost :: Postcond
-- FIXME
followIdPost = id
followIdFun :: AgentStr -> Updater
followIdFun agId (BlackBoard ags _ _ _ _ _ _) mem = Action (move) (Nothing)
    -- get pos of the ID and move towards it
    where
        move = case select agId ags of
            [] -> Nothing
            (_,pos):xs -> Just $ Move pos
        select agId ags = filter ((==agId).fst) ags

followIdBehaviour :: AgentStr -> Behaviour
followIdBehaviour _ = Behaviour ("agent", "follow") 20 (const $ const True) (UpdaterFun $ followIdFun agId) followIdPre followIdPost
    where
        -- FIXME retrieve from the internalmem
        agId = pack "example_foe"
        
------------------------------

foeIdAround :: AgentStr -> Precond
foeIdAround agId (BlackBoard ags _ _ _ _ _ _) _ = case filter ((==agId).fst) ags of
    [] -> False
    (_,pos):xs -> True

shootIdPre :: AgentStr -> [Precond]
shootIdPre agId = [haveBullets, foeIdAround agId]

-- FIXME one possible postcondition is that the foe is down...
shootIdPost :: Postcond
shootIdPost (BlackBoard ags g b p o s l) =
    BlackBoard ags g (b-1) p o s l

shootIdFun :: AgentStr -> Updater
shootIdFun _ (BlackBoard [] _ b _ _ _ _) mem = Action (Nothing) (Nothing)
shootIdFun agId (BlackBoard ags _ b _ _ _ _) mem = Action (Nothing) shootAction
    where
        shootAction = case select agId ags of
           [] -> Nothing
           (_,pos):xs -> Just $ Shoot agId 0.0
        select agId ags = filter ((==agId).fst) ags

shootIdBehaviour :: AgentStr -> Behaviour
shootIdBehaviour _ = Behaviour ("agent", "shootId") 100 (\bb mem -> not $ foeIdAround agId bb mem) (UpdaterFun $ shootIdFun agId) (shootIdPre agId) shootIdPost 
    where
        -- FIXME retrieve from the param
        agId = pack "example_foe"

--}