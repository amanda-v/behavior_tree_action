{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ActionFw.BehaviourTrees21
  ( RunTree
  , RunResult
  , IdRunResult
  , SpawnRunTree
  , Event
  , EventMsg(..)
  , GenNode(GenNode)
  , runMoveToCloserNode
  , runFromCloserSeqNode
  , runBehNode
  , runSeqNode
  , runSelNode
  , runParNode
  , runPrioNode
  , runFsmNode
  , runPlannerNode
  , runNode
  , shootIdBehaviour
  , followIdBehaviour
  , moveBehaviour
  , BehaviourNode(..)
  , handleEvent
  , moveSpawnList
  , Transition
  , Success(..)
  , FsmNode(..)
  , PlannerNode(..)
  , BNode
  , handle
  , handleBehNode
  , handleGen
  ) where
  
import ActionFw
import ActionFw.DataStore
import ActionFw.BehaviourLib (moveBehaviour, haveBullets)
import Control.Monad (foldM)
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Tree (Tree(..), Forest)
import Data.List (cycle, filter, null, foldl', foldl1', unfoldr)
import Data.ByteString.Char8 (pack)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Typeable.Internal (Typeable)
import qualified Data.Map.Lazy as Map

{--
"Class" of nodes; running nodes may have other state;
--}

--------------------------------------------------------------------
-- BEHAVIOUR NODES -------------------------------------------------
--------------------------------------------------------------------

type NodeId = String
type RunTree = Tree GenNode -- Node GenNode [RunTree]
type EventType = String
-- a parameterized payload?
data EventMsg = EventMsg [NodeId] Event
  deriving (Show, Typeable)
type Event = Dynamic
-- type GenEvent = (NodeId, Dynamic)
type RunResult = (Maybe (RunTree, [Behaviour]), [Event])
type IdRunResult = (Maybe (RunTree, [Behaviour]), [EventMsg])

type SpawnRunTree = (InternalMem, BlackBoard) -> RunResult
class (Typeable et) => BNode n et | n -> et where
    handle :: n -> [RunTree] -> NodeId -> (NodeId, et) -> SpawnRunTree
    -- TODO Should handle carry a list of NodeIds instead of a single NodeId?
    -- FIXME this signature is possible! check if useful to return 
    -- appropriate types for handle
    -- handleBis :: GenNode -> et -> SpawnRunTree
    -- dummy :: forall n1 et1. (Show n1, BNode n et1) => n -> et -> n1 -> et1 -> SpawnRunTree
    -- Two situations: either handling the event means there is no
    -- new tree (Nothing) or that the same event is sent upwards
    -- (Just (orig_tree, []), [same_event_stripped_id])
    -- But it's a bit weird that the sender needs to care about this, specially
    -- if other alternatives do not make sense 
    -- run :: init -> SpawnRunTree

same :: RunTree -> SpawnRunTree
same rt = \_ -> (Just (rt, []), [])

data GenNode = forall n et. (Show n, BNode n et) => GenNode NodeId [NodeId] n
-- own id, parent ids
-- TODO IDoes GenNode really need to be an instance of BNode???
instance Show GenNode where
    show (GenNode id _ n) = show id ++ ":" ++ show n
handleGen gn@(GenNode id _ n) runForest (node, devt) = case (fromDynamic devt) of
    Just evt -> handle n runForest id (node, evt)
    Nothing -> error $ "1) Calling: " ++ (show gn) ++ " x " ++ (show runForest) ++ " x " ++ node ++ ":" ++ (show devt) 

-- we exploit the hierarchical structure of ids to check to whom the event goes
-- also, if we use serial events, then we need a priority
-- An EventMsg is a way of dispatching the event itself, which is actually Dynamic
handleEvent :: RunTree -> EventMsg -> (InternalMem, BlackBoard) -> IdRunResult
handleEvent rt@(Node gn@(GenNode id1 gids _) forest) devtmsg@(EventMsg (id2:ids) devt) env
  | id1 == id2 = 
    case ids of
      [] -> (maybeRtB, evtMsgList)
        where
          (maybeRtB, evtList) = handleGen gn forest ("", devt) env
          evtMsgList = map (EventMsg [id1]) evtList
      _ -> (finalTreeAndBehList, finalEvtList)
        where
          -- TODO Maybe use zippers here? 
          -- TODO Let's assume there's a single source
          (subMaybeTreeAndBehsList, subEvtMsgListList) = unzip $ map (\t -> updateChild (EventMsg ids devt) t) forest
          updateChild :: EventMsg -> RunTree -> (Maybe (RunTree, [Behaviour]), [EventMsg])
          updateChild childEvtMsg rtChild@(Node nChild@(GenNode idChild _ _) _) = handleEvent rtChild childEvtMsg env
          -- FIXME: Events MUST come with their attached IDs. The issue is that 
          -- this node may take actions (like success on parallel nodes) that
          -- depend on the origin
          -- They must be assembled here, and handled by the "next root node".
          -- We assume the events are handled from left to right in the active
          -- forest
          (subTreeList, subBehsList) = unzip $ catMaybes subMaybeTreeAndBehsList
          subEvtMsgList = concat subEvtMsgListList
          newRootNode = Node gn subTreeList
          
          (finalTreeAndBehList, finalEvtList) = foldl' (process) (Just (newRootNode, []), []) subEvtMsgList
          process :: IdRunResult -> EventMsg -> IdRunResult
          -- TODO Is it correct to neglect any events that remain to be treated 
          -- once it is decided that this node removes itself from the tree?
          process (Nothing, evtMsgList) evt = (Nothing, evtMsgList)
          process (Just (Node gn forest, someBehList), evtMsgList) (EventMsg (prevId:_) devt) = case handleGen gn forest (prevId, devt) env of
            (Just (newRtree, newBehList), newEvtList) ->
              (Just (newRtree, someBehList ++ newBehList),
               evtMsgList ++ (map (EventMsg (id1:[])) newEvtList) )
            (Nothing, newEvtList) ->
              (Nothing, map (EventMsg (id1:[])) newEvtList)
   | id1 /= id2 = (Just (rt, []), [])


firstNo = 2

{--
State (InternalMem, BlackBoard) (RunTree, [Behaviour])
-- unfold! but we need something in between to handle the events... 
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
--}


{-----------------------------------}

data Success = Success | Failure | Running
  deriving (Show, Typeable)

{--
class (Typeable et) => BNode n et | n -> et where
    handle :: n -> [RunTree] -> et -> SpawnRunTree
    run :: n -> SpawnRunTree
data GenNode = forall n et. (Show n, BNode n et) => GenNode NodeId [NodeId] n
type RunTree = Node GenNode [RunTree]
--}
  
data SeqNode = SeqNode [SpawnRunTree]
instance Show SeqNode where
    show (SeqNode list) = "SEQ" -- ++ (show $ firstItems) ++ restDots
instance BNode SeqNode Success where
    handle = handleSeqNode
runSeqNode :: NodeId -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
runSeqNode id (firstSp:restSp) env = (maybeTreeAndBehs, events)
  where
    (maybeTreeAndBehs, events) = case (firstSp env) of
      (Nothing, firstEvts) -> (Nothing, firstEvts)
      (Just (nextTree, behaviours), firstEvts) -> (Just (runTree, behaviours), firstEvts)
        where
          runTree = Node (GenNode id [] $ SeqNode restSp) [nextTree]
handleSeqNode :: SeqNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handleSeqNode (SeqNode []) _ ownId (id, Success) env = (Nothing, [toDyn Success])
handleSeqNode (SeqNode (firstSp:restSp)) _ ownId (id, Success) env = (maybeTreeAndBehs, events)
  where
    (maybeTreeAndBehs, events) = case (firstSp env) of
      (Nothing, firstEvts) -> (Nothing, firstEvts)
      (Just (nextTree, behaviours), firstEvts) -> (Just (runTree, behaviours), firstEvts)
        where
          runTree = Node (GenNode ownId [] $ SeqNode restSp) [nextTree]
-- Note that we actively ignore Running events
handleSeqNode n forest ownId (id, Running) env = (Just (runTree, []), [])
  where
    runTree = Node (GenNode ownId [] n) forest
handleSeqNode n _ _ (id, Failure) _ = (Nothing, [toDyn Failure])
handleSeqNode n _ _ _ _ = (Nothing, [toDyn Failure])

{-----------------------------------}

data SelNode = SelNode [SpawnRunTree]
instance Show SelNode where
    show (SelNode list) = "SEL" -- ++ (show $ firstItems) ++ restDots
instance BNode SelNode Success where
    handle = handleSelNode
runSelNode :: NodeId -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
runSelNode id (firstSp:restSp) env = (maybeTreeAndBehs, events)
  where
    (maybeTreeAndBehs, events) = case (firstSp env) of
      (Nothing, firstEvts) -> (Nothing, firstEvts)
      (Just (nextTree, behaviours), firstEvts) -> (Just (runTree, behaviours), firstEvts)
        where
          runTree = Node (GenNode id [] $ SelNode restSp) [nextTree]
handleSelNode :: SelNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handleSelNode (SelNode []) _ ownId (id, Failure) env = (Nothing, [toDyn Failure])
handleSelNode (SelNode (firstSp:restSp)) _ ownId (id, Failure) env = (maybeTreeAndBehs, events)
  where
    (maybeTreeAndBehs, events) = case (firstSp env) of
      (Nothing, firstEvts) -> (Nothing, firstEvts)
      (Just (nextTree, behaviours), firstEvts) -> (Just (runTree, behaviours), firstEvts)
        where
          runTree = Node (GenNode ownId [] $ SelNode restSp) [nextTree]
handleSelNode (SelNode _) _ _ (id, Success) _ = (Nothing, [toDyn Success])
handleSelNode (SelNode _) _ _ _ _ = (Nothing, [toDyn Failure])

{-----------------------------------}

data ParNode = ParNode
instance Show ParNode where
    show (ParNode) = "PAR" -- ++ (show $ firstItems) ++ restDots
instance BNode ParNode Success where
    handle = handleParNode
runParNode :: NodeId -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
runParNode id spawnList env = (Just (runTree, concat subBehsList), subEvtList)
  where
    (subMaybeTreeAndBehsList, subEvtListList) = unzip $ map ($ env) spawnList
    subEvtList = concat subEvtListList
    (subTreeList, subBehsList) = unzip $ catMaybes subMaybeTreeAndBehsList
    runTree = Node (GenNode id [] ParNode) subTreeList
handleParNode :: ParNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- We'll assume that a parallel node fails if *any*
-- child nodes fail, and succeeds if *any* suceeds
-- FIXME find an appropriate definition for "fail" and "success" for
-- parallel nodes
-- A REALLY APPEALING SEMANTICS FOR PARALLEL NODES!
-- A "Running" transition in any node inhibits the rest, and transforms the
-- parallel node into whatever was being tried  
handleParNode ParNode [] ownId (id, Failure) env = (Nothing, [toDyn Failure])
handleParNode ParNode forest ownId (id, Failure) env = (Just (Node (GenNode ownId [] ParNode) (filter (isOtherNodeId id) forest), []), [])
handleParNode ParNode _ ownId (id, Success) env = (Nothing, [toDyn Success])
handleParNode ParNode forest ownId (id, _) env = (Just (Node (GenNode ownId [] ParNode) forest, []), [])

isSameNodeId id n@(Node (GenNode idx _ _) _) = id==idx
isOtherNodeId id n@(Node (GenNode idx _ _) _) = id/=idx
{-----------------------------------}

data PrioNode = PrioNode NodeId
instance Show PrioNode where
    show (PrioNode prioId) = "PRIO{" ++ prioId ++ "}" -- ++ (show $ firstItems) ++ restDots
instance BNode PrioNode Success where
    handle = handlePrioNode
runPrioNode :: NodeId -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- TODO use a tuple?
runPrioNode ownId spawnList@(hiPrioSpawn:loPrioSpawn) env = (Just (runTree, concat subBehsList), subEvtList)
  where
    (subMaybeTreeAndBehsList@(hiTB:loTBList), subEvtListList) = unzip (map ($ env) spawnList)
    -- If the PrioNode init fails, it turns into a standard ParNode
    -- FIXME the id gets in the way!!!!
    subEvtList = concat subEvtListList ++ [toDyn Running]
    (subTreeList, subBehsList) = unzip $ catMaybes subMaybeTreeAndBehsList
    runTree = case hiTB of
      Nothing -> Node (GenNode ownId [] ParNode) subTreeList
      Just (Node (GenNode prioId [] n) _, _) -> Node (GenNode ownId [] $ PrioNode prioId) subTreeList

handleDebugPrioNode :: PrioNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handleDebugPrioNode pn rt nid (etid, et) env =
  error $ (show pn) ++ " " ++ (show rt) ++ " " ++ (show nid) ++ " " ++ (show etid) ++ " " ++ (show et)

handlePrioNode :: PrioNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handlePrioNode p [] ownId (id, Failure) env = (Nothing, [toDyn Failure])
handlePrioNode p forest ownId (id, Failure) env = (Just (Node (GenNode ownId [] p) (filter (isOtherNodeId id) forest), []), [])
handlePrioNode p _ ownId (id, Success) env = (Nothing, [toDyn Success])
handlePrioNode (PrioNode prioId) forest ownId (id, Running) env | id == prioId = (Just (filteredTree, []), [])
-- ERROR: the node has already transformed when we reach here; analyze
  where
    filteredTree = case (filter (isSameNodeId id) forest) of
      [t] -> t
      other -> error $ "Not just one matching running prio??? " ++ id ++ " doesn't match " ++ (show other) ++ " => " ++ (show forest)  
handlePrioNode p forest ownId (id, _) env = (Just (Node (GenNode ownId [] p) forest, []), [])

{-----------------------------------}

-- THE FSM NODE TRANSFORMS ITSELF INTO ANOTHER STATE
-- FSM[Running] -> RunAwayBehaviour
--   v
-- FSM[Attacking] -> AttackBehaviour, ApproachBehaviour
type (Transition state event) = event -> NodeId -> FsmNode event state -> [RunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])

data FsmNode event state = FsmNode state (Transition state event)

runNode :: forall n et. (Show n, BNode n et) => NodeId -> n -> [SpawnRunTree] -> SpawnRunTree
-- runNode n spawnList env = (Nothing, [])
runNode id n spawnList env = (Just (runTree, concat subBehsList), subEvtList)
  where
    (subMaybeTreeAndBehsList, subEvtListList) = unzip $ map ($ env) spawnList
    subEvtList = concat subEvtListList
    (subTreeList, subBehsList) = unzip $ catMaybes subMaybeTreeAndBehsList
    runTree = Node (GenNode id [] $ n) subTreeList

runFsmNode :: (Show state, Typeable event) => NodeId -> FsmNode event state -> [SpawnRunTree] -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
runFsmNode id fsm spawnList env = (Just (runTree, concat subBehsList), subEvtList)
  where
    (subMaybeTreeAndBehsList, subEvtListList) = unzip $ map ($ env) spawnList
    subEvtList = concat subEvtListList
    (subTreeList, subBehsList) = unzip $ catMaybes subMaybeTreeAndBehsList
    runTree = Node (GenNode id [] $ fsm) subTreeList
instance (Show state) => Show (FsmNode event state) where
    show (FsmNode state _) = "FSM" ++ show state
instance (Typeable event) => BNode (FsmNode event state) event where
    handle = handleFsmNode
-- handle :: n -> [RunTree] -> (NodeId, et) -> SpawnRunTree
-- type SpawnRunTree = (InternalMem, BlackBoard) -> RunResult
handleFsmNode :: FsmNode event state -> [RunTree] -> NodeId -> (NodeId, event) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handleFsmNode fsm@(FsmNode state transition) treeList ownId (id, evt) env = transition evt ownId fsm treeList env

{-----------------------------------}

-- [Atomic] Behaviour node
data BehaviourNode = BehaviourNode String Behaviour
runBehNode :: NodeId -> BehaviourNode -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
runBehNode id behnode@(BehaviourNode desc beh) env = (Just (runTree, [beh]), [toDyn Running])
  where
    runTree = Node (GenNode id [] $ behnode) []
    
instance Show BehaviourNode where
    show (BehaviourNode desc _) = "BEHAVIOUR[" ++ desc ++ "]"
instance BNode BehaviourNode Success where
    handle = handleBehNode

handleBehNode :: BehaviourNode -> [RunTree] -> NodeId -> (NodeId, Success) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handleBehNode (BehaviourNode desc beh) [] ownId (id2, Failure) env = (Nothing, [toDyn Failure])
handleBehNode (BehaviourNode desc beh) [] ownId (id2, Success) env = (Nothing, [toDyn Success])

-- FIXME or delete?
-- handleBehNode bnode [] _ _ = (Just (bnode, []), [])

{-----------------------------------}

-- PlannerNode
-- TODO use more expressive typing
type Constraints = String
data PlannerNode = PlannerNode Constraints 

-- TODO Redo the whole Behaviour stuff! A plan is a sort of behaviour that does
-- not depend directly on the input state. The input state also needs to be
-- parameterised, and reactors need to be "outsourced" to C/C++.

runPlannerNode :: NodeId -> PlannerNode -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
-- TODO Obviously there is a behaviour here...
runPlannerNode id plannode@(PlannerNode consts) env = (Just (runTree, []), [])
  where
    runTree = Node (GenNode id [] $ plannode) []
    
instance Show PlannerNode where
    show (PlannerNode consts) = "PLANNER[" ++ consts ++ "]"
instance BNode PlannerNode (Maybe SpawnRunTree) where
    handle = handlePlanNode

handlePlanNode :: PlannerNode -> [RunTree] -> NodeId -> (NodeId, Maybe SpawnRunTree) -> (InternalMem, BlackBoard) -> (Maybe (RunTree, [Behaviour]), [Event])
handlePlanNode (PlannerNode consts) [] ownId (id2, Just spawnTree) env =
  case spawnTree env of
    (Just (Node (GenNode _ [] n) f, behs), evtList) ->
      (Just (Node (GenNode ownId [] n) f, behs), evtList)
    other -> other 
handlePlanNode (PlannerNode consts) [] ownId (id2, Nothing) _ =
  (Nothing, [toDyn Failure])


-- FIXME or delete?
-- handleBehNode bnode [] _ _ = (Just (bnode, []), [])


{-----------------------------------} 

-- type RunTree = Tree GenNode -- Node GenNode [RunTree]
-- data GenNode = forall n et. (Show n, BNode n et) => GenNode NodeId [NodeId] n

d (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

runMoveToCloserNode :: NodeId -> [Pos] -> SpawnRunTree
runMoveToCloserNode id posList env@(mem, bb) | length posList > 0
  = runBehNode id (BehaviourNode "moving" (moveBehaviour closest)) env
  where
    BlackBoard _ _ _ curPos _ _ _ = bb
    closeList = [(p, d p curPos) | p <- posList]
    (closest, _) = foldl1' (\tmin@(pmin,dmin) t@(p,d) -> if d<dmin then t else tmin) closeList

runFromCloserSeqNode id posList env@(mem, bb) | length posList > 0
  = runSeqNode id spawnList env
  where
    BlackBoard _ _ _ curPos _ _ _ = bb
    -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    pointSubListDistList = unfoldr (\x ->
      case x of
        [] -> Nothing;
        l@(p:xs) -> Just ((p,l,d p curPos),xs)) posList
    (closest, closestSubList, _) = foldl1' (\tmin@(pmin,smin,dmin) t@(p,s,d) -> if d<dmin then t else tmin) pointSubListDistList
    -- TODO replace with a single foldl!
    spawnList = moveSpawnList closestSubList

moveSpawnList posList = moveBehavioursList
  where
    moveId = (++) "move-" . show
    moveBehavioursList = map (\x -> runBehNode (moveId x) ( 
      BehaviourNode (moveId x) (moveBehaviour x) ) ) posList

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

--------------------------------

