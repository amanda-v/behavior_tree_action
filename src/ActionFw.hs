{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, ForeignFunctionInterface #-}

module ActionFw where

import ActionFw.DataStore
import qualified Data.Vector.Storable (fromList, empty, Vector, null)
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.List (foldl')
import Data.Int
import Control.Monad.State.Strict
import Foreign
import Foreign.C.String
import STMContainers.Map (newIO)

-- Preconds allow action activation
-- Postconds specify what happens, when, how probably and for how much time

-- actions can take part in a *plan*, or advance a *script/tree*, or reduce a *drive*
-- aiming makes a directed random walk

-- how can we model conditions? look into planning languages

-- Preconditions are boolean functions over BB items and internal items
type Precond = BlackBoard -> InternalMem -> Bool
-- Postconditions are actions that provide a sequent BB state
type Postcond = BlackBoard -> BlackBoard

-- data ActionSpec = ActionSpec [Precond] Postcond -- List of preconditions and postconditions

-- [a] i i p f f f

other :: Bool
other = True

type BehaviourSelector = BlackBoard -> InternalMem -> [Behaviour]

-- FIXME Verify this works as intended
data Fail = Fail
type Reactor r =  BlackBoard -> InternalMem -> r
type Updater = Reactor Action
-- TODO Is it better a delta or a complete internalmem?
type Perceptor = Reactor InternalMem

-- FIXME So, a reactor *can* fail, but a perceptor does not
-- It should not be uncommon to deploy both a reactor and a perceptor in parallel
-- The reactor should be able to fail with an event
-- similar to the endcondition

data BehaviourFun = UpdaterFun Updater |
                    PerceptorFun Perceptor
type SelectorId = String -- Maybe ByteString?
type BehPerSelId = String -- Maybe ByteString?
type BehaviourId = (SelectorId, BehPerSelId)
data BehaviourState = BUp | BDown
-- FIXME MERGE WITH PRECOND
-- FIXME Need a more *logical*, *declarative* endcondition!!!!
type EndCondition = BlackBoard -> InternalMem -> Bool
data Behaviour = Behaviour BehaviourId -- ID
                           Priority -- prio
                           EndCondition -- when to remove itself from the active behaviour engine and notify the planner
                           BehaviourFun -- actual behaviour
                           [Precond] -- Precondition list
                           Postcond -- Postconditions
instance Show Behaviour where
    show (Behaviour b1 b2 b3 b4 b5 b6) = "BEH - " ++ show b1 ++ " - Prio:" ++ show b2

data MoveAction = Move Pos -- move angle, speed
data ShootAction = Shoot AgentStr Float -- shoot weapon to angle
data Action = Action (Maybe MoveAction) (Maybe ShootAction)

-- TODO Make it storable!!!
data Prioritized a = Prioritized (Priority, a)
type PrioritizedAction = (Prioritized (Maybe MoveAction), Prioritized (Maybe ShootAction))
type Priority = Int

withPriority :: Prioritized a -> Prioritized a -> Prioritized a
withPriority first@(Prioritized (firstP, _)) second@(Prioritized (secondP, _))
    | firstP > secondP = first
    | otherwise = second

-- TODO We are currently assuming that actions are non-conflicting, but
-- this might not always be the case
-- FIXME solve later...
{--
solveConflicts :: [Updater] -> BlackBoard -> InternalMem -> PrioritizedAction
solveConflicts behList bb mem =
    let prioritize :: PrioritizedAction -> PrioritizedAction -> PrioritizedAction
        prioritize (m1, s1) (m2, s2) =
            (withPriority m1 m2, withPriority s1 s2)
        extractAction (Behaviour _ _ prio fun _ _) =
            let Action m s = fun bb mem 
            in (Prioritized (prio, m), Prioritized (prio, s))
        actionList = map extractAction behList
    in foldl' prioritize (Prioritized (0, Nothing), Prioritized (0, Nothing)) actionList 
--}

-- PLANNER ENGINE PROCESS

type PlannerServerState = (InternalMem, [Behaviour])

-- plannerServerRun :: TVar 

-- REACTOR ENGINE PROTOCOL
-- TODO use a fun signature as a protocol, like "ReactorUpdate -> ReactorShutdown -> Bool"
-- or the like
data ReactorMsg = ReactorUpdate [Behaviour] |
                    ReactorRun BlackBoard
data ReactorRsp = ReactorShutdown |
                  ReactorRunOk Action |
                  ReactorUpdateOk |
                  ReactorDebug [String]

-- Internal mem as ST/TVar transactions!!!! This way, one can compose
-- actions and then apply them atomically. Is this safe for the reactive engine???

-- REACTOR ENGINE PROCESS --
 
type ReactorServerState = (InternalMem, [Behaviour])
--FIXME See if a State monad fixed something...

reactorServerRun :: MVar (ReactorMsg, MVar ReactorRsp) -> ReactorServerState -> IO ()
reactorServerRun entry serverState = do
    -- print "Server ready!!!"
    (msg, respVar) <- takeMVar entry
    let (resp, newServerState) = handleReactorMsg msg serverState
    putMVar respVar resp
    reactorServerRun entry newServerState
    
client :: MVar ReactorRsp -> MVar (ReactorMsg, MVar ReactorRsp) -> ReactorMsg -> IO String
client entryResp entry msg = do
    -- print "Inside the client"
    putMVar entry (msg, entryResp)
    resp <- takeMVar entryResp
    return "ping!!!"

-- ENTRY POINT

handleReactorMsg :: ReactorMsg -> ReactorServerState -> (ReactorRsp, ReactorServerState)
handleReactorMsg (ReactorUpdate behList) (mem, _) = 
    (ReactorUpdateOk, (mem, behList))
handleReactorMsg (ReactorRun bb) oldState =
    (ReactorRunOk (Action Nothing Nothing), oldState)

-- TODO Find a suitable "Reactor" type with enough functionality
type CheckAction = Int32 -> Ptr (CAgentPos) -> Int32 -> Int32 -> Ptr Pos -> Int64 -> Int64 -> Int64 -> IO CString

-- We can pass a partial function holding the MVar
-- http://stackoverflow.com/questions/9283528/ffi-haskell-callback-with-state
-- initAFWHs is quite clear
-- Now the problem is: how to marshal the BB->IOStr into a CheckAction
-- pass this on to runAFW

-- FIXME Dropped performance!!! Check why we're now on 35 us!

initAFWHs :: IO (BlackBoard -> IO String)
initAFWHs = do
    reactorEntry <- newEmptyMVar
    -- Entry to the planner is NOT synchronous!
    -- entryPlanner <- newEmptyTVar
    let mem = emptyMem
    reactorServerId <- forkIO (reactorServerRun reactorEntry (mem, []))
    entryResp <- newEmptyMVar
    return (\bb -> do client entryResp reactorEntry $ ReactorRun bb)

foreign export ccall initAFW :: IO (FunPtr (CheckAction))
initAFW = do
    init <- initAFWHs
    actionWithCWrap $ action init

foreign export ccall runAFW :: FunPtr (CheckAction) -> Int32 -> Ptr CAgentPos -> Int32 -> Int32 -> Ptr Pos -> Int64 -> Int64 -> Int64 -> IO CString 
runAFW stub an ap g b pp o s l = do
    -- print "Running the action FW!!!"
    let act = actionWithCUnwrap stub
    act an ap g b pp o s l

action :: (BlackBoard -> IO String) -> Int32 -> Ptr (CAgentPos) -> Int32 -> Int32 -> Ptr Pos -> Int64 -> Int64 -> Int64 -> IO CString
action run an ap g b p o s l = do
    bb <- buildBB an ap g b p o s l
    run bb >>= newCString

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- forM :: Monad m => [a] -> (a -> m b) -> m [b] 
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
buildBB :: Int32 -> Ptr CAgentPos -> Int32 -> Int32 -> Ptr Pos -> Int64 -> Int64 -> Int64 -> IO BlackBoard
buildBB an ap g b pp o s l = do
    p <- peek pp
    alist <- (forM [0..an-1] (peekElemOff ap . fromIntegral)) >>= mapM readCAgentPos
    return $ BlackBoard alist g b p o s l
    
foreign import ccall "wrapper"
    actionWithCWrap :: (CheckAction) -> IO (FunPtr (CheckAction))
foreign import ccall "dynamic"
    actionWithCUnwrap :: FunPtr (CheckAction) -> (CheckAction) 