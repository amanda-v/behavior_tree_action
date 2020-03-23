-- TODO set exports!

module ActionFw.BehaviourLib where

-- Action examples...
-- Take object
-- MoveTo position
-- MoveTo position silently
-- ShootOn weapon subject
-- Throw item direction
-- Open door
-- HideIn hide
-- UseOn item place
-- Throw object
-- WaitFor someone time
-- Talk subject
-- Follow subject
-- Something that is runnable? That has an actionspec?

import ActionFw 
import ActionFw.DataStore
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack)

-- shootExternal :: () -> IO ()
-- shootExternal = do
--     putStrLn ("moving... " ++ show pos)

-- Do sthg like "runExtNode $ ExtNode $ moveExternal $ (2,3)"
-- let moveTo1 = extNode $ moveExternal (1,2)
-- let moveTo2 = extNode $ moveExternal (3,4)
-- let moveTo3 = extNode $ moveExternal (5,6)
-- let listNode = seqNode [seqNode [moveTo1, moveTo2], moveTo3]

-- 2-step actions
-- 1st, select which behaviours are active
-- 2nd, run active behaviours to get atomic actions
-- 3rd, resolve action priorities

-- functions that yield next state and function; encapsulate in state monad?
-- behaviours are primitive like move to X or shoot (actually like states)
-- actions are things like moves, shoots or similar

-- preconds broken? if so, advance the state of the corresponding generator

haveBullets :: Precond
haveBullets (BlackBoard _ _ b _ _ _ _) _
    | b > 0 = True 
    | other = False

enemiesAround :: Precond
enemiesAround (BlackBoard [] _ _ _ _ _ _) _ = False
enemiesAround (BlackBoard other _ _ _ _ _ _) _ = True

shootPre :: [Precond]
shootPre = [haveBullets, enemiesAround]

shootPost :: Postcond
shootPost (BlackBoard ags g b p o s l) =
    BlackBoard ags g (b-1) p o s l

-- shoot :: ActionSpec
-- shoot = ActionSpec shootPre shootPost

shootFun :: Updater
shootFun (BlackBoard [] _ b _ _ _ _) mem =
    Action (Nothing) (Nothing) 
shootFun (BlackBoard ((agStr,_):ags) _ b _ _ _ _) mem
    | b > 0 = Action (Nothing) (Just $ Shoot agStr 0.0)
    | other = Action (Nothing) (Nothing)

shootBehaviour :: Behaviour
shootBehaviour = Behaviour ("agent", "shoot") 100 (\bb mem -> not $ enemiesAround bb mem) (UpdaterFun shootFun) shootPre shootPost 

-----------------

movePre :: [Precond]
movePre = []

movePost :: Pos -> Postcond
movePost newPos (BlackBoard ags g b _ o s l) =
    BlackBoard ags g b newPos o s l

-- move :: Pos -> ActionSpec
-- move newPos = ActionSpec movePre (movePost newPos)

-- This should be a *steering* behaviour
-- Think about actions/reactors in this way!
moveFun :: Updater
moveFun (BlackBoard _ _ _ p _ _ _) mem =
    Action (Just $ Move p) (Nothing) 

moveBehaviour :: Pos -> Behaviour
moveBehaviour pos = Behaviour ("agent", "move-" ++ show pos) 20 (const $ const True) (UpdaterFun moveFun) movePre (movePost pos) 

-----------------
-- TODO: instantiated behaviours: how to instantiate a new behaviour
-- with a parameter provided by a previous node
-- parameterised types!!!
-- Let's keep it flexible: nodes can output a modified hashmap and nodes can take an optional parameter
-----------------

-- Record new foes

-- Again, leave for the moment...
{--
recordFoePre :: [Precond]
recordFoePre = []

recordFoePost :: AgentStr -> Postcond
movePost = 

recordFoeFun :: Perceptor
recordFoeFun bb@(BlackBoard agList _ _ _ _ _ _) mem 
-- FIXME Use encapsulation!!!
    | null (intersect agList currentList) = Map.adjust (\x -> agList) "foes" mem
    | other = mem
    where currentList = getFoes $ Map.lookup "foes"
          getFoes Nothing = []
          getFoes Just x = x

recordFoeBehaviour :: Behaviour
recordFoeBehaviour = Behaviour ("agent", "recordFoe") 20 (Perceptor recordFoeFun) recordFoePre recordFoePost 
--}

------------------------------
