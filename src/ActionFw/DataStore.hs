module ActionFw.DataStore where

import Data.Vector.Storable
import Data.ByteString
import Data.Int
import qualified Data.Map.Lazy as Map
import Foreign.Storable	
import Foreign.Ptr
import Foreign.C.String
import STMContainers.Map
import Data.ByteString

-- We need to make these storable

type AgentStr = ByteString
type AgentCStr = CString

type Pos = (Int64, Int64)

type AgentPos = (AgentStr, Pos)
data CAgentPos = CAgentPos AgentCStr Pos

-- instance Show Agent where
--     show = toList

align :: Int -> Int -> Int
align a i = case a `rem` i of
  0 -> a
  n -> a + (i - n)

instance Storable CAgentPos where
    sizeOf cap = 16
    alignment cap = 8
    peek p = do
        let agentCStrP = castPtr p
        agentCStr <- peek agentCStrP
        let posP = p `plusPtr` (4 + sizeOf agentCStr)
        pos <- peek posP
        return $ CAgentPos agentCStr pos
    poke p cap = do
        return ()

-- TODO ready to store
-- Works for basic types, not for structs
instance (Show a, Show b, Storable a, Storable b) => Storable (a,b) where
	sizeOf (a, b) = sizeOf a + sizeOf b
	alignment (a, b) = alignment a + alignment b
	peek p = do
	    let ap = castPtr p 
	    av <- peek ap
	    let bp = castPtr $ p `plusPtr` align (sizeOf av) (alignment av)
	    bv <- peek bp
	    return (av, bv)
	poke p (av, bv) = do
	    let ap = castPtr p
	    let bp = castPtr $ p `plusPtr` align (sizeOf p) (alignment p)
	    poke ap av
	    poke bp bv
	    return ()

readCAgentPos :: CAgentPos -> IO AgentPos
readCAgentPos (CAgentPos agentCStr pos) = do
	str' <- packCString agentCStr
	return (str', pos)

type Grenades = Int32
type Bullets = Int32
type Orientation = Int64
type SoundLvl = Int64
type LightLvl = Int64

data BlackBoard = BlackBoard [AgentPos] -- List of other seen agents
                             Grenades
                             Bullets
                             Pos -- Own pos
                             Orientation
                             SoundLvl
                             LightLvl
    deriving Show
                             
-- Internal to the agent!
-- FIXME Look for something more useful
-- The issue is how to combine type safety with flexibility.....
-- A container of logic formulae????
-- type InternalMem = Map Integer ByteString
type InternalMem = Map.Map String [String]
emptyMem :: InternalMem
emptyMem = (Map.singleton "foes" [])

