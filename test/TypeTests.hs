{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Main where

import Data.Maybe
import Data.Dynamic

data Success = Success NodeId | Failure NodeId
data Null

type InternalMem = String
type BlackBoard = String
type Behaviour = String
type NodeId = [String]
  

-- refinement: class of things that have an up and then class of things that have a down
-- data Directed up down a = Directed a
-- class Directed n up where
-- Take 2!
-- We need a runtime type system that is compatible with an offline type system :'(

-------------------------------------
class BNode n up down where
    handle :: n -> [GenNode down] -> (InternalMem, BlackBoard) -> [down] -> (Maybe (GenNode up), [up], [Behaviour])
    getId :: n -> NodeId
data GenNode up = forall n down. (Show n, BNode n up down) => GenNode n [GenNode down]  
type SpawnNode up = (InternalMem, BlackBoard) -> (GenNode up, [Behaviour])
-------------------------------------



-- Take a GenNode and take the behs and the up events
-- Great down!!!!! We'd need a type that would represent all the sources in the tree

type RunTree = GenNode Null

recurse :: GenNode up -> (InternalMem, BlackBoard) -> (Maybe (GenNode up), [up], [Behaviour])
recurse (GenNode n downList) env =
  (me, eventList, behList)
    where
      (maybeNodeList, eventListList, behListList) = unzip3 $ map (\gn -> recurse gn env) downList
      (me, ownEvents, ownBehList) = handle n downList env eventList
      nodeList = catMaybes maybeNodeList
      eventList = concat eventListList
      behList = concat behListList
  







-------------------------------------
data SeqNode = SeqNode NodeId [SpawnNode Success]
instance Show SeqNode where
  show (SeqNode id list) = "SEQNODE[" ++ show id ++ "#" ++ show (length list) ++ "]"
instance BNode SeqNode Success Success where
  handle = handleSeqNode
  getId (SeqNode id _) = id

handleSeqNode (SeqNode id []) [] env [Success _] = (Nothing, [Success id], [])
handleSeqNode (SeqNode id _) [] env [Failure _] = (Nothing, [Failure id], [])
handleSeqNode (SeqNode id (spawn:rest)) [] env [Success _] = (Just (GenNode (SeqNode id rest) [child]), [], behs)
  where
    (child, behs) = spawn env
-------------------------------------


-------------------------------------
data BehaviourNode = BehaviourNode NodeId Behaviour
instance Show BehaviourNode where
    show (BehaviourNode id b) = "BEHAVIOUR[" ++ show id ++ "#" ++ b ++ "]"
instance BNode BehaviourNode Success Success where
    handle = handleBehNode
    getId (BehaviourNode id b) = id
    
handleBehNode (BehaviourNode id1 b) [] env [Success id2]
    | id1 == id2 = (Nothing, [Success id2], [])
handleBehNode (BehaviourNode id1 b) [] env [Failure id2]
    | id1 == id2 = (Nothing, [Failure id2], [])
-------------------------------------

-- Depth-first traversal



main :: IO ()
main = putStrLn "something"