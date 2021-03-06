module PetriNet.Bisimilarity -- (
  -- isBisim,
  -- isMBisim) 
       where

import PetriNet.Net
import PetriNet.PTNet
import PetriNet.Util.NodeMap
import DFSM

import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F

import Data.Graph.Inductive hiding (NodeMap)
import Data.Tree

-- | Strong bisimulation
isBisim :: Eq l => (PTNet, Labelling l) -> (PTNet, Labelling l) -> (PTMark, PTMark) -> Bool
isBisim (pn1,l1) (pn2,l2) (m1,m2) = isJust $ runStateT (bisim (pn1,l1) (pn2,l2) (m1,m2)) S.empty

bisim :: Eq l =>
         (PTNet, Labelling l) -> (PTNet, Labelling l) ->
         (PTMark, PTMark) -> StateT (Set (PTMark,PTMark)) Maybe Bool
bisim (pt1,l1) (pt2,l2) (m1,m2) = do
  r <- get
  if S.member (m1,m2) r
    then return True
    else let ts1 = F.foldMap (\t -> guard (enabled pt1 m1 t) >> return t) $ trans pt1
             ts2 = F.foldMap (\t -> guard (enabled pt2 m2 t) >> return t) $ trans pt2
             (ts1',silentTs1) = partition (isJust . l1) ts1
             (ts2',silentTs2) = partition (isJust . l2) ts2
         in do
           put $ S.insert (m1,m2) r
           mapM_ (\t1 -> msum $ map (existBsim l1 l2 t1) ts2') ts1'
           mapM_ (\t1 -> msum $ map (\t2 -> existBsim l2 l1 t2 t1) ts1') ts2'
           return True
  where
    existBsim l1 l2 t1 t2 = do
      guard (l1 t1 == l2 t2)
      bisim (pt1,l1) (pt2,l2) (fire pt1 m1 t1, fire pt2 m2 t2)

groupByLabel :: (Eq l) => Labelling l -> [Trans] -> [[Trans]]
groupByLabel _ []     = []
groupByLabel l (t:ts) = ts1:groupByLabel l ts
  where (ts1,ts2) = partition ((== l t) . l) ts

findPath :: Eq l => (SS, Labelling l) -> PTNet -> NodeMap PTMark -> Maybe l -> PTMark -> [PTMark]
findPath (ss,ll) pt nm l from = start ++
  growPath (ss,ll) nm (mapMaybe id . concatMap flatten $ findPath' (ss,ll) nm l from) 
  where start | isNothing l = [from]
              | otherwise   = []
                              
nodeFromLab :: NodeMap PTMark -> PTMark -> Node
nodeFromLab nm m = case lookupNode nm m of
  Just (n,_) -> n
  Nothing    -> error "Marking not reachable"

findPath' :: Eq l => (SS, Labelling l) -> NodeMap PTMark 
             -> Maybe l -> PTMark -> [Tree (Maybe PTMark)]
findPath' (ss,ll) nm l from = xdffWithP (nextNode ll l) (\n -> fmap lab' . isfin ll l ss n) [nodeFromLab nm from] ss

isfin :: Eq l => Labelling l -> Maybe l -> SS -> Node -> Context PTMark PTTrans -> Maybe (Context PTMark PTTrans)
isfin lab l ss n ctx = case any (\(toNode,t) -> toNode==node' ctx && lab t==l) (lsuc ss n) of
  False -> Nothing
  True  -> Just ctx

leaves :: Tree t -> [t]
leaves (Node x []) = [x]
leaves (Node _ ts) = concatMap leaves ts

context4l' :: Context a b -> Adj b 
context4l' (p,v,_,s) = s++filter ((==v).snd) p

nextNode :: Eq l => Labelling l -> Maybe l -> CFun PTMark PTTrans [Node]
nextNode lab l =
  map snd . filter (\(x,_) -> lab x == l || isNothing (lab x)) . context4l'

-- Nodes that can be reached via silent transitions
growPath :: Eq l => (SS, Labelling l) -> NodeMap PTMark
            -> [PTMark] -> [PTMark]
growPath (ss,ll) nm ms = ms ++
  concatMap (mapMaybe id . concatMap leaves . findPath' (ss,ll) nm Nothing) ms 

-- | Whether two Petri Net are m-bisimilar
isMBisim :: (Show l, Eq l) =>
            (PTNet, Labelling l) -> (PTNet, Labelling l) -> Maybe (Bool, Set (PTMark, PTMark))
isMBisim (pt1,l1) (pt2,l2) =
  runReaderT 
  (runStateT (mBisim (pt1,l1) (pt2,l2) (initial pt1, initial pt2)) S.empty)
  (swap . reachabilityGraph' $ pt1, swap . reachabilityGraph' $ pt2)
  where  swap (a,b) = (b,a)
         
type SM = (SS, NodeMap PTMark)

mBisim :: (Show l,Eq l) =>
          (PTNet, Labelling l) -> (PTNet, Labelling l) ->
          (PTMark, PTMark) -> StateT (Set (PTMark,PTMark)) (ReaderT (SM,SM) Maybe) Bool
mBisim (pt1,l1) (pt2,l2) (m1,m2) = do
  r <- get
  if S.member (m1,m2) r
    then return True
    else let ts1 = F.foldMap (\t -> guard (enabled pt1 m1 t) >> return t) $ trans pt1
             ts2 = F.foldMap (\t -> guard (enabled pt2 m2 t) >> return t) $ trans pt2
         in do
           put $ S.insert (m1,m2) r
           mapM_ sim1 ts1
           mapM_ sim2 ts2
           return True
  where
    sim1 t1 = do
      let l = l1 t1
          m1' = fire pt1 m1 t1
      ((ss1,nm1),(ss2,nm2)) <- ask
      let nodes = growPath (ss2,l2) nm2 $ findPath (ss2,l2) pt2 nm2 l m2
      guard (not (null nodes)) -- there exist a path ==> m2'
      mapM_ (\m2' -> mBisim (pt1,l1) (pt2,l2) (m1',m2')) nodes -- all of them are m-bisimilar
    sim2 t2 = do
      let l = l2 t2
          m2' = fire pt2 m2 t2
      ((ss1,nm1),(ss2,nm2)) <- ask
      let nodes = growPath (ss1,l1) nm1 $ findPath (ss1,l1) pt1 nm1 l m1
      guard (not (null nodes)) -- there exist a path ==> m1'
      mapM_ (\m1' -> mBisim (pt1,l1) (pt2,l2) (m1',m2')) nodes -- all of them are m-bisimilar
      
