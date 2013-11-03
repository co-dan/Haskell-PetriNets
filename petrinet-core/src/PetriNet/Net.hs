{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- | Generalized nets
module PetriNet.Net
    ( -- * Generalized nets
      Net(..)
    , preP
    , postP
      -- * Dynamic view of nets
    , DynNet(..)
    , fireSequence_, fireSequence
    , reachabilityGraph, reachabilityGraph'
    ) where

import           Data.Functor.Compose
import           Data.Graph.Inductive  (Gr)
import qualified Data.Graph.Inductive  as G
import           Data.Set              (Set)
import qualified Data.Set              as Set
--import Data.Graph.Inductive.NodeMap
import qualified Data.Foldable         as F
import           PetriNet.Util.NodeMap

-- | A generalized net datatype
data Net p t n m = Net
     { places  :: Set p -- ^ A set of places
     , trans   :: Set t -- ^ A set of transitions
     , pre     :: t -> n p -- ^ The pre function
     , post    :: t -> n p -- ^ The post function
     , initial :: m p -- ^ Initial marking
     }

-- | Post-transitions of a place
postP :: (Eq p, F.Foldable n) => p -> Net p t n m -> [t]
postP p (Net {trans=trans, pre=preT, post=postT}) =
  F.foldMap (\x -> if F.any (== p) (preT x) then [x] else []) trans

-- | Pre-transitions of a place
preP :: (Eq p, F.Foldable n) => p -> Net p t n m -> [t]
preP p (Net {trans=trans, pre=preT, post=postT}) =
  F.foldMap (\x -> if F.any (== p) (postT x) then [x] else []) trans

-- | Annotate net using additional functors of place/transition relationship
annotate :: (Functor a, Functor n) =>
              Net p t n m -> (p -> t -> a p) -> (t -> p -> a p) -> Net p t (Compose n a) m
annotate n f g = n { pre = \t -> Compose $ fmap (`f` t) (pre n t)
                   , post = \t -> Compose $ fmap (g t) (post n t) }


-- | A general dynamic net interface
class DynNet net p t m | net -> p, net -> t, net -> m where
  -- | Whether a set of transitions is enabled.
  -- Note that this is different then checking whether each
  -- transition in the set is enabled
  enabledS :: net -> m p -> Set t -> Bool

  -- | Whether some transition is enabled
  enabled :: net -> m p -> t -> Bool
  enabled n mark tr = enabledS n mark (Set.singleton tr)

  -- | The marking after some transitions is fired
  fire    :: net -> m p -> t -> m p


fireSequence_ :: (DynNet net p t m) => net -> m p -> [t] -> m p
fireSequence_ n = foldl (fire n)

fireSequence :: (DynNet net p t m) => net -> m p -> [t] -> Maybe (m p)
fireSequence n = F.foldlM (fire' n)
  where fire' n m t = if enabled n m t then Just (fire n m t) else Nothing

-- | The reachability graph of a Petri Net
reachabilityGraph :: (DynNet (Net p t n m) p t m, Ord p, Ord (m p))
                  => Net p t n m -> Gr (m p) t
reachabilityGraph = snd. reachabilityGraph'

-- | The reachability graph of a Petri Net together with a 'NodeMap'
reachabilityGraph' :: (DynNet (Net p t n m) p t m, Ord p, Ord (m p))
                   => Net p t n m -> (NodeMap (m p), Gr (m p) t)
reachabilityGraph' net = snd $ run G.empty $
                        insMapNodeM (initial net) >> go (Set.singleton (initial net))
  where go !work | Set.null work = return ()
                 | otherwise     = do
          let m = (head . Set.toList) work
                  --- Better way to pick an arbitrary M from the set Work?
              work' = Set.delete m work
          work'' <- F.foldrM (act net m) work' (trans net)
          go $! work''

act :: (G.DynGraph g, DynNet net p t m, Ord p, Ord (m p)) =>
       net -> m p -> t -> Set (m p) -> NodeMapM (m p) t g (Set (m p))
act net m !t !w =
  if enabled net m t
  then do
     let m' = fire net m t
     present <- lookupNodeM m'
     w' <- case present of
                Just _ -> return w
                Nothing -> do
                  insMapNodeM m'
                  return (Set.insert m' w)
     insMapEdgeM (m,m',t)
     return $! w'
  else return w

