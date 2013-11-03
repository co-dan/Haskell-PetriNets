{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- | P/T-nets aka general Petri nets aka flat Petri nets
module PetriNet.PTNet
    ( module PetriNet.Net
      -- * P/T types
    , PTNet
    , PTTrans, PTPlace, PTMark, Trans(..)
    , Labelling
      -- Other stuff
    , LLArc, HLArc(..), SS
    ) where
 
import Prelude
import Data.Set (Set)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Monoid
import Data.Functor.Identity
import Data.Graph.Inductive (Gr)
import PetriNet.Net
import qualified Data.Foldable as F

newtype Trans = Trans { name :: String }
           deriving (Eq,Ord)
                      
instance Show Trans where
  show = name
      
newtype HLArc a p = Arc { unArc :: MultiSet (a p) }
type LLArc   = HLArc Identity
type PTNet   = Net PTPlace Trans MultiSet MultiSet
type PTMark  = MultiSet PTPlace
type PTTrans = Trans
type PTPlace = Int
type Labelling l = Trans -> Maybe l 

instance DynNet PTNet PTPlace PTTrans MultiSet where
  enabledS = enabledSPT
  enabled = enabledPT
  fire    = firePT

enabledPT :: PTNet -> PTMark -> PTTrans -> Bool
enabledPT (Net {pre=pre}) marking =
  (`MSet.isSubsetOf` marking)  . pre

enabledSPT :: PTNet -> PTMark -> Set PTTrans -> Bool
enabledSPT (Net {pre=pre}) marking =
  (`MSet.isSubsetOf` marking) . F.foldMap pre

firePT :: PTNet -> PTMark -> PTTrans -> PTMark
firePT (Net {pre=pre, post=post}) mark t =
  (mark MSet.\\ pre t) <> post t
        
-- XXX: reuse types from StateSpace module

type SS = Gr PTMark PTTrans
