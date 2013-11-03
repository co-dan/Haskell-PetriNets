module CtlTests where

import           PetriNet.CTL
import           PetriNet.Net
import           PetriNet.PTConstr
import           PetriNet.PTNet

import qualified Data.MultiSet        as MSet
import qualified Data.Set             as Set
import qualified Test.HUnit           as H

------------------------------------------------------------
ctlTests :: H.Test
ctlTests = H.TestList [ H.TestLabel "CTL test 1" ctltest1 
                      , H.TestLabel "CTL test 2" ctltest2
                      , H.TestLabel "CTL test 3" ctltest3 ]
------------------------------------------------------------


atom :: [Int] -> CTL
atom a = CTLAtom (show (MSet.fromList a),
                       (==MSet.fromList a))

pn1 :: PTNet
pn1 = Net { places = Set.fromList [1,2,3,4]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> MSet.fromList [1,2]
               "t2"  -> MSet.fromList [1]
          , post   = \(Trans x) -> case x of
               "t1" -> MSet.fromList [3,4]
               "t2"  -> MSet.fromList [2]
          , initial = MSet.fromList [1,1,2,2]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"
  
formula1 :: CTL
formula1 = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))         

ctltest1 :: H.Test
ctltest1 = H.TestCase $ H.assertBool "formula1 should hold for pn1" (fst $ verifyPT pn1 formula1) 

pn2 :: PTNet         
pn2 = Net { places = Set.fromList [1,2]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> MSet.fromList [1]
               "t2" -> MSet.fromList [2]
          , post   = \(Trans x) -> case x of
               "t1" -> MSet.fromList [2]
               "t2" -> MSet.fromList [1]
          , initial = MSet.fromList [1]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"

formula2 :: CTL
formula2 = CTLNot (ef (atom [1,2]))

ctltest2 :: H.Test
ctltest2 = H.TestCase $ H.assertBool "formula2 should hold for pn1" (fst $ verifyPT pn2 formula2) 

data L = A | B | C deriving (Show,Eq,Ord)

circ = do
  p2 <- mkPlace
  p3 <- mkPlace
  conn p2 p3 B
  conn p3 p2 A
  return (p2,p3)

(_,pn7',l7) = flip runL new $ do
  p1 <- mkPlace
  (p2,_) <- circ
  conn p1 p2 A
  return ()

pn7 = pn7' { initial = MSet.fromList [1] }

formula3 :: CTL
formula3 = ag $ (atom [2]) ==> (ef (atom [3]))

ctltest3 :: H.Test
ctltest3 = H.TestCase $ H.assertBool "formula3 should hold for pn7" (fst $ verifyPT pn7 formula3) 


