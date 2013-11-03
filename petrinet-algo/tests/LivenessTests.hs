module LivenessTests where

import           Control.Monad
import qualified Data.MultiSet     as MSet
import           Test.HUnit        as H

import           PetriNet.Liveness
import           PetriNet.PTConstr
import           PetriNet.PTNet

-----------------------------------------------------------------------
livenessTests :: Test
livenessTests = H.TestList
                [ H.TestLabel "Liveness test 1" (H.TestCase testLive1)
                , H.TestLabel "Liveness test 2" (H.TestCase testLive2)
                ]
-----------------------------------------------------------------------

testLive1 = H.assertBool "twoProcNet1 should be live" (isLive ss1 twoProcNet1)
  where ss1 = reachabilityGraph twoProcNet1

testLive2 = H.assertBool "twoProcNet2 should NOT be live" (not (isLive ss2 twoProcNet2))
  where ss2 = reachabilityGraph twoProcNet2

run' :: PTConstrM l a -> (a, PTNet)
run' = flip run new

((a,b,idle),twoProcNet') = run' $ do
  let pA = Trans "PickA"
      pB = Trans "PickB"
      pAB = Trans "PickAB"
      pBA = Trans "PickBA"
  [a,b,waitA,waitB,idle] <- replicateM 5 mkPlace

  arc a    pA
  arc idle pA
  arc pA waitB
  arc waitB pAB
  arc b     pAB
  arc pAB   b
  arc pAB   a
  arc pAB   idle

  arc b    pB
  arc idle pB
  arc pB   waitA
  arc waitA pBA
  arc a     pBA
  arc pBA   a
  arc pBA   idle
  arc pBA   b

  return (a,b,idle)

twoProcNet1 = twoProcNet' { initial = MSet.fromList [a,b,idle] }
twoProcNet2 = twoProcNet' { initial = MSet.fromList [a,b,idle,idle] }
