{-# LANGUAGE NamedFieldPuns #-}

module Test.Dynamic.PBFT (
    tests
  ) where

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Magic

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple PBFT convergence" $
        prop_simple_pbft_convergence k
    ]
  where
    k = defaultSecurityParam

prop_simple_pbft_convergence :: SecurityParam
                             -> TestConfig
                             -> Seed
                             -> Property
prop_simple_pbft_convergence
  k testConfig@TestConfig{numCoreNodes, numSlots} seed =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k (fromIntegral nn) sigThd (NetworkMagic 0xa5a5a5a5)

    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockPBFT params))
            testConfig seed
