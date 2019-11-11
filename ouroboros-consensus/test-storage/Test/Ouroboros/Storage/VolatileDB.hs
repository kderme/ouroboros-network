{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Ouroboros.Storage.VolatileDB (tests) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Util.ResourceRegistry
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Test.Ouroboros.Storage.Util
import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.VolatileDB.TestBlock

tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
  [ testProperty "Invalid argument" prop_VolatileInvalidArg
  , StateMachine.tests
  ]

prop_VolatileInvalidArg :: HasCallStack => Property
prop_VolatileInvalidArg = monadicIO $ do
    let fExpected = \case
            Left (UserError (InvalidArgumentsError _str)) -> return ()
            somethingElse -> fail $ "IO returned " <> show somethingElse <> " instead of InvalidArgumentsError"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> withRegistry $ \registry -> do
            _ <- Internal.openDBFull hasFS err (EH.throwCantCatch EH.monadCatch) (myParser hasFS) 0 registry
            return ()
        )

prop_VolatileReOpen :: HasCallStack => Property
prop_VolatileReOpen = monadicIO $ do
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= ()))  (\hasFS err -> withRegistry $ \registry -> do
            _ <- Internal.openDBFull hasFS err (EH.throwCantCatch EH.monadCatch) (myParser hasFS) 0 registry
--            putBlock 
            return ()
        )
