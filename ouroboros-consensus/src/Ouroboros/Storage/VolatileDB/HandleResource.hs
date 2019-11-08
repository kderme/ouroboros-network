{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module could move outside the VolatileDB if we find it useful
-- elsewhere.
module Ouroboros.Storage.VolatileDB.HandleResource (
    HandleResource (_handle)
  , registeredOpen
  , releaseHandle
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types

-- | A 'Handle' with its associated 'ResourceKey'.
data HandleResource h m = HandleResource {
      _resourceKey :: !(ResourceKey m)
    , _handle      :: !(Handle h)
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | Constructor.
mkHandleResource :: (ResourceKey m, Handle h) -> HandleResource h m
mkHandleResource (key, handle) = HandleResource key handle

-- | Allocates a new open handle in the registry.
registeredOpen :: IOLike m
               => HasFS m h
               -> ResourceRegistry m
               -> FsPath
               -> OpenMode
               -> m (HandleResource h m)
registeredOpen HasFS{..} registry file openMode =
    mkHandleResource
      <$> allocate registry (\_ -> hOpen file openMode) hClose

-- | Releases the resource from the registry, which also closes the handle.
-- If already released, this is a no-op.
releaseHandle :: IOLike m
              => HandleResource h m
              -> m ()
releaseHandle = unsafeRelease . _resourceKey
