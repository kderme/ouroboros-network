{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Shelley
  ()
where

import           BaseTypes                      ( Nonce )
import           BlockChain                     ( Block(..)
                                                , BHeader(..)
                                                , BHBody(..)
                                                , HashHeader
                                                , bheader
                                                , bhHash
                                                , bhbody
                                                )
import           Slot                           ( BlockNo(..) )
import           Cardano.Binary                 ( ToCBOR(..)
                                                , FromCBOR(..)
                                                )
import           Cardano.Prelude                ( NoUnexpectedThunks(..) )
import           Control.State.Transition
import           Data.Coerce                    ( coerce )
import           Data.FingerTree.Strict         ( Measured(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           GHC.Generics                   ( Generic )
import           LedgerState                    ( DState(..)
                                                , DPState(..)
                                                , NewEpochState(..)
                                                , LedgerState(..)
                                                , esPp
                                                )
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block
import qualified STS.Bhead                     as STS
import qualified STS.Bbody                     as STS

{-------------------------------------------------------------------------------
  Crypto Aliases
-------------------------------------------------------------------------------}

type DSIGN = TPraosDSIGN TPraosStandardCrypto
type KES = TPraosKES TPraosStandardCrypto
type VRF = TPraosVRF TPraosStandardCrypto
-- Capitalised for consistency and to avoid conflict with other `Hash` types.
type HASH = TPraosHash TPraosStandardCrypto

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash { unShelleyHash :: HashHeader HASH DSIGN KES VRF }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR) -- TODO , FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ShelleyBlock cfg = ShelleyBlock
  { unShelleyBlock :: Block HASH DSIGN KES VRF
  }
  deriving (Eq, Show)

instance GetHeader (ShelleyBlock cfg) where
  data Header (ShelleyBlock cfg) = ShelleyHeader
    { shelleyHeader :: !(BHeader HASH DSIGN KES VRF)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance Typeable cfg => NoUnexpectedThunks (Header (ShelleyBlock cfg)) where
  showTypeOf _ = show $ typeRep (Proxy @(Header (ShelleyBlock cfg)))

-- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ShelleyHeader hdr _hash) =
    noUnexpectedThunks ctxt hdr

instance (Typeable cfg, NoUnexpectedThunks cfg)
  => SupportedBlock (ShelleyBlock cfg)

type instance HeaderHash (ShelleyBlock cfg) = ShelleyHash

instance (Typeable cfg) => HasHeader (ShelleyBlock cfg) where
  blockHash      = blockHash . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      = blockSlot . getHeader
  blockNo        = blockNo . getHeader
  blockInvariant = const True

instance (Typeable cfg) => HasHeader (Header (ShelleyBlock cfg)) where
  blockHash = shelleyHeaderHash

  blockPrevHash =
    BlockHash . ShelleyHash . bheaderPrev . bhbody . shelleyHeader

  blockSlot      = convertSlotNo . bheaderSlot . bhbody . shelleyHeader
  blockNo        = coerce . bheaderBlockNo . bhbody . shelleyHeader
  blockInvariant = const True

instance (Typeable cfg) => Measured BlockMeasure (ShelleyBlock cfg) where
  measure = blockMeasure

instance StandardHash (ShelleyBlock cfg)

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

-- | The Shelley ledger state is updated by two rules - BHEAD and BBODY. In the
-- executable spec, these states are combined in the `CHAIN` rule. However, we
-- do not wish to use that rule since it also calls `PRTCL`, which is the
-- chain-level processing rule called via the Transitional Praos implementation.
-- Consequently, we defined the combined state here.
data CombinedLedgerState = CombinedLedgerState
  { combinedLedgerStateBHEAD :: STS.State (STS.BHEAD HASH DSIGN KES VRF)
  , combinedLedgerStateBBODY :: STS.State (STS.BBODY HASH DSIGN KES VRF)
  } deriving (Eq, Show)

-- | See note on @CombinedLedgerState@
data CombinedLedgerError =
    BHeadError (PredicateFailure (STS.BHEAD HASH DSIGN KES VRF))
  | BBodyError (PredicateFailure (STS.BBODY HASH DSIGN KES VRF))
  deriving (Eq, Show)

instance UpdateLedger (ShelleyBlock cfg) where

  newtype LedgerState (ShelleyBlock cfg) = ShelleyLedgerState
    { shelleyLedgerState :: CombinedLedgerState
    } deriving (Eq, Show, Generic)
  type LedgerError (ShelleyBlock cfg) = CombinedLedgerError

  -- TODO what config is needed for Shelley?
  newtype LedgerConfig (ShelleyBlock cfg) = ShelleyLedgerConfig cfg

-- TODO extract the needed node config
  ledgerConfigView EncNodeConfig{} = undefined

  applyChainTick (ShelleyLedgerConfig _) slotNo (ShelleyLedgerState (CombinedLedgerState bhState _))
    = applySTS @STS.BHEAD $ TRC (env, bhState, slotNo)

{-------------------------------------------------------------------------------
  Support for Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ShelleyBlock cfg)
  = ExtNodeConfig cfg (TPraos TPraosStandardCrypto)

instance SignedHeader (Header (ShelleyBlock cfg)) where
  type Signed (Header (ShelleyBlock cfg)) = BHBody HASH DSIGN KES VRF
  headerSigned = bhbody . shelleyHeader

instance Typeable cfg
  => HeaderSupportsTPraos TPraosStandardCrypto (Header (ShelleyBlock cfg)) where

  headerToBHeader _ (ShelleyHeader hdr _hash) = hdr

instance ProtocolLedgerView (ShelleyBlock cfg) where
  protocolLedgerView _nc (ShelleyLedgerState ls) = TPraosLedgerView
    { tpraosLedgerViewPoolDistr       = nesPd hs
    , tpraosLedgerViewProtParams      = esPp . nesEs $ hs
    , tpraosLedgerViewDelegationMap   = _genDelegs . _dstate . _delegationState $ lds
    , tpraosLedgerViewEpochNonce      = nesEta0 hs
    , tpraosLedgerViewOverlaySchedule = nesOsched hs
    }
   where
    hs = combinedLedgerStateBHEAD ls
    (STS.BbodyState lds _) = combinedLedgerStateBBODY ls

  -- TODO Implement this!
  anachronisticProtocolLedgerView = undefined
