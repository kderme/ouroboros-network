{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis (HasAnalysis (..)) where

import           Control.Monad.Except
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           GHC.Natural (Natural)

import           Cardano.Binary (unAnnotated)
import qualified Cardano.Crypto as Crypto
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as Chain

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.ProtocolInfo

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as Shelley

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Node (protocolInfoByron)

import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol.Crypto


{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class HasAnalysis blk where
    countTxOutputs  :: blk -> Maybe Int
    blockHeaderSize :: blk -> Maybe Natural
    blockTxsSize    :: blk -> Maybe [BL.ByteString]
    mkTopLevelConfig :: FilePath -> Bool -> IO (EpochInfo Identity, TopLevelConfig blk)

{-------------------------------------------------------------------------------
  ByronBlock instance
-------------------------------------------------------------------------------}

instance HasAnalysis Byron.ByronBlock where
    countTxOutputs = whenNotEBB countTxOutputsByron
    blockHeaderSize = whenNotEBB blockHeaderSizeByron
    blockTxsSize = whenNotEBB blockTxsSizeByron
    mkTopLevelConfig configFile onMainNet = do
      genesisConfig <- openGenesisByron configFile onMainNet
      let epochSlots = Genesis.configEpochSlots genesisConfig
          epochInfo = fixedSizeEpochInfo (coerce epochSlots)
      let config = mkByronTopLevelConfig genesisConfig
      return (epochInfo, config)

-- | Returns 'Nothing' for EBBs and a value for regular blocks.
whenNotEBB :: (Chain.ABlock ByteString -> a) -> Byron.ByronBlock ->  Maybe a
whenNotEBB k blk = case blk of
    Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _
      -> Just $ k regularBlk
    _ -> Nothing

countTxOutputsByron :: Chain.ABlock ByteString -> Int
countTxOutputsByron Chain.ABlock{..} = countTxPayload bodyTxPayload
  where
    Chain.AHeader{..} = blockHeader
    Chain.ABody{..}   = blockBody

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

blockHeaderSizeByron ::  Chain.ABlock ByteString -> Natural
blockHeaderSizeByron = Chain.headerLength . Chain.blockHeader

blockTxsSizeByron :: Chain.ABlock ByteString -> [BL.ByteString]
blockTxsSizeByron block = map (BL.fromStrict . Chain.aTaAnnotation) blockTxAuxs
  where
    Chain.ABlock{ blockBody } = block
    Chain.ABody{ bodyTxPayload } = blockBody
    Chain.ATxPayload{ aUnTxPayload = blockTxAuxs } = bodyTxPayload

openGenesisByron :: FilePath -> Bool -> IO Genesis.Config
openGenesisByron configFile onMainNet = do
    Right genesisHash <- runExceptT $
      snd <$> Genesis.readGenesisData configFile
    Right genesisConfig <- runExceptT $
      Genesis.mkConfigFromFile
        (if onMainNet -- transactions on testnet include magic number
          then Crypto.RequiresNoMagic
          else Crypto.RequiresMagic)
        configFile
        (Genesis.unGenesisHash genesisHash)
    return genesisConfig

mkByronTopLevelConfig :: Genesis.Config -> TopLevelConfig Byron.ByronBlock
mkByronTopLevelConfig genesisConfig = pInfoConfig $
    protocolInfoByron
      genesisConfig
      Nothing
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyse") 2)
      Nothing

{-------------------------------------------------------------------------------
  ShelleyBlock instance
-------------------------------------------------------------------------------}

instance HasAnalysis (Shelley.ShelleyBlock TPraosStandardCrypto) where
    countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) -> Just $ sum $ fmap countOutputs txs
    blockHeaderSize =
      Just . fromIntegral . SL.bHeaderSize . SL.bheader . Shelley.shelleyBlockRaw
    blockTxsSize blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) -> Just $ toList $ fmap Shelley.txFullBytes txs
    mkTopLevelConfig configFile _onMainNet = do
      Right (genesis :: ShelleyGenesis TPraosStandardCrypto) <-
        Aeson.eitherDecodeFileStrict' configFile
      let config = pInfoConfig $ protocolInfoShelley genesis (SL.ProtVer 0 0) Nothing -- appVersion genesis)
      let epochInfo = fixedSizeEpochInfo $ sgEpochLength genesis
--      let epochInfo = tpraosEpochInfo tpraosParams
      return (epochInfo, config)
--      ShelleyGenesis TPraosStandardCrypto

countOutputs :: Shelley.Crypto c => Shelley.Tx c -> Int
countOutputs tx  = length $ Shelley._outputs $ Shelley._body tx
