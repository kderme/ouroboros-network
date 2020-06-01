{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy as BL (length)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import           Data.Foldable (asum)
import           Data.Functor.Identity
import           Data.IORef
import           Data.List (foldl', intercalate)
import           Data.Proxy (Proxy (..))
import           Data.Word
import           GHC.Int (Int64)
import           GHC.Natural (Natural)
import           Options.Applicative
import           Text.Read (read)

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash,
                     SlotNo (..), genesisPoint)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     StreamFrom (..), StreamTo (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB hiding
                     (withImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
                     (withImmDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import qualified Analysis as Analysis

main :: IO ()
main = do
    cmdLine@CmdLine{..}   <- getCmdLine
    marker :: Word32 <-
      read . BS.Char8.unpack <$> BS.readFile (clImmDB ++ "/protocolMagicId")
    case marker of
      764824073  -> startAnalysis cmdLine (Proxy :: Proxy ByronBlock)
      1097911063 -> startAnalysis cmdLine (Proxy :: Proxy ByronBlock)
      42         -> startAnalysis cmdLine (Proxy :: Proxy (ShelleyBlock TPraosStandardCrypto))
      _          -> error $ "unsuported dbmarker: " ++ show marker

startAnalysis :: forall blk.
                 (RunNode blk, HasAnalysis blk)
              => CmdLine -> Proxy blk -> IO ()
startAnalysis CmdLine{..} _ = do
    (epochInfo, cfg :: TopLevelConfig blk) <- mkTopLevelConfig clConfig clIsMainNet
    print epochInfo
    withRegistry $ \registry ->
      withImmDB clImmDB cfg registry $ \immDB -> do
        runAnalysis clAnalysis immDB epochInfo registry
        putStrLn "Done"

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

data AnalysisName =
    ShowSlotBlockNo
  | CountTxOutputs
  | ShowBlockHeaderSize
  | ShowBlockTxsSize

type Analysis blk = ImmDB IO blk
                 -> EpochInfo Identity
                 -> ResourceRegistry IO
                 -> IO ()

runAnalysis :: (HasAnalysis blk, RunNode blk)
    => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showBlockHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasHeader blk
                =>  Analysis blk
showSlotBlockNo immDB _epochInfo rr =
    processAll immDB rr go
  where
    go :: blk -> IO ()
    go blk = putStrLn $ intercalate "\t" [
        show (blockNo   blk)
      , show (blockSlot blk)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. (HasHeader blk, HasAnalysis blk)
               => Analysis blk
countTxOutputs immDB epochInfo rr = do
    cumulative <- newIORef 0
    processAll immDB rr (go cumulative)
  where
    go :: IORef Int -> blk -> IO ()
    go cumulative blk = case Analysis.countTxOutputs blk of
        Nothing ->
          return () -- Skip
        Just count -> do
          countCum  <- atomicModifyIORef cumulative $ \c ->
                         let c' = c + count in (c', c')
          putStrLn $ intercalate "\t" [
              show slotNo
            , show epochSlot
            , show count
            , show countCum
            ]
      where
        slotNo = blockSlot blk
        epochSlot = relativeSlotNo epochInfo slotNo

-- | Convert 'SlotNo' to relative 'EpochSlot'
--
-- We use this only to produce more informative output: "this is the @x@'th
-- block within this epoch", similar to what the explorer reports. We don't
-- output EBBs at all, so we place the first real block in an epoch at relative
-- slot 0 (unlike the imm DB, which puts it at relative slot 1 within a chunk).
relativeSlotNo :: EpochInfo Identity -> SlotNo -> (EpochNo, Word64)
relativeSlotNo chunkInfo (SlotNo absSlot) = runIdentity $ do
    epoch        <- epochInfoEpoch chunkInfo (SlotNo absSlot)
    SlotNo first <- epochInfoFirst chunkInfo epoch
    return (epoch, absSlot - first)

{-------------------------------------------------------------------------------
  Analysis: show the block header size in bytes for all blocks
-------------------------------------------------------------------------------}

showBlockHeaderSize :: forall blk. (HasAnalysis blk, HasHeader blk)
                    => Analysis blk
showBlockHeaderSize immDB epochInfo rr = do
    maxBlockHeaderSizeRef <- newIORef 0
    processAll immDB rr (go maxBlockHeaderSizeRef)
    maxBlockHeaderSize <- readIORef maxBlockHeaderSizeRef
    putStrLn ("Maximum encountered block header size = " <> show maxBlockHeaderSize)
  where
    go :: IORef Natural -> blk -> IO ()
    go maxBlockHeaderSizeRef blk =
        case Analysis.blockHeaderSize blk of
          Just blockHdrSz -> do
            let slotNo = blockSlot blk
            void $ modifyIORef' maxBlockHeaderSizeRef (max blockHdrSz)
            let epochSlot = relativeSlotNo epochInfo slotNo
            putStrLn $ intercalate "\t" [
                show slotNo
              , show epochSlot
              , "Block header size = " <> show blockHdrSz
              ]
          Nothing -> return () -- Skip

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. (HasHeader blk, HasAnalysis blk)
  => Analysis blk
showBlockTxsSize immDB epochInfo rr = processAll immDB rr process
  where
    process :: blk -> IO ()
    process blk = case Analysis. blockTxsSize blk of
      Nothing -> return () -- Skip
      Just txs ->
          putStrLn $ intercalate "\t" [
              show slotNo
            , show epochSlot
            , "Num txs in block = " <> show numBlockTxs
            , "Total size of txs in block = " <> show blockTxsSize
            ]
        where

          numBlockTxs :: Int
          numBlockTxs = length txs

          blockTxsSize :: Int64
          blockTxsSize = foldl' (+) 0 $
            map BL.length txs

          slotNo = blockSlot blk
          epochSlot = relativeSlotNo epochInfo slotNo

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the imm DB
-------------------------------------------------------------------------------}

processAll :: forall blk.
              HasHeader blk
           => ImmDB IO blk
           -> ResourceRegistry IO
           -> (blk -> IO ())
           -> IO ()
processAll immDB rr callback = do
    tipPoint <- getPointAtTip immDB
    case pointToWithOriginRealPoint tipPoint of
      Origin -> return ()
      At tip -> do
        Right itr <- stream immDB rr GetBlock
          (StreamFromExclusive genesisPoint)
          (StreamToInclusive tip)
        go itr
  where
    go :: Iterator (HeaderHash blk) IO (IO blk) -> IO ()
    go itr = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          IteratorExhausted   -> return ()
          IteratorResult mblk -> mblk >>= \blk -> callback blk >> go itr

{-------------------------------------------------------------------------------
  Command line args
-------------------------------------------------------------------------------}

data CmdLine = CmdLine {
      clConfig    :: FilePath
    , clIsMainNet :: Bool
    , clImmDB     :: FilePath
    , clAnalysis  :: AnalysisName
--    , clEra       :: Era
    }

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> strOption (mconcat [
            long "config"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> flag True False (mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> strOption (mconcat [
            long "db"
          , help "Path to the chain DB (parent of \"immutable\" directory)"
          , metavar "PATH"
          ])
    <*> parseAnalysis

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    ]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework for running analysis over the immutable DB"
        ])

{-------------------------------------------------------------------------------
  Interface with the ImmDB
-------------------------------------------------------------------------------}

withImmDB :: forall blk a.
             RunNode blk
          => FilePath
          -> TopLevelConfig blk
          -> ResourceRegistry IO
          -> (ImmDB IO blk -> IO a)
          -> IO a
withImmDB fp cfg registry = ImmDB.withImmDB args
  where
    bcfg = configCodec cfg
    pb   = Proxy @blk

    args :: ImmDbArgs IO blk
    args = (defaultArgs fp) {
          immDecodeHash     = nodeDecodeHeaderHash    pb
        , immDecodeBlock    = nodeDecodeBlock         bcfg
        , immDecodeHeader   = nodeDecodeHeader        bcfg SerialisedToDisk
        , immEncodeHash     = nodeEncodeHeaderHash    pb
        , immEncodeBlock    = nodeEncodeBlockWithInfo bcfg
        , immChunkInfo      = nodeImmDbChunkInfo cfg
        , immHashInfo       = nodeHashInfo            pb
        , immValidation     = ValidateMostRecentChunk
        , immIsEBB          = nodeIsEBB
        , immCheckIntegrity = nodeCheckIntegrity      cfg
        , immAddHdrEnv      = nodeAddHeaderEnvelope   pb
        , immRegistry       = registry
        }
