{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Ledger.Byron (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Binary (ByteSpan, slice)
import           Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Chain.UTxO (UTxOValidationError, annotateTxAux)
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Block.Gen as CC.Gen
import qualified Test.Cardano.Chain.UTxO.Gen as CC.Gen


tests :: TestTree
tests = testGroup "Byron"
  [ testGroup "Serialisation roundtrips"
      [ testProperty "roundtrip Block"       prop_roundtrip_Block
      , testProperty "roundtrip Header"      prop_roundtrip_Header
      , testProperty "roundtrip HeaderHash"  prop_roundtrip_HeaderHash
      , testProperty "roundtrip GenTx"       prop_roundtrip_GenTx
      , testProperty "roundtrip GenTxId"     prop_roundtrip_GenTxId
      , testProperty "roundtrip ApplyTxErr"  prop_roundtrip_ApplyTxErr
      ]
  ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

roundtrip' :: (Eq a, Show a)
           => (a -> Encoding)
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' enc dec a = case deserialiseFromBytes dec bs of
    Right (bs', a')
      | Lazy.null bs'
      -> a === a' bs
      | otherwise
      -> counterexample ("left-over bytes: " <> show bs') False
    Left e
      -> counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

annotate :: forall f. Functor f
         => (f () -> Encoding)
         -> (forall s. Decoder s (f ByteSpan))
         -> f ()
         -> f Strict.ByteString
annotate encode decoder =
      (\bs -> splice bs (deserialiseFromBytes decoder bs))
    . toLazyByteString
    . encode
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, f ByteSpan)
           -> f Strict.ByteString
    splice _ (Left _err) =
        error "annotate: serialization roundtrip failure"
    splice bs (Right (bs', x))
      | Lazy.null bs'
      = Lazy.toStrict . slice bs <$> x
      | otherwise
      = error ("left-over bytes: " <> show bs')

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Block -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: Header Block -> Property
prop_roundtrip_Header h =
    roundtrip' encodeByronHeader (decodeByronHeader epochSlots) h

prop_roundtrip_HeaderHash :: HeaderHash Block -> Property
prop_roundtrip_HeaderHash =
    roundtrip encodeByronHeaderHash decodeByronHeaderHash

prop_roundtrip_GenTx :: GenTx Block -> Property
prop_roundtrip_GenTx =
    roundtrip encodeByronGenTx decodeByronGenTx

prop_roundtrip_GenTxId :: GenTxId Block -> Property
prop_roundtrip_GenTxId =
    roundtrip encodeByronGenTxId decodeByronGenTxId

prop_roundtrip_ApplyTxErr :: ApplyTxErr Block -> Property
prop_roundtrip_ApplyTxErr =
    roundtrip encodeByronApplyTxError decodeByronApplyTxError

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

type Block = ByronBlockOrEBB ByronConfig

epochSlots :: EpochSlots
epochSlots = EpochSlots 2160

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

instance Arbitrary Block where
  arbitrary = frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen Block
      genBlock =
        annotateByronBlock epochSlots <$>
        hedgehog (CC.Gen.genBlock protocolMagicId epochSlots)
      genBoundaryBlock :: Gen Block
      genBoundaryBlock =
        ByronBlockOrEBB . ABOBBoundary . annotateBoundary protocolMagicId <$>
        hedgehog (CC.Gen.genBoundaryBlock)


instance Arbitrary (Header Block) where
  arbitrary = frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
    where
      genHeader :: Gen (Header Block)
      genHeader =
        mkByronHeaderOrEBB . Right .
        annotate
          (CC.Block.toCBORHeader epochSlots)
          (CC.Block.fromCBORAHeader epochSlots) <$>
        hedgehog (CC.Gen.genHeader protocolMagicId epochSlots)
      genBoundaryHeader :: Gen (Header Block)
      genBoundaryHeader =
        mkByronHeaderOrEBB . Left .
        annotate
          (CC.Block.toCBORABoundaryHeader protocolMagicId)
          CC.Block.fromCBORABoundaryHeader <$>
        hedgehog CC.Gen.genBoundaryHeader

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> hedgehog CC.Gen.genHeaderHash

instance Arbitrary (GenTx Block) where
  arbitrary =
    mkByronTx . annotateTxAux <$>
    hedgehog (CC.Gen.genTxAux protocolMagicId)

instance Arbitrary (GenTxId Block) where
  arbitrary = ByronTxId <$> hedgehog CC.Gen.genTxId

instance Arbitrary UTxOValidationError where
  arbitrary = hedgehog CC.Gen.genUTxOValidationError
