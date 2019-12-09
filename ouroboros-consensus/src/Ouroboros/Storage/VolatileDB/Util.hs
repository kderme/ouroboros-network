{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.VolatileDB.Util
    ( -- * FileId utilities
      parseFd
    , unsafeParseFd
    , parseAllFds
    , blockFile
    , metaFile
    , findLastFd
    , keepFileId

      -- * Exception handling
    , fromEither
    , modifyMVar
    , wrapFsError
    , tryVolDB

      -- * Map of Set utilities
    , insertMapSet
    , deleteMapSet

      -- * Comparing utilities
    , maxSlotList
    , cmpMaybe
    , updateSlot
    , updateSlotNoBlockId
    ) where

import           Control.Monad
import           Data.Either (partitionEithers)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (lastMaybe, safeMaximum,
                     safeMaximumOn)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.Types

{------------------------------------------------------------------------------
  FileId utilities
------------------------------------------------------------------------------}

data FileType = Blocks | MetaData
    deriving (Show, Eq)

parseFd :: FsPath -> Either VolatileDBError (FileId, FileType)
parseFd file = maybe err Right $
    parseFilename <=< lastMaybe $ fsPathToList file
  where
    err = Left $ UnexpectedError $ ParserError $ InvalidFilename file

    parseFilename :: Text -> Maybe (FileId, FileType)
    parseFilename txt = do
        fileId <- readMaybe $ T.unpack fileIdStr
        fileType <- case fileTypeStr of
          "blocks"   -> Just Blocks
          "metadata" -> Just MetaData
          _          -> Nothing
        return (fileId, fileType)
      where
        (fileTypeStr, fileIdStr) = T.breakOnEnd "-" $ fst $ T.breakOn "." txt

data FileEntryState = EntryBlock FsPath
                    | EntryMeta FsPath
                    | EntryComplete (FsPath, FsPath)
                    | EntryError [FsPath]

type FileEntries = Map FileId FileEntryState

updateState :: (FsPath, FileType)
            -> Maybe FileEntryState
            -> FileEntryState
updateState (path, Blocks) Nothing = EntryBlock path
updateState (path, MetaData) Nothing = EntryMeta path
updateState (path, Blocks) (Just (EntryMeta metafile))
    = EntryComplete (path, metafile)
updateState (path, MetaData) (Just (EntryBlock blockfile))
    = EntryComplete (blockfile, path)
updateState (path, _) (Just (EntryError paths)) = EntryError $ path : paths
updateState (path, _) _ = EntryError [path]

-- | Parses the 'FileId' of each 'FsPath' and zips them together.
-- When parsing fails, we abort with the corresponding parse error.
parseAllFds :: [FsPath] -> Either VolatileDBError [(FileId, FsPath, FsPath)]
parseAllFds files = do
    groups <- groupEntries Map.empty files
    validateGroups groups
  where
    groupEntries :: FileEntries
                 -> [FsPath]
                 -> Either VolatileDBError FileEntries
    groupEntries entries [] = Right entries
    groupEntries entries (fsPath : rest) = do
      -- If a file does not parse, we immediately stop and do not try to recover.
      (fileId, fileType) <- parseFd fsPath
      -- If we catch an error for a specific fileId, we don't stop, because we want
      -- to catch them all and clean them.
      let newEntries = Map.alter (Just . updateState (fsPath, fileType)) fileId entries
      groupEntries newEntries rest

    validateGroups :: FileEntries
                   -> Either VolatileDBError [(FileId, FsPath, FsPath)]
    validateGroups entries =
        case partitionEithers $ map validate $ Map.toList entries of
          ([], completed) -> Right completed
          (withError, _)  ->
            Left $ UnexpectedError $ ParserError $ InvalidFileGroups withError

    validate :: (FileId, FileEntryState)
             -> Either [FsPath] (FileId, FsPath, FsPath)
    validate (fileId, entryState) = case entryState of
      EntryComplete (blocksFile, metasFile) ->
          Right (fileId, blocksFile, metasFile)
      EntryBlock path  -> Left [path]
      EntryMeta path   -> Left [path]
      EntryError paths -> Left paths

keepFileId :: (a, b, c) -> a
keepFileId (a, _, _) = a

-- | When parsing fails, we abort with the corresponding parse error.
findLastFd :: Set FsPath -> Either VolatileDBError (Maybe FileId)
findLastFd = fmap safeMaximum .  mapM (fmap fst . parseFd) . Set.toList

blockFile :: FileId -> FsPath
blockFile fd = mkFsPath ["blocks-" ++ show fd ++ ".dat"]

metaFile :: FileId -> FsPath
metaFile fd = mkFsPath ["metadata-" ++ show fd ++ ".dat"]

unsafeParseFd :: FsPath -> FileId
unsafeParseFd file = either
    (\_ -> error $ "Could not parse filename " <> show file)
    fst
    (parseFd file)

{------------------------------------------------------------------------------
  Exception handling
------------------------------------------------------------------------------}

fromEither :: Monad m
           => ErrorHandling e m
           -> Either e a
           -> m a
fromEither err = \case
    Left e -> EH.throwError err e
    Right a -> return a

modifyMVar :: IOLike m
           => StrictMVar m a
           -> (a -> m (a,b))
           -> m b
modifyMVar m action =
    snd . fst <$> generalBracket (takeMVar m)
       (\oldState ec -> case ec of
            ExitCaseSuccess (newState,_) -> putMVar m newState
            ExitCaseException _ex        -> putMVar m oldState
            ExitCaseAbort                -> putMVar m oldState
       ) action

wrapFsError :: Monad m
            => ErrorHandling FsError         m
            -> ErrorHandling VolatileDBError m
            -> m a -> m a
wrapFsError fsErr volDBErr action =
    tryVolDB fsErr volDBErr action >>= either (throwError volDBErr) return

-- | Execute an action and catch the 'VolatileDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'VolatileDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the VolatileDB
-- and catch the 'VolatileDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryVolDB :: forall m a. Monad m
         => ErrorHandling FsError         m
         -> ErrorHandling VolatileDBError m
         -> m a -> m (Either VolatileDBError a)
tryVolDB fsErr volDBErr = fmap squash . EH.try fsErr . EH.try volDBErr
  where
    fromFS = UnexpectedError . FileSystemError

    squash :: Either FsError (Either VolatileDBError a)
           -> Either VolatileDBError a
    squash = either (Left . fromFS) id

{------------------------------------------------------------------------------
  Map of Set utilities
------------------------------------------------------------------------------}

alterfInsert :: Ord blockId
             => blockId
             -> Maybe (Set blockId)
             -> Maybe (Set blockId)
alterfInsert successor mSet = case mSet of
    Nothing  -> Just $ Set.singleton successor
    Just set -> Just $ Set.insert successor set

insertMapSet :: Ord blockId
             => SuccessorsIndex blockId
             -> (blockId, WithOrigin blockId)
             -> SuccessorsIndex blockId
insertMapSet mapSet (bid, pbid) = Map.alter (alterfInsert bid) pbid mapSet

alterfDelete :: Ord blockId
             => blockId
             -> Maybe (Set blockId)
             -> Maybe (Set blockId)
alterfDelete successor mSet = case mSet of
    Nothing  -> Nothing
    Just set -> Just $ Set.delete successor set

deleteMapSet :: Ord blockId
             => SuccessorsIndex blockId
             -> (blockId, WithOrigin blockId)
             -> SuccessorsIndex blockId
deleteMapSet mapSet (bid, pbid) = Map.alter (alterfDelete bid) pbid mapSet

{------------------------------------------------------------------------------
  Comparing utilities
------------------------------------------------------------------------------}

maxSlotList :: [(blockId, SlotNo)] -> Maybe (blockId, SlotNo)
maxSlotList = updateSlot Nothing

cmpMaybe :: Ord a => Maybe a -> a -> Bool
cmpMaybe Nothing _   = False
cmpMaybe (Just a) a' = a >= a'

updateSlot :: forall blockId. Maybe (blockId, SlotNo)
           -> [(blockId, SlotNo)]
           -> Maybe (blockId, SlotNo)
updateSlot msl ls = safeMaximumOn snd $ case msl of
    Nothing -> ls
    Just sl -> sl : ls

updateSlotNoBlockId :: MaxSlotNo -> [SlotNo] -> MaxSlotNo
updateSlotNoBlockId msl ls = maxSlotNoFromMaybe $ safeMaximum $ case msl of
    NoMaxSlotNo  -> ls
    MaxSlotNo sl -> sl : ls
