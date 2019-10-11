{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
-- Volatile on-disk database of binary blobs
--
-- Logic
--
-- The db is a key-value store of binary blocks and is parametric
-- on the key of blocks, named blockId. The only constraints are that one must provide
-- a function (toSlot :: blockId -> SlotNo), as well as an Ord instance of blockId.
-- The db expects NO properties for this Ord instance, not even one that makes
-- toBlock monotonic.

-- The database uses in memory indexes, which are created on each reopening. reopening
-- includes parsing all blocks of the dbFolder, so it can be an expensive operation
-- if the database gets big. That's why the intention of this db is to be used for only
-- the tip of the blockchain, when there is still volatility on which blocks are included.
-- The db is agnostic to the format of the blocks, so a parser must be provided.
-- In addition to getBlock and putBlock, the db provides also the ability to garbage-collect
-- old blocks. The actual garbage-collection happens in terms of files and not blocks: a file
-- is deleted/garbage-collected only if its latest block is old enough. A block is old enough
-- if its toSlot value is old enough and not based on its Ord instance. This type of garbage
-- collection makes the deletion of blocks depend on the number of blocks we insert on
-- each file, as well as the order of insertion, so it's not deterministic on blocks themselves.
--
-- Errors
--
-- On any exception or error the db closes and its Internal State is lost, inluding in memory
-- indexes. We try to make sure that even on errors the fs represantation of the db remains
-- consistent and the Internal State can be recovered on reopening. In general we try to make
-- sure that at any point, losing the in-memory Internal State is not fatal to the db as it can
-- recovered. This is important since we must always expect unexpected shutdowns, power loss,
-- sleep mode etc. This is achived by leting only basic operations on the db:
-- + putBlock only appends a new block on a file. Losing an update means we only lose a block,
--   which can be recovered.
-- + garbage collect deletes only whole files.
-- + there is no modify block operation. Thanks to that we need not keep any rollback journals
--   to make sure we are safe in case of unexpected shutdowns.
--
-- We only throw VolatileDBError. All internal errors, like io errors, are cought, wrapped
-- and rethrown. For all new calls of HasFs, we must make sure that they are used properly
-- wrapped. All top-level function of this module are safe. You can safely use HasFs calls
-- in modifyState or wrapFsError actions.
--
-- Concurrency
--
-- The same db should only be opened once
-- Multiple threads can share the same db as concurency if fully supported.
--
-- FS Layout:
--
-- On disk represantation is as follows:
--
--  dbFolder\
--    blocks-0.dat
--    blocks-1.dat
--    ...
--
--  If on opening any other filename which does not follow blocks-i.dat is found
--  an error is raised. The Ordering of blocks is not guarranteed to be followed,
--  files can be garbage-collected.
--
--  Each file stores a fixed number of slots, specified by _maxBlocksPerFile.
--  If the db finds files with less blocks than this max, it will start appending
--  to the newest of them, if it's the newest of all files. If it's not the newest
--  of all files it will create a new file to append blocks..
--
--  There is an implicit ordering of block files, which is NOT alpharithmetic
--  For example blocks-20.dat < blocks-100.dat
--
module Ouroboros.Storage.VolatileDB.Impl
    ( -- * Opening a database
      openDB
      -- * tests only
    , VolatileDBEnv(..)
    , InternalState(..)
    , filePath
    , getInternalState
    , openDBFull
    ) where

import           Control.Monad
import qualified Data.ByteString.Builder as BS
import           Data.ByteString.Lazy (ByteString)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (SomePair (..), safeMaximumOn)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..),
                     ThrowCantCatch (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.FileInfo (FileInfo,
                     FileSlotInfo (..))
import qualified Ouroboros.Storage.VolatileDB.FileInfo as FileInfo
import           Ouroboros.Storage.VolatileDB.Index
import           Ouroboros.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
  Main Types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h e. VolatileDBEnv {
      _dbHasFS          :: !(HasFS m h)
    , _dbErr            :: !(ErrorHandling (VolatileDBError blockId) m)
    , _dbErrSTM         :: !(ThrowCantCatch (VolatileDBError blockId) (STM m))
    , _dbInternalState  :: !(StrictMVar m (OpenOrClosed blockId h))
    , _maxBlocksPerFile :: !Int
    , _parser           :: !(Parser e m blockId)
    }

data OpenOrClosed blockId h =
    VolatileDbOpen !(InternalState blockId h)
  | VolatileDbClosed
  deriving (Generic, NoUnexpectedThunks)

volatileDbIsOpen :: OpenOrClosed blockId h -> Bool
volatileDbIsOpen (VolatileDbOpen _) = True
volatileDbIsOpen VolatileDbClosed   = False

data InternalState blockId h = InternalState
    { _currentWriteHandle :: !(Handle h)
    -- ^ The unique open file we append blocks.
    , _currentWritePath   :: !FsPath
    -- ^ The path of the file above.
    , _currentWriteOffset :: !Word64
    -- ^ The 'WriteHandle' for the same file.
    , _nextNewFileId      :: !Int
    -- ^ The next file name Id.
    , _currentMap         :: !(Index blockId)
    -- ^ The content of each file.
    , _currentRevMap      :: !(ReverseIndex blockId)
    -- ^ Where to find each block from slot.
    , _currentSuccMap     :: !(SuccessorsIndex blockId)
    -- ^ successors for each block.
    , _currentMaxSlotNo   :: !MaxSlotNo
    -- ^ Highest ever stored SlotNo
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

openDB :: (HasCallStack, IOLike m, Ord blockId, NoUnexpectedThunks blockId)
       => HasFS m h
       -> ErrorHandling (VolatileDBError blockId) m
       -> ThrowCantCatch (VolatileDBError blockId) (STM m)
       -> Parser e m blockId
       -> Int
       -> m (VolatileDB blockId m)
openDB h e e' p m = fst <$> openDBFull h e e' p m

openDBFull :: (HasCallStack, IOLike m, Ord blockId, NoUnexpectedThunks blockId)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> ThrowCantCatch (VolatileDBError blockId) (STM m)
           -> Parser e m blockId
           -> Int
           -> m (VolatileDB blockId m, VolatileDBEnv m blockId)
openDBFull hasFS err errSTM parser maxBlocksPerFile = do
    env <- openDBImpl hasFS err errSTM parser maxBlocksPerFile
    return $ (, env) VolatileDB {
          closeDB        = closeDBImpl  env
        , isOpenDB       = isOpenDBImpl env
        , reOpenDB       = reOpenDBImpl env
        , getBlock       = getBlockImpl env
        , putBlock       = putBlockImpl env
        , garbageCollect = garbageCollectImpl env
        , getIsMember    = getIsMemberImpl env
        , getBlockIds    = getBlockIdsImpl env
        , getSuccessors  = getSuccessorsImpl env
        , getPredecessor = getPredecessorImpl env
        , getMaxSlotNo   = getMaxSlotNoImpl env
        }

-- | NOTE: After opening the db once, the same @maxBlocksPerFile@ must be
-- provided for all next opens.
openDBImpl :: (HasCallStack, IOLike m, Ord blockId, NoUnexpectedThunks blockId)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> ThrowCantCatch (VolatileDBError blockId) (STM m)
           -> Parser e m blockId
           -> Int
           -> m (VolatileDBEnv m blockId)
openDBImpl hasFS@HasFS{..} err errSTM parser maxBlocksPerFile =
    if maxBlocksPerFile <= 0
    then EH.throwError err $ UserError . InvalidArgumentsError $
        "maxBlocksPerFile should be positive"
    else do
        st <- mkInternalStateDB hasFS err parser maxBlocksPerFile
        stVar <- newMVar $ VolatileDbOpen st
        return $ VolatileDBEnv hasFS err errSTM stVar maxBlocksPerFile parser

closeDBImpl :: IOLike m
            => VolatileDBEnv m blockId
            -> m ()
closeDBImpl VolatileDBEnv{..} = do
    mbInternalState <- swapMVar _dbInternalState VolatileDbClosed
    case mbInternalState of
        VolatileDbClosed -> return ()
        VolatileDbOpen InternalState{..} ->
            wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle
  where
    HasFS{..} = _dbHasFS

isOpenDBImpl :: IOLike m
             => VolatileDBEnv m blockId
             -> m Bool
isOpenDBImpl VolatileDBEnv{..} = do
    mSt <- readMVar _dbInternalState
    return $ volatileDbIsOpen mSt

-- | Preconditions: closeDB . reOpenDB is a no-op. This is achieved because
-- when we reOpen we always append onto the latest created file.
reOpenDBImpl :: (HasCallStack, IOLike m, Ord blockId)
             => VolatileDBEnv m blockId
             -> m ()
reOpenDBImpl VolatileDBEnv{..} =
    modifyMVar _dbInternalState $ \mbSt -> case mbSt of
        VolatileDbOpen st -> return (VolatileDbOpen st, ())
        VolatileDbClosed -> do
            st <- mkInternalStateDB _dbHasFS _dbErr _parser _maxBlocksPerFile
            return (VolatileDbOpen st, ())

getBlockImpl :: (IOLike m, Ord blockId)
             => VolatileDBEnv m blockId
             -> blockId
             -> m (Maybe ByteString)
getBlockImpl env@VolatileDBEnv{..} slot =
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
        case Map.lookup slot _currentRevMap of
            Nothing -> return (st, Nothing)
            Just InternalBlockInfo {..} ->  do
                bs <- withFile hasFS ibFile ReadMode $ \hndl -> do
                        _ <- hSeek hndl AbsoluteSeek (fromIntegral ibSlotOffset)
                        hGetExactly hasFS hndl (fromIntegral ibBlockSize)
                return (st, Just bs)

-- | This function follows the approach:
-- (1) hPut bytes to the file
-- (2) if full hClose the write file
-- (3)         hOpen a new write file
-- (4) update the Internal State.
-- If there is an error after (1) or after (2) we should make sure that when
-- we reopen a db from scratch, it can succesfully recover if it does not
-- find an empty file to write and all other files are full.
-- We should also make sure that the fs can be recovered if we get an
-- exception/error at any moment and that we are left with an empty Internal
-- State.
-- We should be careful about not leaking open fds when we open a new file,
-- since this can affect garbage collection of files.
putBlockImpl :: forall m blockId. (IOLike m, Ord blockId)
             => VolatileDBEnv m blockId
             -> BlockInfo blockId
             -> BS.Builder
             -> m ()
putBlockImpl env@VolatileDBEnv{..} binfo@BlockInfo{..} builder =
  modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
    if Map.member bbid _currentRevMap
    then return (st, ()) -- putting an existing block is a no-op.
    else do
        bytesWritten <- hPut hasFS _currentWriteHandle builder
        putBlockImpl' env binfo hasFS st bytesWritten

putBlockImpl' :: forall m blockId h. (IOLike m, Ord blockId)
              => VolatileDBEnv m blockId
              -> BlockInfo blockId
              -> HasFS m h
              -> InternalState blockId h
              -> Word64
              -> m (InternalState blockId h, ())
putBlockImpl' env BlockInfo{..} hasFS@HasFS{..} st@InternalState{..} bytesWritten =
    if not (FileInfo.isFull (_maxBlocksPerFile env) fileInfo')
    then return (st', ())
    else (,()) <$> nextFile hasFS (_dbErr env) env st'
    where
        fileInfo = fromMaybe
            (error $ "Volatile db invariant violation:"
                  ++ "Current write file not found in Index.")
            (Map.lookup _currentWritePath _currentMap)
        fileInfo' = FileInfo.addSlot bslot _currentWriteOffset
            (FileSlotInfo (fromIntegral bytesWritten) bbid) fileInfo
        mp = Map.insert _currentWritePath fileInfo' _currentMap
        internalBlockInfo' = InternalBlockInfo {
              ibFile       = _currentWritePath
            , ibSlotOffset = _currentWriteOffset
            , ibBlockSize  = bytesWritten
            , ibSlot       = bslot
            , ibPreBid     = bpreBid
        }
        revMp = Map.insert bbid internalBlockInfo' _currentRevMap
        st' = st {
              _currentWriteOffset = _currentWriteOffset + fromIntegral bytesWritten
            , _currentMap       = mp
            , _currentRevMap    = revMp
            , _currentSuccMap   = insertMapSet _currentSuccMap (bbid, bpreBid)
            , _currentMaxSlotNo = _currentMaxSlotNo `max` MaxSlotNo bslot
        }

-- The approach we follow here is to try to garbage collect each file.
-- For each file we update the fs and then we update the Internal State.
-- If some fs update fails, we are left with an empty Internal State and a
-- subset of the deleted files in fs. Any unexpected failure (power loss,
-- other exceptions) has the same results, since the Internal State will
-- be empty on re-opening. This is ok only if any fs updates leave the fs
-- in a consistent state every moment.
-- This approach works since we always close the Database in case of errors,
-- but we should rethink it if this changes in the future.
garbageCollectImpl :: forall m blockId. (IOLike m, Ord blockId)
                   => VolatileDBEnv m blockId
                   -> SlotNo
                   -> m ()
garbageCollectImpl env@VolatileDBEnv{..} slot =
    modifyState env $ \hasFS st -> do
        st' <- foldM (tryCollectFile hasFS env slot) st
                (sortOn (unsafeParseFd . fst) $ Map.toList (_currentMap st))
        return (st', ())

-- For the given file, we check if it should be garbage collected.
-- At the same time we return the updated InternalState.
-- Important note here is that, every call should leave the fs in a
-- consistent state, without depending on other calls.
-- This is achieved so far, since fs calls are reduced to
-- removeFile and truncate 0.
-- This may throw an FsError.
tryCollectFile :: forall m h blockId
               .  (MonadThrow m, Ord blockId)
               => HasFS m h
               -> VolatileDBEnv m blockId
               -> SlotNo
               -> InternalState blockId h
               -> (FsPath, FileInfo blockId)
               -> m (InternalState blockId h)
tryCollectFile hasFS@HasFS{..} env slot st@InternalState{..} (file, fileInfo) =
    if  | not canGC     -> return st
        | not isCurrent -> do
            removeFile file
            return st { _currentMap = Map.delete file _currentMap
                      , _currentRevMap = rv'
                      , _currentSuccMap = succMap'
                      }
        | isCurrentNew  -> return st
        | True          -> do
            st' <- reOpenFile hasFS (_dbErr env) env st
            return st' { _currentRevMap = rv'
                       , _currentSuccMap = succMap'
                       }
    where
        canGC        = FileInfo.canGC fileInfo slot
        isCurrent    = file == _currentWritePath
        isCurrentNew = _currentWriteOffset == 0
        bids         = FileInfo.blockIds fileInfo
        rv'          = Map.withoutKeys _currentRevMap (Set.fromList bids)
        deletedPairs =
            mapMaybe (\b -> (b,) . ibPreBid <$> Map.lookup b _currentRevMap) bids
        succMap'     = foldl deleteMapSet _currentSuccMap deletedPairs

getInternalState :: forall m blockId. IOLike m
                 => VolatileDBEnv m blockId
                 -> m (SomePair (HasFS m) (InternalState blockId))
getInternalState VolatileDBEnv{..} = do
    mSt <- readMVar _dbInternalState
    case mSt of
        VolatileDbClosed  -> EH.throwError _dbErr $ UserError ClosedDBError
        VolatileDbOpen st -> return $ SomePair _dbHasFS st

getIsMemberImpl :: forall m blockId. (IOLike m, Ord blockId)
                => VolatileDBEnv m blockId
                -> STM m (blockId -> Bool)
getIsMemberImpl env =
    getterStm env $ \st bid -> Map.member bid (_currentRevMap st)

getBlockIdsImpl :: forall m blockId. (IOLike m)
                => VolatileDBEnv m blockId
                -> m [blockId]
getBlockIdsImpl env@VolatileDBEnv{..} =
    getter env $ Map.keys . _currentRevMap

getSuccessorsImpl :: forall m blockId. (IOLike m, Ord blockId)
                  => VolatileDBEnv m blockId
                  -> STM m (WithOrigin blockId -> Set blockId)
getSuccessorsImpl env =
    getterStm env $ \st blockId ->
        fromMaybe Set.empty (Map.lookup blockId (_currentSuccMap st))

getPredecessorImpl :: forall m blockId. (IOLike m, Ord blockId, HasCallStack)
                   => VolatileDBEnv m blockId
                   -> STM m (blockId -> WithOrigin blockId)
getPredecessorImpl env =
    getterStm env $ \st blockId ->
        maybe (error msg) ibPreBid (Map.lookup blockId (_currentRevMap st))
  where
    msg = "precondition violated: block not member of the VolatileDB"

getMaxSlotNoImpl :: forall m blockId. IOLike m
                 => VolatileDBEnv m blockId
                 -> STM m MaxSlotNo
getMaxSlotNoImpl env =
    getterStm env _currentMaxSlotNo

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- A new file is created.
-- This may throw an FsError.
nextFile :: forall h m blockId. IOLike m
         => HasFS m h
         -> ErrorHandling (VolatileDBError blockId) m
         -> VolatileDBEnv m blockId
         -> InternalState blockId h
         -> m (InternalState blockId h)
nextFile HasFS{..} _err VolatileDBEnv{..} st = do
    hClose $ _currentWriteHandle st
    hndl <- hOpen file (AppendMode MustBeNew)
    return $ st {
          _currentWriteHandle = hndl
        , _currentWritePath   = file
        , _currentWriteOffset = 0
        , _nextNewFileId      = _nextNewFileId st + 1
        , _currentMap = Map.insert file FileInfo.empty (_currentMap st)
    }
    where
        file = filePath $ _nextNewFileId st

-- This may throw an FsError.
reOpenFile :: forall m h blockId
           .  (MonadThrow m)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> VolatileDBEnv m blockId
           -> InternalState blockId h
           -> m (InternalState blockId h)
reOpenFile HasFS{..} _err VolatileDBEnv{..} st@InternalState{..} = do
    -- The manual for truncate states that it does not affect offset.
    -- However the file is open on Append Only, so it should automatically go
    -- to the end before each write.
   hTruncate _currentWriteHandle 0
   return $ st {
         _currentMap = Map.insert _currentWritePath FileInfo.empty _currentMap
       , _currentWriteOffset = 0
    }

mkInternalStateDB :: (HasCallStack, MonadThrow m, MonadCatch m, Ord blockId)
                  => HasFS m h
                  -> ErrorHandling (VolatileDBError blockId) m
                  -> Parser e m blockId
                  -> Int
                  -> m (InternalState blockId h)
mkInternalStateDB hasFS@HasFS{..} err parser maxBlocksPerFile =
  wrapFsError hasFsErr err $ do
    createDirectoryIfMissing True dir
    allFiles <- Set.map toFsPath <$> listDirectory dir
    mkInternalState hasFS err parser maxBlocksPerFile allFiles
  where
    dir = mkFsPath []

    toFsPath :: String -> FsPath
    toFsPath file = mkFsPath [file]

-- | This function may create a new
mkInternalState :: forall blockId m h e. (HasCallStack, MonadCatch m, Ord blockId)
                => HasFS m h
                -> ErrorHandling (VolatileDBError blockId) m
                -> Parser e m blockId
                -> Int
                -> Set FsPath
                -> m (InternalState blockId h)
mkInternalState hasFS err parser n files =
  wrapFsError (hasFsErr hasFS) err $ do
    lastFd <- findLastFdM err files
    mkInternalState' hasFS err parser n files lastFd

mkInternalState' :: forall blockId m h e. (HasCallStack, MonadCatch m, Ord blockId)
                 => HasFS m h
                 -> ErrorHandling (VolatileDBError blockId) m
                 -> Parser e m blockId
                 -> Int
                 -> Set FsPath
                 -> Maybe FileId
                 -> m (InternalState blockId h)
mkInternalState' hasFS err parser n files lastFd =
    go Map.empty Map.empty Map.empty Nothing [] (Set.toList files)
    where
        newFileInfo mp newIndex =
            ( newFile
            , newIndex + (1 :: Int)
            , Map.insert newFile FileInfo.empty mp
            , 0 )
            where
                newFile = filePath newIndex

        errorMsg = concat
            [ "Volatile db invariant violation:"
            , "A file was found with less than "
            , show n
            , " blocks, but there are no files parsed."]

        truncateOnError Nothing _ _ = return ()
        truncateOnError (Just _) file offset =
            -- the handle of the parser is closed at this point. We need
            -- to reOpen the file in AppendMode now (parser opens with
            -- ReadMode).
            --
            -- Note that no file is open at this point, so we can safely
            -- open with AppendMode any file, without the fear of opening
            -- multiple concurrent writers, which is not allowed.
            --
            withFile hasFS file (AppendMode AllowExisting) $ \hndl ->
                hTruncate hasFS hndl (fromIntegral offset)

        go :: Index blockId
           -> ReverseIndex blockId
           -> SuccessorsIndex blockId
           -> Maybe (blockId, SlotNo)
           -> [(FileId, FsPath, FileSize)] -- Info of files with < n blocks.
           -> [FsPath]
           -> m (InternalState blockId h)
        go mp revMp succMp maxSlot haveLessThanN (file : restFiles) = do
            (blocks, mErr) <- parse parser file
            goAux mp revMp succMp maxSlot haveLessThanN file restFiles blocks mErr
        go mp revMp succMp _maxSlot haveLessThanN [] = do
            hndl <- hOpen hasFS fileToWrite (AppendMode AllowExisting)
            return $ InternalState {
                  _currentWriteHandle = hndl
                , _currentWritePath   = fileToWrite
                , _currentWriteOffset = offset'
                , _nextNewFileId      = nextNewFileId'
                , _currentMap         = mp'
                , _currentRevMap      = revMp
                , _currentSuccMap     = succMp
                , _currentMaxSlotNo   = FileInfo.maxSlotInFiles (Map.elems mp')
            }
            where
                (fileToWrite, nextNewFileId', mp', offset') =
                    case (safeMaximumOn (\(a,_,_) -> a) haveLessThanN, lastFd) of
                        (Nothing, _) -> newFileInfo mp $ case lastFd of
                            Just lst -> lst + 1
                            Nothing -> 0
                        (_, Nothing) -> error errorMsg
                        (Just (fd, wrfile, size), Just lst) | fd == lst->
                            -- if the file with biggest index is non-empty,
                            -- then this is the file we're looking for.
                            (wrfile, lst + 1, mp, size)
                        (_, Just lst) ->
                            -- If it's not the last file, we just ignore it and
                            -- open a new one.
                            newFileInfo mp $ lst + 1

        goAux mp revMp succMp maxSlot haveLessThanN file restFiles blocks mErr = do
            truncateOnError mErr file offset
            newRevMp <- fromEither err $ reverseMap file revMp fileMp
            go newMp newRevMp newSuccMp newMaxSlot newHaveLessThanN restFiles
            where
                offset = case reverse blocks of
                    [] -> 0
                    (slotOffset, (blockSize,_)) : _ ->
                        -- The file offset is given by the offset of the last
                        -- block plus its size.
                        slotOffset + blockSize
                fileMp = Map.fromList blocks
                (fileInfo, maxSlotOfFile) = FileInfo.fromParsedInfo blocks
                newMp = Map.insert file fileInfo mp
                newMaxSlot = maxSlotList $ catMaybes [maxSlot, maxSlotOfFile]
                -- For each block we need to update the succesor Map of its
                -- predecesor.
                newSuccMp = foldr
                    (\(_,(_, blockInfo)) succMp' ->
                        insertMapSet succMp' (bbid blockInfo, bpreBid blockInfo))
                        succMp
                        blocks
                -- unsafe here is reasonable because we have already checked
                -- that all filenames parse.
                fd = unsafeParseFd file
                newHaveLessThanN = if FileInfo.isFull n fileInfo
                    then haveLessThanN
                    else (fd, file, offset) : haveLessThanN

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyState :: forall blockId m r. (HasCallStack, IOLike m)
            => VolatileDBEnv m blockId
            -> (forall h
               .  HasFS m h
               -> InternalState blockId h
               -> m (InternalState blockId h, r)
               )
            -> m r
modifyState VolatileDBEnv{_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (tryVolDB hasFsErr _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (_, r) -> return r
  where
    ErrorHandling{..} = _dbErr
    HasFS{..}         = hasFS

    open :: m (OpenOrClosed blockId h)
    open = takeMVar _dbInternalState

    close :: OpenOrClosed blockId h
          -> ExitCase (Either (VolatileDBError blockId) (InternalState blockId h, r))
          -> m ()
    close mst ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> putMVar _dbInternalState mst
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        putMVar _dbInternalState VolatileDbClosed
        closeOpenHandle mst
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (newState, _)) ->
        putMVar _dbInternalState (VolatileDbOpen newState)
      -- In case of an error (not an exception), close the DB for safety
      ExitCaseSuccess (Left _) -> do
        putMVar _dbInternalState VolatileDbClosed
        closeOpenHandle mst

    mutation :: OpenOrClosed blockId h
             -> m (InternalState blockId h, r)
    mutation VolatileDbClosed          = throwError $ UserError ClosedDBError
    mutation (VolatileDbOpen oldState) = action hasFS oldState

    -- TODO what if this fails?
    closeOpenHandle :: OpenOrClosed blockId h -> m ()
    closeOpenHandle VolatileDbClosed                    = return ()
    closeOpenHandle (VolatileDbOpen InternalState {..}) =
        wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle

getter :: IOLike m
       => VolatileDBEnv m blockId
       -> (forall h. InternalState blockId h -> a)
       -> m a
getter VolatileDBEnv{..} fromSt = do
    mSt <- readMVar _dbInternalState
    case mSt of
        VolatileDbClosed  -> EH.throwError _dbErr $ UserError ClosedDBError
        VolatileDbOpen st -> return $ fromSt st

getterStm :: forall m blockId a. IOLike m
          => VolatileDBEnv m blockId
          -> (forall h. InternalState blockId h -> a)
          -> STM m a
getterStm VolatileDBEnv{..} fromSt = do
    mSt <- readMVarSTM _dbInternalState
    case mSt of
        VolatileDbClosed  -> EH.throwError' _dbErrSTM $ UserError ClosedDBError
        VolatileDbOpen st -> return $ fromSt st

reverseMap :: forall blockId
           .  Ord blockId
           => FsPath
           -> ReverseIndex blockId
           -> Map SlotOffset (BlockSize, BlockInfo blockId)
           -> Either (VolatileDBError blockId) (ReverseIndex blockId)
reverseMap file revMp mp = foldM go revMp (Map.toList mp)
    where
        go :: ReverseIndex blockId
           -> (SlotOffset, (BlockSize, BlockInfo blockId))
           -> Either (VolatileDBError blockId) (ReverseIndex blockId)
        go rv (offset, (size, BlockInfo {..})) = case Map.lookup bbid rv of
            Nothing -> Right $ Map.insert
                bbid (InternalBlockInfo file offset size bslot bpreBid) rv
            Just blockInfo -> Left $ UnexpectedError . ParserError
                $ DuplicatedSlot bbid file (ibFile blockInfo)

-- | Throws an error if one of the given file names does not parse.
findLastFdM :: forall m blockId. Monad m
            => ErrorHandling (VolatileDBError blockId) m
            -> Set FsPath
            -> m (Maybe FileId)
findLastFdM err files = case findLastFd files of
    Left e  -> EH.throwError err e
    Right r -> return r
