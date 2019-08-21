{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Error policies, and integration with 'SuspendDecision'-semigroup action on
-- 'PeerState'.
--
module Ouroboros.Network.ErrorPolicy
  ( ErrorPolicies (..)
  , nullErrorPolicies
  , ErrorPolicy (..)
  , evalErrorPolicy
  , evalErrorPolicies
  , ConnectionOrApplicationException (..)
  , CompleteApplication
  , Result (..)
  , completeApplicationTx

  -- * Traces
  , ErrorPolicyTrace (..)
  , traceErrorPolicy
  , WithAddr (..)

  -- * Re-exports of PeerState
  , PeerStates
  , SuspendDecision (..)
  ) where

import           Control.Exception (Exception, SomeException (..))
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable ( Proxy (..)
                               , gcast
                               , tyConName
                               , typeRepTyCon
                               , typeRep
                               )
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Data.Semigroup.Action

import           Ouroboros.Network.Subscription.PeerState

data ErrorPolicy where
     ErrorPolicy :: forall err.
                      Exception err
                   => (ConnectionOrApplicationException err -> SuspendDecision DiffTime)
                   -> ErrorPolicy

instance Show ErrorPolicy where
    show (ErrorPolicy (_err :: ConnectionOrApplicationException err -> SuspendDecision DiffTime)) =
           "ErrorPolicy ("
        ++ tyConName (typeRepTyCon (typeRep (Proxy :: Proxy err)))
        ++ ")"

evalErrorPolicy :: forall e.
                   Exception e
                => ConnectionOrApplicationException e
                -> ErrorPolicy
                -> Maybe (SuspendDecision DiffTime)
evalErrorPolicy e p =
    case p of
      ErrorPolicy (f :: ConnectionOrApplicationException e' -> SuspendDecision DiffTime)
        -> case gcast e :: Maybe (ConnectionOrApplicationException e') of
              Nothing -> Nothing
              Just e' -> Just $ f e'

-- | Evaluate a list of 'ErrorPolicy's; If none of them applies this function
-- returns 'Nothing', in this case the exception will be traced and not thrown.
--
evalErrorPolicies :: forall e.
                     Exception e
                  => ConnectionOrApplicationException e
                  -> [ErrorPolicy]
                  -> Maybe (SuspendDecision DiffTime)
evalErrorPolicies e =
    f . mapMaybe (evalErrorPolicy e)
  where
    f :: [SuspendDecision DiffTime]
      -> Maybe (SuspendDecision DiffTime)
    f []          = Nothing
    f (cmd : rst) = Just $ sconcat (cmd :| rst)


-- | List of error policies for exception handling and a policy for handing
-- application return values.
--
data ErrorPolicies m addr a = ErrorPolicies {
    epErrorPolicies  :: [ErrorPolicy]
  , epReturnCallback :: Time m -> addr -> a -> SuspendDecision DiffTime
  }

nullErrorPolicies :: ErrorPolicies m addr a
nullErrorPolicies = ErrorPolicies [] (\_ _ _ -> Throw)

instance Semigroup (ErrorPolicies m addr a) where
    ErrorPolicies ep fn <> ErrorPolicies ep' fn'
      = ErrorPolicies (ep <> ep') (fn <> fn')

-- | Sum type which distinguishes between connection and application
-- exceptions.
--
data ConnectionOrApplicationException err =
     -- | Exception thrown by `connect`
     ConnectionException err
     -- | Exception thrown by an application
   | ApplicationException err
   deriving (Show, Functor)


-- | Complete a connection, which receive application result (or exception).
--
type CompleteApplication m s addr r = Result m addr r -> s -> STM m (s, m ())


-- | Result of the connection thread.  It's either result of an application, or
-- an exception thrown by it.
--
data Result m addr r where
     ApplicationResult
       :: !(Time m)
       -> !addr
       -> !r
       -> Result m addr r

     Connected
       :: !(Time m)
       -> !addr
       -> Result m addr r

     ConnectionError
       :: Exception e
       => !(Time m)
       -> !addr
       -> !e
       -> Result m addr r

     ApplicationError
       :: Exception e
       => !(Time m)
       -> !addr
       -> !e
       -> Result m addr r


-- | 'CompleteApplication' callback
--
completeApplicationTx
  :: forall m addr a.
     ( MonadAsync  m
     , Ord addr
     , Ord (Async m ())
     , TimeMeasure (Time m)
     )
  => Tracer m (WithAddr addr ErrorPolicyTrace)
  -> ErrorPolicies m addr a
  -> CompleteApplication m
       (PeerStates m addr (Time m))
       addr
       a

-- the 'ResultQ' did not throw the exception yet; it should not happen.
completeApplicationTx _ _ _ ps@ThrowException{} = pure ( ps, pure () )

-- application returned; classify the return value and update the state.
completeApplicationTx tracer ErrorPolicies {epReturnCallback} (ApplicationResult t addr r) (PeerStates ps) =
  let cmd = epReturnCallback t addr r
      fn :: Maybe (PeerState m (Time m))
         -> ( Set (Async m ())
            , Maybe (PeerState m (Time m))
            )
      fn mbps = ( maybe Set.empty (`threadsToCancel` cmd) mbps
                , mbps <| (flip addTime t <$> cmd)
                )
  in case alterAndLookup fn addr ps of
    (ps', mbthreads) -> pure
      ( PeerStates ps'
      , do
          traverse_ (traceWith tracer . WithAddr addr)
                    (traceErrorPolicy (Right r) cmd)
          traverse_ cancel
                    (fromMaybe Set.empty mbthreads)
      )

-- application errored
completeApplicationTx tracer ErrorPolicies {epErrorPolicies} (ApplicationError t addr e) ps =
  case evalErrorPolicies (ApplicationException e) epErrorPolicies of
    -- the error is not handled by any policy; we're not rethrowing the
    -- error from the main thread, we only trace it.  This will only kill
    -- the local consumer application.
    Nothing  -> pure ( ps
                     , traceWith tracer
                        (WithAddr addr
                          (ErrorPolicyUnhandledApplicationException
                            (SomeException e)))
                     )
    -- the error was classified; act with the 'SuspendDecision' on the state
    -- and find threads to cancel.
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                traverse_ (traceWith tracer . WithAddr addr)
                          (traceErrorPolicy (Left $ ApplicationException (SomeException e))
                                            cmd)
                traverse_ cancel threads
            )

-- we connected to a peer; this does not require to update the 'PeerState'.
completeApplicationTx _ _ (Connected _t  _addr) ps =
  pure ( ps, pure () )

-- error raised by the 'connect' call; we handle this in the same way as
-- application exceptions, the only difference is that we wrap the exception
-- with 'ConnException' type constructor.
completeApplicationTx tracer ErrorPolicies {epErrorPolicies} (ConnectionError t addr e) ps =
  case evalErrorPolicies (ConnectionException e) epErrorPolicies of
    Nothing  ->
      let fn p@(HotPeer producers consumers)
             | Set.null producers && Set.null consumers
             = Just ColdPeer
             | otherwise
             = Just p
          fn p = Just p

      in pure ( case ps of
                  PeerStates peerStates -> PeerStates $ Map.update fn addr peerStates
                  ThrowException{} -> ps
              , traceWith tracer
                 (WithAddr addr
                   (ErrorPolicyUnhandledConnectionException
                     (SomeException e)))
              )
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                 traverse_ (traceWith tracer . WithAddr addr)
                           (traceErrorPolicy (Left $ ConnectionException (SomeException e)) cmd)
                 traverse_ cancel threads
             )

--
-- Traces
--

-- | Trace data for error policies
data ErrorPolicyTrace
  = ErrorPolicySuspendPeer (Maybe (ConnectionOrApplicationException SomeException)) !DiffTime !DiffTime
  -- ^ suspending peer with a given exception until
  | ErrorPolicySuspendConsumer (Maybe (ConnectionOrApplicationException SomeException)) !DiffTime
  -- ^ suspending consumer until
  | ErrorPolicyLocalNodeError (ConnectionOrApplicationException SomeException)
  -- ^ caught a local exception
  | ErrorPolicyResumePeer
  -- ^ resume a peer (both consumer and producer)
  | ErrorPolicyKeepSuspended
  -- ^ consumer was suspended until producer will resume
  | ErrorPolicyResumeConsumer
  -- ^ resume consumer
  | ErrorPolicyResumeProducer
  -- ^ resume producer
  | ErrorPolicyUnhandledApplicationException SomeException
  -- ^ an application throwed an exception, which was not handled by any
  -- 'ErrorPolicy'.
  | ErrorPolicyUnhandledConnectionException SomeException
  -- ^ 'connect' throwed an exception, which was not handled by any
  -- 'ErrorPolicy'.
  deriving Show

traceErrorPolicy :: Either (ConnectionOrApplicationException SomeException) r
                 -> SuspendDecision DiffTime
                 -> Maybe ErrorPolicyTrace
traceErrorPolicy (Left e) (SuspendPeer prodT consT) =
    Just $ ErrorPolicySuspendPeer (Just e) prodT consT
traceErrorPolicy (Right _) (SuspendPeer prodT consT) =
    Just $ ErrorPolicySuspendPeer Nothing prodT consT
traceErrorPolicy (Left e) (SuspendConsumer consT) =
    Just $ ErrorPolicySuspendConsumer (Just e) consT
traceErrorPolicy (Right _) (SuspendConsumer consT) =
    Just $ ErrorPolicySuspendConsumer Nothing consT
traceErrorPolicy (Left e) Throw =
    Just $ ErrorPolicyLocalNodeError e
traceErrorPolicy _ _ =
    Nothing

data WithAddr addr a = WithAddr {
      wiaAddr  :: !addr
    , wiaEvent :: !a
    }

instance (Show addr, Show a) => Show (WithAddr addr a) where
    show WithAddr { wiaAddr, wiaEvent } =
        printf "IP %s %s" (show wiaAddr) (show wiaEvent)