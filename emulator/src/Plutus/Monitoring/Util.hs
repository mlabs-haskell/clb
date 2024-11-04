{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.Monitoring.Util (
  -- handleLogMsgTrace,
  -- handleLogMsgTraceMap,
  -- handleObserveTrace,
  runLogEffects,
  convertLog,
  -- toSeverity,
) where

import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Severity
import Cardano.BM.Data.Trace
import Cardano.BM.Trace
import Control.Monad.Freer
import Control.Monad.Freer.Extras.Log (LogMsg (..))
import Control.Monad.Freer.Extras.Log qualified as L
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))

runLogEffects ::
  forall m l.
  (MonadIO m) =>
  Trace m l ->
  Eff '[LogMsg l, m]
    ~> m
runLogEffects trace = runM . interpret (handleLogMsgTrace trace)

-- | Handle the 'LogMsg' effect by logging messages to a 'Trace'
handleLogMsgTrace ::
  forall a m effs.
  ( LastMember m effs
  , MonadIO m
  ) =>
  Trace m a ->
  LogMsg a
    ~> Eff effs
handleLogMsgTrace trace = \case
  LMessage L.LogMessage {L._logLevel, L._logMessageContent} ->
    let defaultPrivacy = Public -- TODO: Configurable / add to 'L.LogMessage'?
     in sendM $ traceNamedItem trace defaultPrivacy (toSeverity _logLevel) _logMessageContent

toSeverity :: L.LogLevel -> Severity
toSeverity = \case
  L.Debug -> Debug
  L.Info -> Info
  L.Notice -> Notice
  L.Warning -> Warning
  L.Error -> Error
  L.Critical -> Critical
  L.Alert -> Alert
  L.Emergency -> Emergency

-- | Convert tracer structured log data
convertLog :: (a -> b) -> Trace m b -> Trace m a
convertLog f = contramap (second (fmap f))
