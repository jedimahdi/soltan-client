module Soltan.AppM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Safe.Coerce (coerce)
import Soltan.Capability.LogMessages (class LogMessages)
import Soltan.Capability.Now (class Now)
import Soltan.Data.Log as Log

newtype AppM a = AppM (Aff a)

unApp :: forall a. AppM a -> Aff a
unApp (AppM f) = f

runAppM :: forall q i o. H.Component q i o AppM -> H.Component q i o Aff
runAppM = coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    let logLevel = Dev
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log
