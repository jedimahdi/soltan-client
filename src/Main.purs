module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (launchAff_)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Soltan.AppM (runAppM)
import Soltan.Component.Game as Game
import Soltan.Data.Username (Username)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (forall a. Game.Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.mkTell $ Game.ReceiveMessage msg
  pure Nothing

-- A handler for messages from our component IO that sends them to the server
-- using the websocket
wsSender :: WS.WebSocket -> Game.Output -> Effect Unit
wsSender socket = case _ of
  Game.OutputMessage msgContents -> do
    WS.sendString socket msgContents

main :: Effect Unit
main = do
  connection <- WS.create "ws://localhost:5000" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    let rootComponent = runAppM Game.component
    io <- runUI rootComponent unit body

    -- Subscribe to all output messages from our component
    _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connection

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
