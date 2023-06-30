module Soltan.Component.Login where

import Prelude
import Soltan.Data.Msg

import Data.Argonaut.Core as Json
import Data.Argonaut.Parser as Json
import Data.Array as A
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Soltan.Capability.LogMessages (class LogMessages, logError, logInfo)
import Soltan.Capability.Now (class Now)
import Soltan.Component.Utils (WebSocketOutput, sendMsg)
import Soltan.Data.Game (GameSummary, GameSummaryStatus(..))
import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

data Action
  = Submit Event
  | HandleUsernameInput String

type State =
  { usernameInput :: String
  }

initialState :: State
initialState = { usernameInput: "" }

handleAction :: forall action slots m. MonadEffect m => Now m => LogMessages m => Action -> H.HalogenM State action slots WebSocketOutput m Unit
handleAction = case _ of
  HandleUsernameInput text -> do
    H.modify_ _ { usernameInput = text }
  Submit ev -> do
    liftEffect $ Event.preventDefault ev
    state <- H.get
    case Username.parse state.usernameInput of
      Nothing -> logError "Failed to parse Username"
      Just username -> do
        sendMsg $ Login username
        liftEffect $ HTML.window >>= Window.document >>= HTMLDocument.setTitle state.usernameInput
