module Soltan.Component.Utils where

import Prelude

import Halogen as H
import Soltan.Data.Msg (MsgOut, msgOutToString)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot

data WebSocketOutput = OutputMessage String

sendMsg :: forall state action slots m. MsgOut -> H.HalogenM state action slots WebSocketOutput m Unit
sendMsg = H.raise <<< OutputMessage <<< msgOutToString
