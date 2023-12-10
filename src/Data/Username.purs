module Soltan.Data.Username
  ( Username -- constructor not exported
  , parse
  , toString
  , codec
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

instance decodeUsername :: DecodeJson Username where
  decodeJson json = do
    s <- decodeString json
    case parse s of
      Just username -> Right username
      Nothing -> Left $ UnexpectedValue json

instance showUsername :: Show Username where
    show = toString

codec :: JsonCodec Username
codec = dimap (\(Username user) -> user) Username CA.string

parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
