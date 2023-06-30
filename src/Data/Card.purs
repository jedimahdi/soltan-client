module Soltan.Data.Card where

import Prelude

import Data.Argonaut (class EncodeJson, JsonDecodeError(..), (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type Card =
  { suit :: Suit
  , rank :: Rank
  }

cardCodec :: JsonCodec Card
cardCodec =
  CAR.object "Card"
    { suit: suitCodec
    , rank: valueCodec
    }

type Deck = Array Card

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

derive instance genericRoomStatus :: Generic Rank _
instance showRoomStatus :: Show Rank where
  show = genericShow

valueCodec :: JsonCodec Rank
valueCodec = CA.prismaticCodec "CardRank" dec enc CA.string
  where
  dec = case _ of
    "Two" -> Just Two
    "Three" -> Just Three
    "Four" -> Just Four
    "Five" -> Just Five
    "Six" -> Just Six
    "Seven" -> Just Seven
    "Eight" -> Just Eight
    "Nine" -> Just Nine
    "Ten" -> Just Ten
    "Jack" -> Just Jack
    "Queen" -> Just Queen
    "King" -> Just King
    "Ace" -> Just Ace
    _ -> Nothing
  enc = case _ of
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"
    Ace -> "Ace"

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades

instance showSuit :: Show Suit where
  show Clubs = "Clubs"
  show Diamonds = "Diamonds"
  show Hearts = "Hearts"
  show Spades = "Spades"

suitCodec :: JsonCodec Suit
suitCodec = CA.prismaticCodec "CardSuit" dec enc CA.string
  where
  dec = case _ of
    "Clubs" -> Just Clubs
    "Diamonds" -> Just Diamonds
    "Hearts" -> Just Hearts
    "Spades" -> Just Spades
    _ -> Nothing
  enc = case _ of
    Clubs -> "Clubs"
    Diamonds -> "Diamonds"
    Hearts -> "Hearts"
    Spades -> "Spades"

instance encodeRank :: EncodeJson Rank where
  encodeJson v = encodeString $ show v

instance decodeRank :: DecodeJson Rank where
  decodeJson json = do
    s <- decodeString json
    case s of
      "Two" -> Right Two
      "Three" -> Right Three
      "Four" -> Right Four
      "Five" -> Right Five
      "Six" -> Right Six
      "Seven" -> Right Seven
      "Eight" -> Right Eight
      "Nine" -> Right Nine
      "Ten" -> Right Ten
      "Jack" -> Right Jack
      "Queen" -> Right Queen
      "King" -> Right King
      "Ace" -> Right Ace
      _ -> Left $ UnexpectedValue json

instance encodeSuit :: EncodeJson Suit where
  encodeJson Clubs = encodeString "Clubs"
  encodeJson Diamonds = encodeString "Diamonds"
  encodeJson Hearts = encodeString "Hearts"
  encodeJson Spades = encodeString "Spades"

instance decodeSuit :: DecodeJson Suit where
  decodeJson json = do
    s <- decodeString json
    case s of
      "Clubs" -> Right Clubs
      "Diamonds" -> Right Diamonds
      "Hearts" -> Right Hearts
      "Spades" -> Right Spades
      _ -> Left $ UnexpectedValue json

-- instance encodeCard :: EncodeJson Card where
--   encodeJson (Card { suit, value }) =
--     "suit" := suit
--       ~> "value" := value
--
-- instance decodeCard :: DecodeJson Card where
--   decodeJson json = do
--     x <- decodeJson json
--     suit <- x .: "suit"
--     value <- x .: "value"
--     pure $ Card { suit, value }
