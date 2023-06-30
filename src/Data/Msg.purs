module Soltan.Data.Msg where

import Data.Argonaut
import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Decoders (decodeArray, decodeString, decodeTuple)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Soltan.Data.Card (Suit, Card)
import Soltan.Data.Game (Game, GameSummary)
import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username

type TableName = String

type TableSummary =
  { tableName :: TableName
  , playerCount :: Int
  }

data MsgOut
  = Login Username
  | GetTables
  | SubscribeToTable TableName
  | ChooseHokmMsg TableName Suit
  | PlayCardMsg TableName Card

instance encodeMsgOut :: EncodeJson MsgOut where
  encodeJson (Login username) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "Login")
        , Tuple "contents" (Json.fromString (Username.toString username))
        ]
  encodeJson GetTables = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "GetTables")
        ]
  encodeJson (SubscribeToTable tableName) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "SubscribeToTable")
        , Tuple "contents" (Json.fromString tableName)
        ]
  encodeJson (ChooseHokmMsg tableName suit) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "GameMsgIn")
        , Tuple "contents"
            ( Json.fromObject $ Object.fromFoldable
                [ Tuple "tag" (Json.fromString "ChooseHokmMsg")
                , Tuple "contents" (Json.fromArray [ Json.fromString tableName, encodeJson suit ])
                ]
            )
        ]
  encodeJson (PlayCardMsg tableName card) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "GameMsgIn")
        , Tuple "contents"
            ( Json.fromObject $ Object.fromFoldable
                [ Tuple "tag" (Json.fromString "PlayCardMsg")
                , Tuple "contents" (Json.fromArray [ Json.fromString tableName, encodeJson card ])
                ]
            )
        ]

msgOutToString :: MsgOut -> String
msgOutToString msg = Json.stringify $ encodeJson msg

data MsgIn
  = AuthSuccess Username
  | TableList (Array TableSummary)
  | SuccessfullySubscribedToTable TableName TableSummary
  | NewGameStateSummary TableName Game

instance decodeMsgIn :: DecodeJson MsgIn where
  decodeJson json = do
    x <- decodeJson json
    tag <- x .: "tag"
    case tag of
      "AuthSuccess" -> do
        contents <- x .: "contents"
        Right $ AuthSuccess contents
      "TableList" -> do
        contents <- x .: "contents"
        Right $ TableList contents
      "SuccessfullySubscribedToTable" -> do
        contents <- x .: "contents"
        Tuple tableName tableSummary <- decodeTuple decodeString decodeJson contents
        Right $ SuccessfullySubscribedToTable tableName tableSummary
      "NewGameStateSummary" -> do
        contents <- x .: "contents"
        Tuple tableName game <- decodeTuple decodeString decodeJson contents
        Right $ NewGameStateSummary tableName game
      _ -> Left $ UnexpectedValue json

data Triple a b c = Triple a b c

decodeTriple
  :: forall a b c
   . (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> (Json -> Either JsonDecodeError c)
  -> Json
  -> Either JsonDecodeError (Triple a b c)
decodeTriple decoderA decoderB decoderC json = decodeArray Right json >>= f
  where
  f :: Array Json -> Either JsonDecodeError (Triple a b c)
  f = case _ of
    [ a, b, c ] -> Triple <$> decoderA a <*> decoderB b <*> decoderC c
    _ -> Left $ TypeMismatch "Triple"

msgInParse :: String -> Maybe MsgIn
msgInParse s = case parseJson s of
  Left _ -> Nothing
  Right json -> case decodeJson json of
    Left _ -> Nothing
    Right msg -> Just msg
