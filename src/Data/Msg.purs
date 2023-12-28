module Soltan.Data.Msg where

import Data.Argonaut
import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt, decodeNumber, decodeString, decodeTuple)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Soltan.Data.Card (Suit, Card)
import Soltan.Data.Game (Game)
import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username

type TableName = String
type TableId = Number

type TableSummary =
  { id :: TableId
  , users :: Array Username
  , disconnectedUsers :: Array Username
  }

data MsgOut
  = Login Username
  | GetTables
  | NewTable
  | JoinTableMsg TableId
  | ChooseHokmMsg TableId Suit
  | PlayCardMsg TableId Card

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
  encodeJson (JoinTableMsg tableName) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "JoinTable")
        , Tuple "contents" (Json.fromNumber tableName)
        ]
  encodeJson NewTable = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "NewTable")
        ]
  encodeJson (ChooseHokmMsg tableName suit) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "GameMsgIn")
        , Tuple "contents"
            ( Json.fromObject $ Object.fromFoldable
                [ Tuple "tag" (Json.fromString "ChooseHokmMsg")
                , Tuple "contents" (Json.fromArray [ Json.fromNumber tableName, encodeJson suit ])
                ]
            )
        ]
  encodeJson (PlayCardMsg tableName card) = Json.fromObject
    $ Object.fromFoldable
        [ Tuple "tag" (Json.fromString "GameMsgIn")
        , Tuple "contents"
            ( Json.fromObject $ Object.fromFoldable
                [ Tuple "tag" (Json.fromString "PlayCardMsg")
                , Tuple "contents" (Json.fromArray [ Json.fromNumber tableName, encodeJson card ])
                ]
            )
        ]

msgOutToString :: MsgOut -> String
msgOutToString msg = Json.stringify $ encodeJson msg

data MsgIn
  = AuthSuccess Username
  | TableList (Array TableSummary)
  | SuccessfullySubscribedToTable TableSummary
  | NewGameStateSummary TableId Game

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
        tableSummary <- decodeJson contents
        Right $ SuccessfullySubscribedToTable tableSummary
      "NewGameStateSummary" -> do
        contents <- x .: "contents"
        Tuple tableName game <- decodeTuple decodeNumber decodeJson contents
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

msgInParse :: String -> Either JsonDecodeError MsgIn
msgInParse s = parseJson s >>= decodeJson
