module Soltan.Data.Game where

import Prelude

import Data.Argonaut (class EncodeJson, JsonDecodeError(..), (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array as A
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Soltan.Data.Card (Card, Suit, cardCodec)
import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username

data Game
  = GameBeforeStart
  | GameChoosingHokm ChoosingHokmState
  | GameInProgress GameInProgressState
  | GameEndOfTrick GameEndOfTrickState
  | GameEnd GameEndState

instance decodeGame :: DecodeJson Game where
  decodeJson json = do
    x <- decodeJson json
    tag <- x .: "tag"
    case tag of
      "GameSummaryBeforeStart" -> do
        Right GameBeforeStart
      "GameSummaryChoosingHokm" -> do
        y <- decodeJson json
        Right $ GameChoosingHokm y
      "GameSummaryInProgress" -> do
        y <- decodeJson json
        Right $ GameInProgress y
      "GameSummaryEndOfTrick" -> do
        y <- decodeJson json
        Right $ GameEndOfTrick y
      "GameSummaryEnd" -> do
        y <- decodeJson json
        Right $ GameEnd y
      _ -> Left $ UnexpectedValue json

type ChoosingHokmState =
  { cards :: Array Card
  , hakem :: PlayerIndex
  , playerIndex :: PlayerIndex
  , player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
  , yourTeamPoints :: Int
  , rivalTeamPoints :: Int
  }

-- , players :: Array Player
-- , teamAPoints :: Int
-- , teamBPoints :: Int

type GameInProgressState =
  { cards :: Array Card
  , hakem :: PlayerIndex
  , playerIndex :: PlayerIndex
  , turn :: PlayerIndex
  , trumpSuit :: Suit
  , player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
  , yourTeamTricks :: Int
  , rivalTeamTricks :: Int
  , yourTeamPoints :: Int
  , rivalTeamPoints :: Int
  }

-- , players :: Array PlayerSummary
-- , teamATricks :: Int
-- , teamBTricks :: Int
-- , teamAPoints :: Int
-- , teamBPoints :: Int

type GameEndOfTrickState =
  { cards :: Array Card
  , hakem :: PlayerIndex
  , playerIndex :: PlayerIndex
  , trumpSuit :: Suit
  , player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
  , yourTeamTricks :: Int
  , rivalTeamTricks :: Int
  , yourTeamPoints :: Int
  , rivalTeamPoints :: Int
  }

type GameEndState =
  { playerIndex :: PlayerIndex
  , winnerTeam :: Team
  , areYouWinner :: Boolean
  , player1 :: PlayerSummary -- You
  , player2 :: PlayerSummary
  , player3 :: PlayerSummary
  , player4 :: PlayerSummary
  , yourTeamPoints :: Int
  , rivalTeamPoints :: Int
  }

-- getPlayer :: PlayerIndex -> Game -> Maybe Player
-- getPlayer idx game =
--   case game of
--     GameBeforeStart -> Nothing
--     GameChoosingHokm g -> A.find (\p -> p.idx == idx) g.players
--     GameInProgress g -> A.find (\p -> p.idx == idx) g.players
--     GameEndOfTrick g -> A.find (\p -> p.idx == idx) g.players
--     GameEnd g -> A.find (\p -> p.idx == idx) g.players

-- getPlayerTeam :: PlayerIndex -> Game -> Maybe Team
-- getPlayerTeam idx game = (\p -> p.team) <$> getPlayer idx game
--
-- getTeamsPoints :: Game -> Tuple Int Int
-- getTeamsPoints GameBeforeStart = Tuple 0 0
-- getTeamsPoints (GameChoosingHokm g) = Tuple g.teamAPoints g.teamBPoints
-- getTeamsPoints (GameInProgress g) = Tuple g.teamAPoints g.teamBPoints
-- getTeamsPoints (GameEndOfTrick g) = Tuple g.teamAPoints g.teamBPoints
-- getTeamsPoints (GameEnd g) = Tuple g.teamAPoints g.teamBPoints
--
-- getTeamsTricks :: Game -> Tuple Int Int
-- getTeamsTricks GameBeforeStart = Tuple 0 0
-- getTeamsTricks (GameChoosingHokm _) = Tuple 0 0
-- getTeamsTricks (GameInProgress g) = Tuple g.teamATricks g.teamBTricks
-- getTeamsTricks (GameEndOfTrick g) = Tuple g.teamATricks g.teamBTricks
-- getTeamsTricks (GameEnd _) = Tuple 0 0

type Player =
  { username :: Username
  , team :: Team
  , idx :: PlayerIndex
  , isHakem :: Boolean
  , playedCard :: Maybe Card
  , isTurnToPlay :: Boolean
  }

type PlayedCard =
  { card :: Card
  , playerIndex :: PlayerIndex
  }

data GameSummaryStatus
  = SummaryNotStarted
  | SummaryChoosingHokm
  | SummaryInProgress
  | SummaryEndOfTrick
  | SummaryEndOfGame

instance decodeGameSummaryStatus :: DecodeJson GameSummaryStatus where
  decodeJson json = do
    s <- decodeString json
    case s of
      "SummaryNotStarted" -> Right SummaryNotStarted
      "SummaryChoosingHokm" -> Right SummaryChoosingHokm
      "SummaryInProgress" -> Right SummaryInProgress
      "SummaryEndOfTrick" -> Right SummaryEndOfTrick
      "SummaryEndOfGame" -> Right SummaryEndOfGame
      _ -> Left $ UnexpectedValue json

data Team = A | B

instance showTeam :: Show Team where
  show A = "A"
  show B = "B"

instance decodeTeam :: DecodeJson Team where
  decodeJson json = do
    s <- decodeString json
    case s of
      "A" -> Right A
      "B" -> Right B
      _ -> Left $ UnexpectedValue json

data PlayerIndex = Player1 | Player2 | Player3 | Player4

derive instance eqPlayerIndex :: Eq PlayerIndex

instance showPlayerIndex :: Show PlayerIndex where
  show Player1 = "Player1"
  show Player2 = "Player2"
  show Player3 = "Player3"
  show Player4 = "Player4"

instance decodePlayerIndex :: DecodeJson PlayerIndex where
  decodeJson json = do
    s <- decodeString json
    case s of
      "Player1" -> Right Player1
      "Player2" -> Right Player2
      "Player3" -> Right Player3
      "Player4" -> Right Player4
      _ -> Left $ UnexpectedValue json

nextPlayerIndex :: PlayerIndex -> PlayerIndex
nextPlayerIndex Player1 = Player2
nextPlayerIndex Player2 = Player3
nextPlayerIndex Player3 = Player4
nextPlayerIndex Player4 = Player1

type PlayerSummary =
  { username :: Username
  , team :: Team
  , idx :: PlayerIndex
  }

type GameSummary =
  { status :: GameSummaryStatus
  , cards :: Array Card
  , players :: Array PlayerSummary
  , hakem :: Maybe Username
  , turn :: Maybe Username
  , trumpSuit :: Maybe Suit
  , board :: Array PlayedCard
  , teamATricks :: Int
  , teamBTricks :: Int
  , teamAPoints :: Int
  , teamBPoints :: Int
  }
