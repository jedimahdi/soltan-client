module Soltan.Component.Game where

import Prelude
import Soltan.Data.Game
import Soltan.Data.Msg

import Data.Array (replicate)
import Data.Either (Either(..), hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Soltan.Capability.LogMessages (class LogMessages, logError, logInfo)
import Soltan.Capability.Now (class Now)
import Soltan.Component.Assets.Images (cardLargeImage, cardMediumImage, cardSmallImage)
import Soltan.Component.HTML.Utils (css, whenElem)
import Soltan.Data.Card (Card, Suit(..))
import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

data Query a = ReceiveMessage String a

data Output = OutputMessage String

data State
  = GameLogin GameLoginState
  | GameLobby GameLobbyState
  | InGame InGameState

type InGameState =
  { username :: Username
  , game :: Game
  , table :: TableSummary
  }

data Future a = Loading | GotData a

type GameLobbyState =
  { username :: Username
  , tables :: Future (Array TableSummary)
  }

type GameLoginState =
  { usernameInput :: String
  }

data Action
  = Initialize
  | HandleUsernameInput String
  | Submit Event
  | JoinTable TableId
  | OnNewTable
  | ChooseHokm Suit
  | PlayCard Card

component
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => H.Component Query Unit Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState :: forall i. i -> State
  initialState _ = GameLogin { usernameInput: "" }

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction action = do
    state <- H.get
    case state, action of
      _, Initialize -> logInfo "Initialize"
      GameLogin s, HandleUsernameInput text -> H.put (GameLogin $ s { usernameInput = text })
      GameLogin s, Submit ev -> do
        H.liftEffect $ Event.preventDefault ev
        case Username.parse s.usernameInput of
          Nothing -> logError "Failed to parse Username"
          Just username -> do
            sendMsg $ Login username
            liftEffect $ HTML.window >>= Window.document >>= HTMLDocument.setTitle s.usernameInput
      GameLobby _, JoinTable tableId -> sendMsg $ SubscribeToTable tableId
      GameLobby _, OnNewTable -> sendMsg NewTable
      InGame { game: GameChoosingHokm g, table }, ChooseHokm suit
        | g.hakem == g.playerIndex -> sendMsg $ ChooseHokmMsg (table.id) suit
      InGame { game: GameInProgress g, table }, PlayCard card
        | g.turn == g.playerIndex -> sendMsg $ PlayCardMsg (table.id) card
      _, _ -> pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    ReceiveMessage msg a -> do
      logInfo $ "Received: " <> msg
      case msgInParse msg of
        Left e -> logError $ "Failed to parse MsgIn2: " <> show e
        Right m -> handleMsg m
      pure (Just a)

  handleMsg :: MsgIn -> H.HalogenM State _ _ Output m Unit
  handleMsg msg = do
    state <- H.get
    case state, msg of
      GameLogin _, AuthSuccess username -> do
        sendMsg $ GetTables
        H.put $ GameLobby { username, tables: Loading }
      GameLobby s, TableList tables -> H.put (GameLobby $ s { tables = GotData tables })
      GameLobby s, SuccessfullySubscribedToTable _ table -> H.put $ InGame { table, username: s.username, game: GameBeforeStart }
      InGame s, NewGameStateSummary _ game -> H.put (InGame $ s { game = game })
      _, _ -> pure unit

  sendMsg :: MsgOut -> H.HalogenM _ _ _ Output m Unit
  sendMsg msg = do
    logInfo $ "Sending: " <> msgOutToString msg
    H.raise <<< OutputMessage <<< msgOutToString $ msg

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state = HH.div [ css "container" ]
    [ case state of
        GameLogin s -> renderLogin s
        GameLobby s -> renderLobby s
        InGame s -> renderGame s
    ]

renderLogin :: forall w. GameLoginState -> HH.HTML w Action
renderLogin state =
  HH.div_
    [ HH.h1_ [ HH.text "Login" ]
    , HH.form
        [ HE.onSubmit Submit ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.value (state.usernameInput)
            , HE.onValueInput HandleUsernameInput
            ]
        , HH.button
            [ HP.type_ HP.ButtonSubmit ]
            [ HH.text "Login" ]
        ]
    ]

renderLobby :: forall w. GameLobbyState -> HH.HTML w Action
renderLobby state = case state.tables of
  Loading -> HH.text "Lobby Loading..."
  GotData tables -> HH.div_ $ [HH.button [HP.type_ HP.ButtonButton, HE.onClick (const OnNewTable)] [ HH.text "New Table"] , HH.div_ $ map renderTable tables]
  where
  renderTable :: forall p. TableSummary -> HH.HTML p Action
  renderTable table = HH.div_
    [ HH.text (show table.id <> " - " <> show table.users <> "/4")
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick (\_ -> JoinTable table.id) ] [ HH.text "Join" ]
    ]

renderScoreboard
  :: forall w i r
   . { yourTeamPoints :: Int
     , rivalTeamPoints :: Int
     , yourTeamTricks :: Int
     , rivalTeamTricks :: Int
     , trumpSuit :: Maybe Suit
     | r
     }
  -> HH.HTML w i
renderScoreboard { yourTeamPoints, rivalTeamPoints, yourTeamTricks, rivalTeamTricks, trumpSuit } = HH.div [ css "scoreboard" ]
  [ renderTeamScore yourTeamPoints yourTeamTricks false
  , HH.div [ css "hokm" ] [ HH.div_ [ HH.text $ fromMaybe "Choosing..." (map show trumpSuit) ], HH.div_ [ HH.text "Hokm" ] ]
  , renderTeamScore rivalTeamPoints rivalTeamTricks true

  ]
  where
  renderTeamScore points tricks isRivals = HH.div [ css ("team" <> guard isRivals " team-rivals") ]
    [ HH.div [ css "tricks" ]
        (replicate tricks (HH.div [ css "trick active" ] []) <> replicate (7 - tricks) (HH.div [ css "trick" ] []))
    , HH.div [ css "points" ]
        (replicate points (HH.div [ css "point active" ] []) <> replicate (7 - points) (HH.div [ css "point" ] []))
    , HH.div_ [ HH.text (if isRivals then "Rivals" else "Ours") ]
    ]

renderBoard :: forall w i r. { player1 :: Player, player2 :: Player, player3 :: Player, player4 :: Player | r } -> HH.HTML w i
renderBoard { player1, player2, player3, player4 } = HH.div [ css "board" ]
  [ renderPlayer player1 "player1"
  , renderPlayer player2 "player2"
  , renderPlayer player3 "player3"
  , renderPlayer player4 "player4"
  ]
  where
  renderPlayer p n = HH.div [ css $ "player " <> n <> guard (p.isTurnToPlay) " turn" ]
    [ HH.div [ css "player-info" ]
        [ HH.text $ Username.toString p.username, whenElem p.isHakem (\_ -> HH.text " (hakem)") ]
    , HH.div
        ( [ css "card" ]
            <> fromMaybe [] (p.playedCard <#> (\playedCard -> [ HP.style $ "background-image: url(" <> cardLargeImage playedCard <> ");" ]))
        )
        []
    ]

renderPlayerCards :: forall w r. { cards :: Array Card | r } -> HH.HTML w Action
renderPlayerCards { cards } = HH.div [ css "player-cards" ] (map renderCard cards)
  where
  renderCard card = HH.div [ css "card", HE.onClick (\_ -> PlayCard card) ] [ HH.img [ HP.src (cardMediumImage card) ] ]

renderGame :: forall w. InGameState -> HH.HTML w Action
renderGame state = case state.game of
  GameBeforeStart -> renderGameBeforeStart state
  GameChoosingHokm game -> renderGameChoosingHokm state game
  GameInProgress game -> renderGameInProgress state game
  GameEndOfTrick game -> renderGameEndOfTrick state game
  GameEnd game -> renderGameEnd state game

renderGameBeforeStart :: forall w. InGameState -> HH.HTML w Action
renderGameBeforeStart _ = HH.text "Not started"

renderGameChoosingHokm :: forall w. InGameState -> ChoosingHokmState -> HH.HTML w Action
renderGameChoosingHokm _ game = HH.div_
  [ renderScoreboard (Record.union game { rivalTeamTricks: 0, yourTeamTricks: 0, trumpSuit: Nothing })
  , if game.player1.isHakem then renderChooseHokm
    else renderWaitForHakem
  , renderPlayerCards game
  ]
  where
  renderChooseHokm = HH.div_
    [ renderSuit Hearts "Hearts"
    , renderSuit Spades "Spades"
    , renderSuit Diamonds "Diamonds"
    , renderSuit Clubs "Clubs"
    ]
  renderSuit suit name = HH.div [ HE.onClick (\_ -> ChooseHokm suit) ] [ HH.text name ]
  renderWaitForHakem = HH.div_ [ HH.text "Wait for Hakem..." ]

renderGameInProgress :: forall w. InGameState -> GameInProgressState -> HH.HTML w Action
renderGameInProgress _ game = HH.div_
  [ renderScoreboard (game { trumpSuit = Just game.trumpSuit })
  , renderBoard game
  , renderPlayerCards game
  ]

renderGameEndOfTrick :: forall w. InGameState -> GameEndOfTrickState -> HH.HTML w Action
renderGameEndOfTrick _ game = HH.div_ $
  [ renderScoreboard (game { trumpSuit = Just game.trumpSuit })
  , renderBoard game
  , renderPlayerCards game
  ]

renderGameEnd :: forall w. InGameState -> GameEndState -> HH.HTML w Action
renderGameEnd _ game = HH.div_ $
  [ renderScoreboard (Record.union game { rivalTeamTricks: 0, yourTeamTricks: 0, trumpSuit: Nothing })
  , HH.text $ "Winner is " <> show game.winnerTeam
  ]
