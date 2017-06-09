module Main exposing (..)

import Style
import Html exposing (..)
import Html.Events
import Html.Attributes
import Collage
import Element
import Mouse
import Board
import Random
import Task
import Time
import Keyboard
import Char
import Window


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type GameMode
    = Init
    | Playing
    | GameOver


type alias Model =
    { mode : GameMode
    , board : Board.Board
    , locking : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { mode = Init
      , board = Board.emptyBoard 5 600
      , locking = False
      }
    , Task.perform WindowSizeMsg Window.size
    )



-- Update


type Msg
    = MouseMsg Mouse.Position
    | KeyboardMsg Keyboard.KeyCode
    | ToggleLockMsg
    | NewGameMsg
    | NewBoardMsg Time.Time
    | WindowSizeMsg Window.Size


toggleLock : Model -> ( Model, Cmd Msg )
toggleLock model =
    ( { model | locking = not model.locking }, Cmd.none )


playingUpdate : Msg -> Model -> ( Model, Cmd Msg )
playingUpdate msg model =
    case msg of
        MouseMsg mousePos ->
            let
                adjusted =
                    { mousePos | x = mousePos.x - menuWidth }

                newBoard =
                    if model.locking then
                        Board.lockTile adjusted model.board
                    else
                        Board.rotateTile adjusted model.board

                mode =
                    if newBoard.state == Board.Complete then
                        GameOver
                    else
                        Playing
            in
                ( { model | board = newBoard, mode = mode }, Cmd.none )

        KeyboardMsg keyCode ->
            case Char.fromCode keyCode of
                'L' ->
                    toggleLock model

                _ ->
                    ( model, Cmd.none )

        ToggleLockMsg ->
            toggleLock model

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGameMsg ->
            ( model, Task.perform NewBoardMsg Time.now )

        WindowSizeMsg size ->
            let
                newBoard =
                    Board.setRenderSize model.board (boardRenderSize size)
            in
                ( { model | board = newBoard }, Cmd.none )

        NewBoardMsg time ->
            let
                newBoard =
                    Random.initialSeed (round time)
                        |> Board.generateBoard 13 model.board.renderSize
            in
                ( { model
                    | mode = Playing
                    , locking = False
                    , board = newBoard
                  }
                , Cmd.none
                )

        _ ->
            case model.mode of
                Init ->
                    ( model, Cmd.none )

                Playing ->
                    playingUpdate msg model

                GameOver ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyboardMsg
        , Window.resizes WindowSizeMsg
        ]



-- View


boardRenderSize : Window.Size -> Int
boardRenderSize size =
    min (size.width - menuWidth) size.height


menuWidth : Int
menuWidth =
    200


container : List Style.Style
container =
    []


menuColumn : List Style.Style
menuColumn =
    [ Style.width (Style.px menuWidth)
    , Style.float Style.left_
    ]


gameColumn : List Style.Style
gameColumn =
    [ Style.width Style.auto
    , Style.alignItems Style.left_
    , Style.marginLeft (Style.px menuWidth)
    ]


view : Model -> Html Msg
view model =
    let
        board =
            model.board

        render =
            Board.renderBoard board
                |> Collage.collage board.renderSize board.renderSize
                |> Element.toHtml
    in
        Html.div [ Html.Attributes.style container ]
            [ Html.div [ Html.Attributes.style menuColumn ]
                [ Html.button [ Html.Events.onClick NewGameMsg ] [ Html.text "New Game" ]
                , Html.button [ Html.Events.onClick ToggleLockMsg ] [ Html.text "Toggle Lock" ]
                ]
            , Html.div [ Html.Attributes.style gameColumn ] [ render ]
            ]
