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


type alias Model =
    ( Bool, Board.Board )


init : ( Model, Cmd Msg )
init =
    ( ( False, Board.emptyBoard 13 600 )
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


update msg (( locking, board ) as model) =
    case msg of
        MouseMsg mousePos ->
            let
                adjusted =
                    { mousePos | x = mousePos.x - menuWidth }

                newBoard =
                    if locking then
                        Board.lockTile adjusted board
                    else
                        Board.rotateTile adjusted board
            in
                ( ( locking, newBoard ), Cmd.none )

        KeyboardMsg keyCode ->
            case Char.fromCode keyCode of
                'L' ->
                    ( ( not locking, board ), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLockMsg ->
            ( ( not locking, board ), Cmd.none )

        NewGameMsg ->
            ( model, Task.perform NewBoardMsg Time.now )

        NewBoardMsg time ->
            ( ( False
              , Board.generateBoard 13 board.renderSize <|
                    Random.initialSeed <|
                        round time
              )
            , Cmd.none
            )

        WindowSizeMsg size ->
            let
                newBoard =
                    Board.setRenderSize board (boardRenderSize size)
            in
                ( ( locking, newBoard ), Cmd.none )



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


view ( _, board ) =
    let
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
