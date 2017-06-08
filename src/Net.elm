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
    ( ( False, Board.emptyBoard 5 ), Cmd.none )



-- Update


type Msg
    = MouseMsg Mouse.Position
    | KeyboardMsg Keyboard.KeyCode
    | ToggleLockMsg
    | NewGameMsg
    | NewBoardMsg Time.Time



-- TODO: Derive these from collage width and height, share with render.


tileSize =
    64


clickInfo : Mouse.Position -> ( Board.TilePos, Board.Rotation )
clickInfo mousePos =
    let
        tilePos =
            ( mousePos.x // tileSize, mousePos.y // tileSize )
    in
        case (mousePos.x // (tileSize // 2)) % 2 of
            0 ->
                ( tilePos, Board.RotateCW )

            _ ->
                ( tilePos, Board.RotateCCW )


update msg (( locking, board ) as model) =
    case msg of
        MouseMsg mousePos ->
            let
                ( tilePos, dir ) =
                    clickInfo mousePos

                newBoard =
                    if locking then
                        Board.lockTile tilePos board
                    else
                        Board.rotateTile tilePos dir board
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
            ( ( locking
              , Board.generateBoard 5 <|
                    Random.initialSeed <|
                        round
                            time
              )
            , Cmd.none
            )



-- Subscriptions


subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyboardMsg
        ]



-- View


container : List Style.Style
container =
    [ Style.height Style.auto
    , Style.overflow Style.hidden
    ]


menuColumn : List Style.Style
menuColumn =
    [ Style.width (Style.px 200)
    , Style.float Style.right_
    ]


gameColumn : List Style.Style
gameColumn =
    [ Style.width Style.auto
    , Style.overflow Style.hidden
    ]


collageWidth =
    600


collageHeight =
    600


view ( _, board ) =
    let
        render =
            Board.renderBoard board
                |> Collage.collage collageWidth collageHeight
                |> Element.toHtml
    in
        Html.div [ Html.Attributes.style container ]
            [ Html.div [ Html.Attributes.style menuColumn ]
                [ Html.button [ Html.Events.onClick NewGameMsg ] [ Html.text "New Game" ]
                , Html.button [ Html.Events.onClick ToggleLockMsg ] [ Html.text "Toggle Lock" ]
                ]
            , Html.div [ Html.Attributes.style gameColumn ] [ render ]
            ]
