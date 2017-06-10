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


defaultBoardSize =
    5


type GameMode
    = Init
    | Playing
    | GameOver


type alias Model =
    { mode : GameMode
    , board : Board.Board
    , locking : Bool
    , gameTime : Int
    , newBoardSize : Int
    , newBoardWrapping : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { mode = Init
      , board = Board.emptyBoard defaultBoardSize 600
      , locking = False
      , gameTime = 0
      , newBoardSize = defaultBoardSize
      , newBoardWrapping = False
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
    | TickMsg Time.Time
    | BoardSizeMsg String
    | ToggleWrappingMsg


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

        TickMsg time ->
            ( { model | gameTime = model.gameTime + 1 }, Cmd.none )

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
                seed =
                    Random.initialSeed (round time)

                newBoard =
                    Board.generateBoard model.newBoardSize
                        model.board.renderSize
                        seed
                        (not model.newBoardWrapping)
            in
                ( { model
                    | mode = Playing
                    , locking = False
                    , board = newBoard
                    , gameTime = 0
                  }
                , Cmd.none
                )

        BoardSizeMsg sizeString ->
            ( { model
                | newBoardSize =
                    Result.withDefault defaultBoardSize
                        (String.toInt
                            sizeString
                        )
              }
            , Cmd.none
            )

        ToggleWrappingMsg ->
            ( { model | newBoardWrapping = not model.newBoardWrapping }
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
        , Time.every Time.second TickMsg
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


gameTimeString : Model -> String
gameTimeString model =
    let
        hours =
            model.gameTime // 3600

        minutes =
            (rem model.gameTime 3600) // 60

        seconds =
            rem model.gameTime 60
    in
        [ hours, minutes, seconds ]
            |> List.map toString
            |> List.map (String.pad 2 '0')
            |> String.join ":"


movesString : Model -> String
movesString model =
    [ model.board.moves, model.board.minMoves ]
        |> List.map toString
        |> String.join "/"


boardSizeOptions : Model -> List (Html Msg)
boardSizeOptions model =
    let
        extraAttrs n =
            if n == model.newBoardSize then
                [ Html.Attributes.selected True ]
            else
                []

        attrs n =
            (Html.Attributes.value (toString n)) :: (extraAttrs n)
    in
        List.range 3 13
            |> List.filter (\n -> n % 2 == 1)
            |> List.map
                (\n ->
                    Html.option (attrs n) [ Html.text (toString n) ]
                )


checkbox : msg -> String -> Html msg
checkbox msg name =
    Html.label []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Events.onClick msg
            ]
            []
        , Html.text name
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
                [ Html.button [ Html.Events.onClick ToggleLockMsg ] [ Html.text "Toggle Lock" ]
                , Html.p [] [ Html.text (movesString model) ]
                , Html.p [] [ Html.text (gameTimeString model) ]
                , Html.button [ Html.Events.onClick NewGameMsg ] [ Html.text "New Game" ]
                , Html.select [ Html.Events.onInput BoardSizeMsg ] (boardSizeOptions model)
                , checkbox ToggleWrappingMsg "Wrapping"
                ]
            , Html.div [ Html.Attributes.style gameColumn ] [ render ]
            ]
