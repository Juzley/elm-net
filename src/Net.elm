module Net exposing (..)

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
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button


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
    , endGameModalState : Modal.State
    , newGameModalState : Modal.State
    }


init : ( Model, Cmd Msg )
init =
    ( { mode = Init
      , board = Board.emptyBoard defaultBoardSize 600
      , locking = False
      , gameTime = 0
      , newBoardSize = defaultBoardSize
      , newBoardWrapping = False
      , endGameModalState = Modal.hiddenState
      , newGameModalState = Modal.hiddenState
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
    | EndGameModalMsg Modal.State
    | NewGameModalMsg Modal.State


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
                case newBoard.state of
                    Board.Complete ->
                        ( { model
                            | board = newBoard
                            , mode = GameOver
                            , endGameModalState = Modal.visibleState
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( { model | board = newBoard }, Cmd.none )

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
            ( { model
                | endGameModalState = Modal.hiddenState
                , newGameModalState = Modal.hiddenState
              }
            , Task.perform NewBoardMsg Time.now
            )

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

        EndGameModalMsg state ->
            ( { model | endGameModalState = state }, Cmd.none )

        NewGameModalMsg state ->
            ( { model | newGameModalState = state }, Cmd.none )

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
    , Style.padding (Style.px 10)
    , Style.float Style.left_
    ]

menuItem : List Style.Style
menuItem =
    []

menuHeader : List Style.Style
menuHeader =
    [ Style.display Style.block
    , Style.fontWeight "bold"
    , Style.fontVariant Style.smallCaps
    , Style.fontSize (Style.em 1)
    , Style.marginTop (Style.em 2)
    , Style.letterSpacing (Style.px 1)
    , Style.color "blue"
    ]

menuContent : List Style.Style
menuContent =
    [ Style.display Style.block
    , Style.textAlign "right"
    , Style.fontWeight "bold"
    , Style.fontSize (Style.em 3)
    , Style.lineHeight (Style.em 1)
    , Style.letterSpacing (Style.px 2)
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
        minutes =
            model.gameTime // 60

        seconds =
            rem model.gameTime 60
    in
        [ minutes, seconds ]
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



-- TODO: Make this look better, calculate score


endGameModal : Model -> Html Msg
endGameModal model =
    Modal.config EndGameModalMsg
        |> Modal.h4 [] [ Html.text "You Win!" ]
        |> Modal.body []
            [ Html.p [] [ Html.text <| "Moves: " ++ movesString model ]
            , Html.p [] [ Html.text <| "Time: " ++ gameTimeString model ]
            , Html.p [] [ Html.text "Score: 0" ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.attrs [ Html.Events.onClick NewGameMsg ] ]
                [ Html.text "Restart" ]
            ]
        |> Modal.view model.endGameModalState


newGameModal : Model -> Html Msg
newGameModal model =
    Modal.config NewGameModalMsg
        |> Modal.h4 [] [ Html.text "Start New Game" ]
        |> Modal.body []
            [ Html.select [ Html.Events.onInput BoardSizeMsg ] (boardSizeOptions model)
            , checkbox ToggleWrappingMsg "Wrapping"
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.attrs [ Html.Events.onClick NewGameMsg ] ]
                [ Html.text "Start" ]
            ]
        |> Modal.view model.newGameModalState


lockButton : Model -> Html Msg
lockButton model =
    let
        attrs =
            if model.locking then
                Button.attrs [ Html.Attributes.class "active" ]
            else
                Button.attrs []
    in
        Button.button
            [ attrs, Button.primary, Button.block, Button.onClick ToggleLockMsg ]
            [ Html.text "Toggle Lock" ]


newGameButton : Html Msg
newGameButton =
    Button.button
        [ Button.primary
        , Button.block
        , Button.onClick (NewGameModalMsg Modal.visibleState)
        ]
        [ Html.text "New Game" ]


menu : Model -> Html Msg
menu model =
    let
        contents =
            if model.mode == Init then
                [ newGameButton ]
            else
                [ newGameButton
                , Html.hr [] []
                , lockButton model
                , Html.p [ Html.Attributes.style menuItem ]
                    [ Html.span [ Html.Attributes.style menuHeader ]
                        [ Html.text "Moves" ]
                    , Html.span [ Html.Attributes.style menuContent ]
                        [ Html.text (movesString model) ]
                    , Html.span [ Html.Attributes.style menuHeader ]
                        [Html.text "Time" ]
                    , Html.span [ Html.Attributes.style menuContent ]
                        [ Html.text (gameTimeString model) ]
                    ]
                ]
    in
        Html.div [ Html.Attributes.style menuColumn ] contents


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
            , menu model
            , Html.div [ Html.Attributes.style gameColumn ] [ render ]
            , newGameModal model
            , endGameModal model
            ]
