module Net exposing (..)

import Board
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Random
import Task
import Time
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button


main =
    Browser.element
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


type alias WindowSize =
    { width : Int, height : Int }


type alias Model =
    { mode : GameMode
    , board : Board.Board
    , locking : Bool
    , gameTime : Int
    , newBoardSize : Int
    , newBoardWrapping : Bool
    , endGameModalState : Modal.Visibility
    , newGameModalState : Modal.Visibility
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mode = Init
      , board = Board.emptyBoard defaultBoardSize 600
      , locking = False
      , gameTime = 0
      , newBoardSize = defaultBoardSize
      , newBoardWrapping = False
      , endGameModalState = Modal.hidden
      , newGameModalState = Modal.hidden
      }
    , Task.perform
        (\vp ->
            WindowSizeMsg
                { width = round vp.scene.width
                , height = round vp.scene.height
                }
        )
        Browser.Dom.getViewport
    )



-- Update


type Msg
    = BoardClickMsg Int Int
    | KeyboardMsg String
    | ToggleLockMsg
    | NewGameMsg
    | NewBoardMsg Time.Posix
    | WindowSizeMsg WindowSize
    | TickMsg Time.Posix
    | BoardSizeMsg String
    | ToggleWrappingMsg
    | CloseEndGameModal
    | CloseNewGameModal
    | ShowNewGameModal


toggleLock : Model -> ( Model, Cmd Msg )
toggleLock model =
    ( { model | locking = not model.locking }, Cmd.none )


playingUpdate : Msg -> Model -> ( Model, Cmd Msg )
playingUpdate msg model =
    case msg of
        BoardClickMsg mouseX mouseY ->
            let
                adjusted =
                    { x = mouseX, y = mouseY }

                newBoard =
                    if model.locking then
                        Board.lockTile adjusted model.board

                    else
                        Board.rotateTile adjusted model.board
            in
            case newBoard.state of
                Board.Complete ->
                    ( { model
                        | board = newBoard
                        , mode = GameOver
                        , endGameModalState = Modal.shown
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | board = newBoard }, Cmd.none )

        KeyboardMsg key ->
            case key of
                "l" ->
                    toggleLock model

                "L" ->
                    toggleLock model

                _ ->
                    ( model, Cmd.none )

        ToggleLockMsg ->
            toggleLock model

        TickMsg _ ->
            ( { model | gameTime = model.gameTime + 1 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGameMsg ->
            ( { model
                | endGameModalState = Modal.hidden
                , newGameModalState = Modal.hidden
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
                    Random.initialSeed (Time.posixToMillis time)

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
                    Maybe.withDefault defaultBoardSize
                        (String.toInt sizeString)
              }
            , Cmd.none
            )

        ToggleWrappingMsg ->
            ( { model | newBoardWrapping = not model.newBoardWrapping }
            , Cmd.none
            )

        CloseEndGameModal ->
            ( { model | endGameModalState = Modal.hidden }, Cmd.none )

        CloseNewGameModal ->
            ( { model | newGameModalState = Modal.hidden }, Cmd.none )

        ShowNewGameModal ->
            ( { model | newGameModalState = Modal.shown }, Cmd.none )

        _ ->
            case model.mode of
                Init ->
                    ( model, Cmd.none )

                Playing ->
                    playingUpdate msg model

                GameOver ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.field "key" Decode.string |> Decode.map KeyboardMsg)
        , Browser.Events.onResize
            (\w h -> WindowSizeMsg { width = w, height = h })
        , Time.every 1000 TickMsg
        ]



-- View


boardRenderSize : WindowSize -> Int
boardRenderSize size =
    min (size.width - menuWidth) size.height


menuWidth : Int
menuWidth =
    200


menuColumn : List (Html.Attribute msg)
menuColumn =
    [ Html.Attributes.style "width" (String.fromInt menuWidth ++ "px")
    , Html.Attributes.style "padding" "10px"
    , Html.Attributes.style "float" "left"
    ]


menuHeader : List (Html.Attribute msg)
menuHeader =
    [ Html.Attributes.style "display" "block"
    , Html.Attributes.style "font-weight" "bold"
    , Html.Attributes.style "font-variant" "small-caps"
    , Html.Attributes.style "font-size" "1.5em"
    , Html.Attributes.style "margin-top" "1.5em"
    , Html.Attributes.style "color" "#025aa5"
    , Html.Attributes.style "font-family" "sans-serif"
    ]


menuContent : List (Html.Attribute msg)
menuContent =
    [ Html.Attributes.style "display" "block"
    , Html.Attributes.style "text-align" "right"
    , Html.Attributes.style "font-weight" "bold"
    , Html.Attributes.style "font-size" "2em"
    , Html.Attributes.style "line-height" "1em"
    , Html.Attributes.style "letter-spacing" "2px"
    , Html.Attributes.style "font-family" "sans-serif"
    ]


gameColumn : List (Html.Attribute Msg)
gameColumn =
    [ Html.Attributes.style "width" "auto"
    , Html.Attributes.style "margin-left" (String.fromInt menuWidth ++ "px")
    ]


newGameOption : List (Html.Attribute msg)
newGameOption =
    [ Html.Attributes.style "display" "block"
    , Html.Attributes.style "line-height" "3em"
    , Html.Attributes.style "font-size" "1.2em"
    , Html.Attributes.style "font-weight" "bold"
    , Html.Attributes.style "font-family" "sans-serif"
    ]


score : Model -> Int
score model =
    let
        excessMoves =
            max 0 (model.board.moves - model.board.minMoves)
    in
    (131 * model.board.size * model.board.moves // model.gameTime)
        // (excessMoves + 1)


scoreString : Model -> String
scoreString model =
    String.fromInt (score model)


gameTimeString : Model -> String
gameTimeString model =
    let
        minutes =
            model.gameTime // 60

        seconds =
            remainderBy 60 model.gameTime
    in
    [ minutes, seconds ]
        |> List.map String.fromInt
        |> List.map (String.padLeft 2 '0')
        |> String.join ":"


movesString : Model -> String
movesString model =
    [ model.board.moves, model.board.minMoves ]
        |> List.map String.fromInt
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
            Html.Attributes.value (String.fromInt n) :: extraAttrs n
    in
    List.range 3 13
        |> List.filter (\n -> modBy 2 n == 1)
        |> List.map
            (\n ->
                Html.option (attrs n) [ Html.text (String.fromInt n) ]
            )


endGameText : String -> String -> List (Html Msg)
endGameText label value =
    [ Html.span menuHeader [ Html.text label ]
    , Html.span menuContent [ Html.text value ]
    ]


endGameModal : Model -> Html Msg
endGameModal model =
    Modal.config CloseEndGameModal
        |> Modal.h4 [] [ Html.text "You Win!" ]
        |> Modal.body []
            [ Html.p [] (endGameText "Moves" (movesString model))
            , Html.p [] (endGameText "Time" (gameTimeString model))
            , Html.p [] (endGameText "Score" (scoreString model))
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.primary, Button.onClick NewGameMsg ]
                [ Html.text "Restart" ]
            ]
        |> Modal.view model.endGameModalState


newGameModal : Model -> Html Msg
newGameModal model =
    let
        body =
            [ Html.span newGameOption
                [ Html.text "Board Size "
                , Html.select [ Html.Events.onInput BoardSizeMsg ]
                    (boardSizeOptions model)
                ]
            , Html.span newGameOption
                [ Html.text "Wrapping "
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Events.onClick ToggleWrappingMsg
                    ]
                    []
                ]
            ]
    in
    Modal.config CloseNewGameModal
        |> Modal.small
        |> Modal.h4 [] [ Html.text "Start New Game" ]
        |> Modal.body [] body
        |> Modal.footer []
            [ Button.button
                [ Button.primary, Button.onClick NewGameMsg ]
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
        , Button.onClick ShowNewGameModal
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
                , Html.p []
                    [ Html.span menuHeader
                        [ Html.text "moves" ]
                    , Html.span menuContent
                        [ Html.text (movesString model) ]
                    , Html.span menuHeader
                        [ Html.text "time" ]
                    , Html.span menuContent
                        [ Html.text (gameTimeString model) ]
                    ]
                ]
    in
    Html.div menuColumn contents


boardClickDecoder : Decode.Decoder Msg
boardClickDecoder =
    Decode.map2 BoardClickMsg
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


view : Model -> Html Msg
view model =
    let
        board =
            model.board

        render =
            Board.renderBoard board
    in
    Html.div []
        [ menu model
        , Html.div
            (Html.Events.on "click" boardClickDecoder :: gameColumn)
            [ render ]
        , newGameModal model
        , endGameModal model
        ]
