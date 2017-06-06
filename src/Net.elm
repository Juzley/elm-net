import Html
import Html.Events
import Collage
import Element
import Mouse
import Board
import Random
import Task
import Time

main =
  Html.program
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }

-- Model

type alias Model = Board.Board

init : (Model, Cmd Msg)
init = (Board.emptyBoard 5, Cmd.none)


-- Update

type Msg =
    MouseMsg Mouse.Position
  | NewGameMsg
  | NewBoardMsg Time.Time


-- TODO: Derive these from collage width and height, share with render.
tileSize = 64


clickInfo : Mouse.Position -> (Board.TilePos, Board.Rotation)
clickInfo mousePos =
  let tilePos = (mousePos.x // tileSize, mousePos.y // tileSize) in
  case (mousePos.x // (tileSize // 2)) % 2 of
    0 -> (tilePos, Board.RotateCW)
    _ -> (tilePos, Board.RotateCCW)


updateRotation : Mouse.Position -> Model -> Model
updateRotation mousePos board = 
  let (tilePos, dir) = clickInfo mousePos in
    Board.rotateTile tilePos dir board


update msg model =
  case msg of
    MouseMsg mousePos ->
      updateRotation mousePos model
      |> (\m -> (m, Cmd.none))

    NewGameMsg ->
        (model, Task.perform NewBoardMsg Time.now)

    NewBoardMsg time ->
        (Board.generateBoard 5 <| Random.initialSeed <| round time, Cmd.none)


-- Subscriptions

subscriptions model =
  Mouse.clicks MouseMsg


-- View

collageWidth = 600
collageHeight = 600

view model =
  let 
    board =
      Board.renderBoard model
      |> Collage.collage collageWidth collageHeight
      |> Element.toHtml
  in
    Html.div [] [board, Html.button [Html.Events.onClick NewGameMsg] [Html.text "New Game"]]
