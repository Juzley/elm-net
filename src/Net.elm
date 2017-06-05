import Html
import Html.Events
import Collage
import Element
import Mouse
import Board
import Random

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
init = (Board.makeBoard, Cmd.none)


-- Update

type Msg =
    MouseMsg Mouse.Position
  | NewGameMsg
  | GenerateMsg Board.GenerationInfo Int


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
      |> Board.updateConnections
      |> (\m -> (m, Cmd.none))

    NewGameMsg ->
      let (board, state, genInfo, randVal) = Board.startGenerate in
      case state of
        Board.Finished -> (board, Cmd.none)
        Board.InProgress ->
          (board, Random.generate (GenerateMsg genInfo) (Random.int 1 randVal))

    GenerateMsg genInfo randVal ->
      let
        (board, state, newGenInfo, newRandVal) =
          Board.contGenerate model genInfo randVal
      in
        case state of
          Board.Finished -> (board, Cmd.none)
          Board.InProgress ->
            (board, Random.generate (GenerateMsg newGenInfo) (Random.int 1 newRandVal))


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
