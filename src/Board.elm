module Board exposing
  ( Board, Tile, TilePos
  , TileType(..), Rotation(..), GenerationState(..), GenerationInfo
  , startGenerate, contGenerate, makeBoard, rotateTile, updateConnections
  , renderBoard
  )


import List
import Dict
import Set
import Color
import Collage
import Util exposing (..)


-- TYPES

type GenerationState = InProgress | Finished

type alias GenerationInfo = List Int

type Direction = Up | Right | Down | Left

type Rotation = RotateCW | RotateCCW

type alias TilePos = (Int, Int)

type TileType = Source | Normal

type alias Connections = List Bool

type alias Barriers = List Bool

type alias Tile = 
  {
    x: Int,
    y: Int,
    tileType: TileType,
    connected: Bool,
    connections: Connections,
    barriers: Barriers
  }

type alias Board = 
  {
    size: Int,
    tiles: Dict.Dict TilePos Tile 
  }


-- MISC

{-| Get the tile at the given board coordinates. -}
getTile : Board -> TilePos -> Maybe Tile
getTile board pos =
  Dict.get pos board.tiles

{-| Create a new board -}
makeBoard : Board
makeBoard =
  Board 3 (Dict.fromList
    [((0, 0), (Tile 0 0 Normal False [True, False, False, False] [False, False, False, False])),
     ((1, 0), (Tile 1 0 Normal False [True, False, True, False] [False, False, False, False])),
     ((2, 0), (Tile 2 0 Normal False [True, True, False, False] [False, False, False, False])),
     ((0, 1), (Tile 0 1 Normal False [True, False, False, False] [False, False, False, False])),
     ((1, 1), (Tile 1 1 Source True [True, False, False, False] [True, False, False, False])),
     ((2, 1), (Tile 2 1 Normal False [False, True, False, True] [False, False, False, False])),
     ((0, 2), (Tile 0 2 Normal False [True, True, False, False] [False, False, False, False])),
     ((1, 2), (Tile 1 2 Normal False [True, True, True, False] [False, False, False, False])),
     ((2, 2), (Tile 2 2 Normal False [True, True, False, False] [False, False, False, False]))])
  |> updateConnections


-- INPUT HANDLING

{-| Rotate the tlie at the given board coordinates in the given direction -}
rotateTile : TilePos -> Rotation -> Board -> Board
rotateTile pos dir board =
  let
    rotateFn = (\tile -> case (tile, dir) of
      (Just t, RotateCW) ->
        Just { t | connections = rotateList 1 t.connections }

      (Just t, RotateCCW) ->
        Just { t | connections = rotateList -1 t.connections }

      (Nothing, _) -> Nothing)
  in
    { board | tiles = Dict.update pos rotateFn board.tiles }


-- CONNECTION HANDLING

{-| Recalculate which tiles are connected to the source -}
updateConnections : Board -> Board
updateConnections board =
  board
  |> resetConnections
  |> updateConnectionsHelper [sourceTile board] Set.empty


{-| Get a list of the neighbours of a given tile, in the format
    [up, right, down, left]
-}
tileNeighbours : Maybe Tile -> Board -> List (Maybe Tile)
tileNeighbours tile board =
  let
    coords = case tile of
      Just t  -> [(t.x, t.y - 1), (t.x + 1, t.y),
                  (t.x, t.y + 1), (t.x - 1, t.y)]
      Nothing -> []
    wrap = (\(a, b) -> (a % board.size, b % board.size))
  in
    coords
    |> List.map wrap 
    |> List.map (getTile board)


{-| Get a list of valid connections for a tile, in the form of a list of
    bools indicating whether a connection in a certain direction exists.
    The order is [up, right, down, left]
-}
tileConnections : Tile -> List Bool
tileConnections  tile =
  List.map2 (\b c -> c && not b) tile.barriers tile.connections


{-| Determine whether a tile has a valid connection in a given diretion. -}
tileHasConnection : Maybe Tile -> Direction -> Bool
tileHasConnection tile dir =
  case tile of
  Just t ->
    tileConnections t
    |> zipList [Up, Right, Down, Left]
    |> List.filter (\(d, c) -> c && d == dir)
    |> (\l -> List.length l > 0)
  Nothing -> False


{-| Get the opposite direction for a given direction. -}
oppositeDir : Direction -> Direction
oppositeDir dir =
  case dir of
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left


{-| Determine whether two tiles are connected. -}
tilesConnected : Maybe Tile -> Direction -> Maybe Tile -> Bool
tilesConnected src dir dst =
    (tileHasConnection src dir) && (tileHasConnection dst (oppositeDir dir))


{-| Get a list of tiles which are connected to the given tile. -}
tileConnectedNeighbours : Maybe Tile -> Board -> List (Maybe Tile)
tileConnectedNeighbours tile board =
  tileNeighbours tile board
  |> zipList [Up, Right, Down, Left]
  |> List.filter (\(dir, neighbour) -> tilesConnected tile dir neighbour)
  |> List.map (\(d, t) -> t)


{-| Mark a given tile as connected. -}
markTileConnected : Maybe Tile -> Maybe Tile
markTileConnected tile =
  case tile of
    Just t -> Just { t | connected = True }
    Nothing -> Nothing


{-| Update the board to mark a given tile as connected. -}
markConnected : Maybe Tile -> Board -> Board
markConnected tile board =
  case tile of
    Just t ->
      { board |
          tiles = Dict.update (t.x, t.y) markTileConnected board.tiles }
    Nothing -> board


{-| Function used to filter out visited tile from a list. -}
visitedFilter : Set.Set TilePos -> Maybe Tile -> Bool
visitedFilter visited tile =
  case tile of
    Just t -> not (Set.member (t.x, t.y) visited)
    Nothing -> False


{-| Add a given tile to the visited set. -}
updateVisited : Maybe Tile -> Set.Set TilePos -> Set.Set TilePos
updateVisited tile visited =
  case tile of
    Just t -> Set.insert (t.x, t.y) visited
    Nothing -> visited


{-| Recursive helper function used by updateConnection. Maintains a queue
    of tiles to visit next, and a set of tiles already visited.
-}
updateConnectionsHelper : List (Maybe Tile) -> Set.Set TilePos -> Board -> Board
updateConnectionsHelper queue visited board =
  case queue of
    (tile::tiles) ->
      let
        filterFn = visitedFilter visited
        neighbours = List.filter filterFn (tileConnectedNeighbours tile board)
        newVisited = updateVisited tile visited
      in
        markConnected tile board
        |> updateConnectionsHelper (neighbours ++ tiles) newVisited
    [] -> board


{-| Get the 'source' tile for a given board -}
sourceTile : Board -> Maybe Tile
sourceTile board =
  let centerPos = (board.size // 2, board.size // 2) in
  getTile board centerPos


{-| Mark all tiles as not connected -}
resetConnections : Board -> Board
resetConnections board = 
  { board | tiles = 
      Dict.map (\_ t -> { t | connected = False }) board.tiles }


-- BOARD GENERATION

startGenerate : (Board, GenerationState, GenerationInfo, Int)
startGenerate =
  (makeBoard, InProgress, [], 4)

contGenerate : Board -> List a -> Int -> (Board, GenerationState, GenerationInfo, Int)
contGenerate board genInfo randVal =
  let _ = Debug.log "rand" randVal in
  (board, Finished, [], 0)
  

-- BOARD RENDERING

-- TODO: Derive these from collage width and height
tileSize = 64
lineWidth = 4
collageTop = 300
collageLeft = -300


tileCoordsToCollagePosition : TilePos -> (Float, Float)
tileCoordsToCollagePosition (tileX, tileY) =
  let
    top = collageTop - tileY * tileSize
    left = collageLeft + tileX * tileSize
    centerY = top - tileSize // 2
    centerX = left + tileSize // 2
  in 
    (toFloat(centerX), toFloat(centerY))


connectionColor : Tile -> Color.Color
connectionColor tile =
  if tile.connected then
    Color.green
  else
    Color.white


renderConnection : Tile -> (Float, Float) -> Float-> Collage.Form
renderConnection tile translation rotation =
  let
    color = connectionColor tile
    form = Collage.filled color (Collage.rect lineWidth (tileSize / 2))
  in
    form
    |> Collage.move translation
    |> Collage.rotate (degrees rotation)


renderUpConnection : Tile -> Collage.Form
renderUpConnection tile =
  renderConnection tile (0, tileSize / 4) 0


renderRightConnection : Tile -> Collage.Form
renderRightConnection tile = 
    renderConnection tile (tileSize / 4, 0) 90


renderDownConnection : Tile -> Collage.Form
renderDownConnection tile =
  renderConnection tile (0, -tileSize / 4) 0


renderLeftConnection : Tile -> Collage.Form
renderLeftConnection tile =
  renderConnection tile (-tileSize / 4, 0) 90


renderConnections : Tile -> List Collage.Form
renderConnections tile =
  let
    renderFns = [renderUpConnection, renderRightConnection,
                 renderDownConnection, renderLeftConnection]
  in
    zipList renderFns tile.connections
    |> List.filter (\(r, c) -> c)
    |> List.map (\(r, c) -> r tile)


renderForeground : Tile -> List Collage.Form
renderForeground tile =
  case tile.tileType of
    Source ->
      [Collage.filled Color.grey (Collage.square (tileSize / 1.5))]
      
    Normal ->
      let conCount =
        tile.connections |> List.map (\c -> if c then 1 else 0) |> List.sum in
      if conCount == 1 then
        [Collage.filled (connectionColor tile) (Collage.circle (tileSize / 4))]
      else
        []


renderBackground : Tile -> List Collage.Form
renderBackground tile =
  [Collage.filled Color.black (Collage.square tileSize),
   Collage.filled Color.blue (Collage.square (tileSize * 0.99))]


renderTile : (TilePos, Tile) -> Collage.Form
renderTile (pos, tile) = 
  let
    background = renderBackground tile
    connections = renderConnections tile
    barriers = renderBarriers (pos, tile)
    foreground = renderForeground tile
    group = Collage.group (background ++ connections ++ foreground)
    move = Collage.move (tileCoordsToCollagePosition pos)
  in
    move group


barrierRenderFunctions : List Collage.Form
barrierRenderFunctions =
  let line = Collage.filled Color.darkBlue (Collage.rect (lineWidth * 2) tileSize) in
  [Collage.move (0, tileSize / 2) (Collage.rotate (degrees 90) line),
   Collage.move (tileSize / 2, 0) line,
   Collage.move (0, -tileSize / 2) (Collage.rotate (degrees 90) line),
   Collage.move (-tileSize / 2, 0) line]


renderBarriers : (TilePos, Tile) -> Collage.Form
renderBarriers (pos, tile) =
  let
    zipped = List.map2 (,) tile.barriers barrierRenderFunctions
    filtered = List.map Tuple.second (List.filter Tuple.first zipped)
    move = Collage.move (tileCoordsToCollagePosition pos)
    group = Collage.group filtered
  in
    move group


renderBoard : Board -> List Collage.Form
renderBoard board =
  let
    tiles = Dict.toList board.tiles
    tileRenders = List.map renderTile tiles
    barrierRenders = List.map renderBarriers tiles
  in
    tileRenders ++ barrierRenders
