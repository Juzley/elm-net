module Board
    exposing
        ( Board
        , BoardState(..)
        , HoverSide(..)
        , Position
        , Tile
        , TileType(..)
        , emptyBoard
        , generateBoard
        , lockTile
        , renderBoard
        , rotateTile
        , setRenderSize
        )

import Dict
import List
import Random
import Set
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (..)



-- TYPES


type Direction
    = Up
    | Right
    | Down
    | Left


type Rotation
    = RotateCW
    | RotateCCW


type HoverSide
    = LeftSide
    | RightSide


type BoardState
    = Complete
    | Incomplete


type alias TilePos =
    ( Int, Int )


type TileType
    = Source
    | Normal


type alias Connections =
    List Bool


type alias Barriers =
    List Bool


type alias Tile =
    { x : Int
    , y : Int
    , tileType : TileType
    , connected : Bool
    , connections : Connections
    , barriers : Barriers
    , locked : Bool
    }


type alias TileDict =
    Dict.Dict TilePos Tile


type alias Board =
    { size : Int
    , tiles : TileDict
    , state : BoardState
    , moves : Int
    , minMoves : Int
    , lastMove : Maybe TilePos
    , renderSize : Int
    }


type alias Position =
    { x : Int, y : Int }



-- MISC


{-| Get the tile at the given board coordinates.
-}
getTile : Board -> TilePos -> Maybe Tile
getTile board pos =
    Dict.get pos board.tiles



-- INPUT HANDLING


{-| Determine which tile a click has hit.
-}
clickInfo : Position -> Board -> Maybe ( TilePos, Rotation )
clickInfo mousePos board =
    let
        size =
            tileSizeInt board

        tilePos =
            ( mousePos.x // size, mousePos.y // size )

        info =
            case modBy 2 (mousePos.x // (size // 2)) of
                0 ->
                    ( tilePos, RotateCW )

                _ ->
                    ( tilePos, RotateCCW )

        inBounds =
            mousePos.x
                > 0
                && mousePos.y
                > 0
                && mousePos.x
                < board.renderSize
                && mousePos.y
                < board.renderSize
    in
    if inBounds then
        Just info

    else
        Nothing


{-| Lock/Unlock the tile at the given board coordinates.
-}
lockTile : Position -> Board -> Board
lockTile mousePos board =
    let
        click =
            clickInfo mousePos board

        lockFn =
            \tile ->
                case tile of
                    Just t ->
                        Just { t | locked = not t.locked }

                    Nothing ->
                        Nothing
    in
    case click of
        Just ( pos, _ ) ->
            { board | tiles = Dict.update pos lockFn board.tiles }

        Nothing ->
            board


{-| Rotate the given tile without a lock check - the caller must check whether
the tile is locked.
-}
rotateUnlockedTile : Rotation -> Tile -> Tile
rotateUnlockedTile dir tile =
    let
        rot =
            case dir of
                RotateCW ->
                    1

                RotateCCW ->
                    -1
    in
    { tile | connections = rotateList rot tile.connections }


{-| Update the number of moves the player has made for this board.
-}
updateMoves : Maybe Tile -> Board -> Board
updateMoves tile board =
    case ( tile, board.lastMove ) of
        ( Just t, Just l ) ->
            if ( t.x, t.y ) /= l then
                { board
                    | moves = board.moves + 1
                    , lastMove = Just ( t.x, t.y )
                }

            else
                board

        ( Just t, Nothing ) ->
            { board | moves = board.moves + 1, lastMove = Just ( t.x, t.y ) }

        _ ->
            board


{-| Rotate the tile at the given board coordinates in the given direction
-}
rotateTile : Position -> Board -> Board
rotateTile mousePos board =
    let
        click =
            clickInfo mousePos board

        ( tile, pos, dir ) =
            maybeCall click
                ( Nothing, ( 0, 0 ), RotateCW )
                (\( p, d ) -> ( getTile board p, p, d ))

        move =
            maybeCall tile False (\t -> not t.locked)

        newTile =
            maybeCall tile
                errorTile
                (\t -> rotateUnlockedTile dir t)
    in
    if move then
        { board | tiles = Dict.insert pos newTile board.tiles }
            |> updateMoves tile
            |> updateConnections

    else
        board



-- CONNECTION HANDLING


{-| Recalculate which tiles are connected to the source. This also updates
the board's state to indicate whether all tiles are connected or not.
-}
updateConnections : Board -> Board
updateConnections board =
    board
        |> resetConnections
        |> updateConnectionsHelper [ sourceTile board ] Set.empty


{-| Get a list of the neighbours of a given tile, in the format
[up, right, down, left]
-}
tileNeighbours : Maybe Tile -> Board -> List (Maybe Tile)
tileNeighbours tile board =
    let
        coords =
            case tile of
                Just t ->
                    [ ( t.x, t.y - 1 )
                    , ( t.x + 1, t.y )
                    , ( t.x, t.y + 1 )
                    , ( t.x - 1, t.y )
                    ]

                Nothing ->
                    []

        wrap =
            \( a, b ) -> ( modBy board.size a, modBy board.size b )
    in
    coords
        |> List.map wrap
        |> List.map (getTile board)


{-| Get a list of valid connections for a tile, in the form of a list of
bools indicating whether a connection in a certain direction exists.
The order is [up, right, down, left]
-}
tileConnections : Tile -> List Bool
tileConnections tile =
    List.map2 (\b c -> c && not b) tile.barriers tile.connections


{-| Determine whether a tile has a valid connection in a given direction.
-}
tileHasConnection : Maybe Tile -> Direction -> Bool
tileHasConnection tile dir =
    case tile of
        Just t ->
            tileConnections t
                |> zipList [ Up, Right, Down, Left ]
                |> List.filter (\( d, c ) -> c && d == dir)
                |> (\l -> List.length l > 0)

        Nothing ->
            False


{-| Get the opposite direction for a given direction.
-}
oppositeDir : Direction -> Direction
oppositeDir dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


{-| Determine whether two tiles are connected.
-}
tilesConnected : Maybe Tile -> Direction -> Maybe Tile -> Bool
tilesConnected src dir dst =
    tileHasConnection src dir && tileHasConnection dst (oppositeDir dir)


{-| Get a list of tiles which are connected to the given tile.
-}
tileConnectedNeighbours : Maybe Tile -> Board -> List (Maybe Tile)
tileConnectedNeighbours tile board =
    tileNeighbours tile board
        |> zipList [ Up, Right, Down, Left ]
        |> List.filter (\( dir, neighbour ) -> tilesConnected tile dir neighbour)
        |> List.map (\( _, t ) -> t)


{-| Mark a given tile as connected.
-}
markTileConnected : Maybe Tile -> Maybe Tile
markTileConnected tile =
    Maybe.map (\t -> { t | connected = True }) tile


{-| Update the board to mark a given tile as connected.
-}
markConnected : Maybe Tile -> Board -> Board
markConnected tile board =
    case tile of
        Just t ->
            { board
                | tiles = Dict.update ( t.x, t.y ) markTileConnected board.tiles
            }

        Nothing ->
            board


{-| Function used to filter out visited tile from a list.
-}
visitedFilter : Set.Set TilePos -> Maybe Tile -> Bool
visitedFilter visited tile =
    case tile of
        Just t ->
            not (Set.member ( t.x, t.y ) visited)

        Nothing ->
            False


{-| Add a given tile to the visited set.
-}
updateVisited : Maybe Tile -> Set.Set TilePos -> Set.Set TilePos
updateVisited tile visited =
    case tile of
        Just t ->
            Set.insert ( t.x, t.y ) visited

        Nothing ->
            visited


{-| Recursive helper function used by updateConnection. Maintains a queue
of tiles to visit next, and a set of tiles already visited.
-}
updateConnectionsHelper :
    List (Maybe Tile)
    -> Set.Set TilePos
    -> Board
    -> Board
updateConnectionsHelper queue visited board =
    case queue of
        tile :: tiles ->
            let
                filterFn =
                    visitedFilter visited

                neighbours =
                    List.filter filterFn (tileConnectedNeighbours tile board)

                newVisited =
                    updateVisited tile visited
            in
            markConnected tile board
                |> updateConnectionsHelper (neighbours ++ tiles) newVisited

        [] ->
            let
                tileCount =
                    board.size * board.size

                visitedCount =
                    Set.size visited

                state =
                    if tileCount == visitedCount then
                        Complete

                    else
                        Incomplete
            in
            { board | state = state }


{-| Get the 'source' tile for a given board
-}
sourceTile : Board -> Maybe Tile
sourceTile board =
    let
        centerPos =
            ( board.size // 2, board.size // 2 )
    in
    getTile board centerPos


{-| Mark all tiles as not connected
-}
resetConnections : Board -> Board
resetConnections board =
    { board
        | tiles =
            Dict.map (\_ t -> { t | connected = False }) board.tiles
    }



-- BOARD GENERATION


{-| Create an empty tile for the given position.
-}
emptyTile : Int -> TilePos -> Tile
emptyTile size ( x, y ) =
    let
        mid =
            size // 2

        tileType =
            if x == mid && y == mid then
                Source

            else
                Normal

        falses =
            List.repeat 4 False
    in
    Tile x y tileType False falses falses False


{-| A tile to use when failing to look up tiles from the board.
-}
errorTile : Tile
errorTile =
    emptyTile 0 ( 0, 0 )


{-| Create tiles for an empty board.
-}
emptyBoardTiles : Int -> TileDict
emptyBoardTiles size =
    let
        coords =
            List.range 0 (size - 1)
    in
    listProduct coords coords
        |> List.map (\pos -> ( pos, emptyTile size pos ))
        |> Dict.fromList


{-| Create an empty board of a given size.
-}
emptyBoard : Int -> Int -> Board
emptyBoard size renderSize =
    Board size (emptyBoardTiles size) Incomplete 0 0 Nothing renderSize


{-| Add an edge barrier to a tile if is along an edge.
-}
addWrap : Direction -> Int -> Tile -> Tile
addWrap dir boardSize tile =
    let
        mergeBarriers =
            \b t -> List.map2 (||) b t.barriers
    in
    case dir of
        Up ->
            if tile.y == 0 then
                { tile
                    | barriers = mergeBarriers [ True, False, False, False ] tile
                }

            else
                tile

        Right ->
            if tile.x == boardSize - 1 then
                { tile
                    | barriers = mergeBarriers [ False, True, False, False ] tile
                }

            else
                tile

        Down ->
            if tile.y == boardSize - 1 then
                { tile
                    | barriers = mergeBarriers [ False, False, True, False ] tile
                }

            else
                tile

        Left ->
            if tile.x == 0 then
                { tile
                    | barriers = mergeBarriers [ False, False, False, True ] tile
                }

            else
                tile


{-| Generate an empty board with barriers around the edge.
-}
enclosedBoard : Int -> Int -> Board
enclosedBoard size renderSize =
    let
        board =
            emptyBoard size renderSize

        updateFn dir _ tile =
            addWrap dir size tile
    in
    { board
        | tiles =
            board.tiles
                |> Dict.map (updateFn Up)
                |> Dict.map (updateFn Right)
                |> Dict.map (updateFn Down)
                |> Dict.map (updateFn Left)
    }


{-| Get the number of edges of a tile that don't currently have a connection.
-}
remainingConnections : Board -> TilePos -> Int
remainingConnections board pos =
    4 - connectionCount board pos


{-| Get the number of edges of a tile that have connections.
-}
connectionCount : Board -> TilePos -> Int
connectionCount board pos =
    let
        tile =
            getTile board pos

        countFn =
            \t ->
                t.connections
                    |> List.map
                        (\c ->
                            if c then
                                1

                            else
                                0
                        )
                    |> List.sum
    in
    maybeCall tile 0 countFn


{-| Get a list of surrounding tiles which haven't yet been visited (i.e.
have no connections yet). A list of the tiles, along with the direction to
that tile from the start location, is returned.
-}
availablePaths : Board -> TilePos -> List ( Direction, Tile )
availablePaths board pos =
    let
        tile =
            getTile board pos

        barriers =
            maybeCall tile [ False, False, False, False ] (\t -> t.barriers)

        filterFn =
            \( _, b, t ) ->
                not b && remainingConnections board ( t.x, t.y ) == 4

        directions =
            [ Up, Right, Down, Left ]
    in
    tileNeighbours (getTile board pos) board
        |> List.map (\t -> Maybe.withDefault errorTile t)
        |> List.map3 (\d b t -> ( d, b, t )) directions barriers
        |> List.filter filterFn
        |> List.map (\( d, _, t ) -> ( d, t ))


{-| Make a connection from a given tile in a given direction.
-}
connectTile : Tile -> Direction -> Board -> Board
connectTile tile dir board =
    let
        newConnections =
            zipList [ Up, Right, Down, Left ] tile.connections
                |> List.map
                    (\( d, c ) ->
                        if d == dir then
                            True

                        else
                            c
                    )

        updateFn =
            \dt ->
                case dt of
                    Just t ->
                        Just { t | connections = newConnections }

                    Nothing ->
                        Nothing
    in
    { board | tiles = Dict.update ( tile.x, tile.y ) updateFn board.tiles }


{-| Connect two tiles.
-}
connectTiles : Tile -> Direction -> Tile -> Board -> Board
connectTiles src dir dst board =
    let
        opposite =
            oppositeDir dir
    in
    board |> connectTile src dir |> connectTile dst opposite


{-| Make a new connection from a given tile, before moving onto the next
iteration of board generation.
-}
makeNewConnection : TilePos -> List TilePos -> ( Random.Seed, Board ) -> ( Random.Seed, Board )
makeNewConnection pos queue ( seed, board ) =
    let
        paths =
            availablePaths board pos

        pathCount =
            List.length paths

        ( selected, seed1 ) =
            Random.step (Random.int 1 pathCount) seed

        branchChance =
            0.3

        gen =
            Random.float 0 1 |> Random.map (\v -> branchChance > v)

        ( branch, newSeed ) =
            Random.step gen seed1

        path =
            List.drop (selected - 1) paths |> List.head

        tile =
            getTile board pos
    in
    case ( tile, path ) of
        ( Just src, Just ( dir, dst ) ) ->
            let
                newBoard =
                    connectTiles src dir dst board

                newQueue =
                    if branch then
                        pos :: ( dst.x, dst.y ) :: queue

                    else
                        ( dst.x, dst.y ) :: pos :: queue
            in
            generateBoardHelper newQueue ( newSeed, newBoard )

        _ ->
            generateBoardHelper queue ( newSeed, board )


{-| Recursive helper function used by generateBoard to generate a new board.
Maintains a queue of tiles to form connections from, performing a depth-first
traversal of the board.
-}
generateBoardHelper : List TilePos -> ( Random.Seed, Board ) -> ( Random.Seed, Board )
generateBoardHelper queue ( seed, board ) =
    case queue of
        q :: qs ->
            let
                pathCount =
                    List.length (availablePaths board q)
            in
            if connectionCount board q >= 3 || pathCount == 0 then
                generateBoardHelper qs ( seed, board )

            else
                makeNewConnection q qs ( seed, board )

        [] ->
            ( seed, board )


{-| Place a single barrier on the given side of a given tile, only if a
connection isn't needed through that edge.
-}
placeBarrier : Tile -> List Bool -> Board -> Board
placeBarrier tile merge board =
    let
        barriers =
            List.map3 (\bar mer con -> (bar || mer) && not con)
                tile.barriers
                merge
                tile.connections

        updateTile =
            \_ -> Just { tile | barriers = barriers }
    in
    { board | tiles = Dict.update ( tile.x, tile.y ) updateTile board.tiles }


{-| Place barriers for an individual tile. This function also updates the
neighbouring tiles with the barriers.
-}
placeTileBarriers : ( TilePos, ( Bool, Bool ) ) -> Board -> Board
placeTileBarriers ( pos, ( horiz, vert ) ) board =
    let
        tile =
            getTile board pos

        neighbours =
            tileNeighbours tile board
    in
    case ( tile, neighbours ) of
        ( Just t, [ Just up, Just right, _, _ ] ) ->
            board
                |> placeBarrier t [ horiz, vert, False, False ]
                |> placeBarrier up [ False, False, horiz, False ]
                |> placeBarrier right [ False, False, False, vert ]

        _ ->
            board


{-| Randomly place barriers on the grid in places where connections aren't
required.
-}
placeBarriers : ( Random.Seed, Board ) -> ( Random.Seed, Board )
placeBarriers ( seed, board ) =
    let
        range =
            List.range 0 (board.size - 1)

        coords =
            listProduct range range

        barrierChance =
            0.2

        gen =
            Random.map (\v -> barrierChance > v) (Random.float 0 1)

        comboGen =
            Random.map2 Tuple.pair gen gen

        listGen =
            Random.list (List.length coords) comboGen

        ( barrierDecisions, newSeed ) =
            Random.step listGen seed
    in
    ( newSeed
    , List.foldl placeTileBarriers
        board
        (zipList coords
            barrierDecisions
        )
    )


{-| Calculate the minimum number of moves to complete the board.
-}
minMoves : TileDict -> TileDict -> Int
minMoves original shuffled =
    let
        toConnections =
            \dictEntry -> .connections (Tuple.second dictEntry)

        origConnections =
            List.map toConnections (Dict.toList original)

        shuffledConnections =
            List.map toConnections (Dict.toList shuffled)

        changes =
            List.map2
                (\o s ->
                    if o == s then
                        0

                    else
                        1
                )
                origConnections
                shuffledConnections
    in
    List.sum changes


{-| Randomly rotate tiles in a generated board.
-}
shuffleBoard : ( Random.Seed, Board ) -> Board
shuffleBoard ( seed, board ) =
    let
        tiles =
            Dict.toList board.tiles

        gen =
            Random.list (List.length tiles) (Random.int 0 3)

        ( rotates, _ ) =
            Random.step gen seed

        newTiles =
            List.map2
                (\( p, t ) r ->
                    ( p
                    , { t
                        | connections =
                            rotateList r
                                t.connections
                      }
                    )
                )
                tiles
                rotates
                |> Dict.fromList
    in
    { board
        | tiles = newTiles
        , minMoves = minMoves board.tiles newTiles
    }


{-| Generate a new board layout of a given size.
-}
generateBoard : Int -> Int -> Random.Seed -> Bool -> Board
generateBoard size renderSize seed enclosed =
    let
        board =
            if enclosed then
                enclosedBoard size renderSize

            else
                emptyBoard size renderSize

        mid =
            size // 2

        centerPos =
            ( mid, mid )
    in
    generateBoardHelper [ centerPos ] ( seed, board )
        |> placeBarriers
        |> shuffleBoard
        |> updateConnections



-- BOARD RENDERING


tileSizeInt : Board -> Int
tileSizeInt board =
    board.renderSize // board.size


tileSizeFloat : Board -> Float
tileSizeFloat board =
    toFloat (tileSizeInt board)


lineWidth : Board -> Float
lineWidth board =
    tileSizeFloat board / 12


connectionColor : Tile -> String
connectionColor tile =
    if tile.connected then
        "#00ff00"

    else
        "#ffffff"


setRenderSize : Board -> Int -> Board
setRenderSize board size =
    { board | renderSize = size }


{-| Render a single connection arm from tile center to edge in a given direction.
In SVG coordinates: x goes right, y goes down.
The base shape is a rect extending upward from center to top edge, then rotated.
-}
renderConnection : Board -> Tile -> Float -> Float -> Float -> Svg msg
renderConnection board tile cx cy rotation =
    let
        halfTile =
            tileSizeFloat board / 2

        color =
            connectionColor tile

        w =
            lineWidth board

        -- Rect from center to top edge (before rotation)
        rx =
            cx - w / 2

        ry =
            cy - halfTile

        transform =
            "rotate(" ++ String.fromFloat rotation ++ " " ++ String.fromFloat cx ++ " " ++ String.fromFloat cy ++ ")"
    in
    Svg.rect
        [ SA.x (String.fromFloat rx)
        , SA.y (String.fromFloat ry)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat halfTile)
        , SA.fill color
        , SA.transform transform
        ]
        []


renderConnections : Board -> Tile -> Float -> Float -> List (Svg msg)
renderConnections board tile cx cy =
    let
        -- Directions: Up=0°, Right=90°, Down=180°, Left=270°
        rotations =
            [ 0, 90, 180, 270 ]

        pairs =
            List.map2 Tuple.pair tile.connections rotations
    in
    pairs
        |> List.filter (\( c, _ ) -> c)
        |> List.map (\( _, r ) -> renderConnection board tile cx cy r)


renderForeground : Board -> Tile -> Float -> Float -> List (Svg msg)
renderForeground board tile cx cy =
    let
        size =
            tileSizeFloat board
    in
    case tile.tileType of
        Source ->
            let
                s =
                    size / 1.5
            in
            [ Svg.rect
                [ SA.x (String.fromFloat (cx - s / 2))
                , SA.y (String.fromFloat (cy - s / 2))
                , SA.width (String.fromFloat s)
                , SA.height (String.fromFloat s)
                , SA.fill "#808080"
                ]
                []
            ]

        Normal ->
            let
                conCount =
                    tile.connections
                        |> List.map
                            (\c ->
                                if c then
                                    1

                                else
                                    0
                            )
                        |> List.sum
            in
            if conCount == 1 then
                [ Svg.circle
                    [ SA.cx (String.fromFloat cx)
                    , SA.cy (String.fromFloat cy)
                    , SA.r (String.fromFloat (size / 4))
                    , SA.fill (connectionColor tile)
                    ]
                    []
                ]

            else
                []


renderBackground : Board -> Tile -> Float -> Float -> List (Svg msg)
renderBackground board tile tileX tileY =
    let
        size =
            tileSizeFloat board

        innerSize =
            size * 0.99

        offset =
            (size - innerSize) / 2
    in
    [ Svg.rect
        [ SA.x (String.fromFloat tileX)
        , SA.y (String.fromFloat tileY)
        , SA.width (String.fromFloat size)
        , SA.height (String.fromFloat size)
        , SA.fill "#000000"
        ]
        []
    , Svg.rect
        [ SA.x (String.fromFloat (tileX + offset))
        , SA.y (String.fromFloat (tileY + offset))
        , SA.width (String.fromFloat innerSize)
        , SA.height (String.fromFloat innerSize)
        , SA.fill "#3465a4"
        ]
        []
    ]


renderLock : Board -> Float -> Float -> Svg msg
renderLock board tileX tileY =
    let
        size =
            tileSizeFloat board
    in
    Svg.rect
        [ SA.x (String.fromFloat tileX)
        , SA.y (String.fromFloat tileY)
        , SA.width (String.fromFloat size)
        , SA.height (String.fromFloat size)
        , SA.fill "#800080"
        , SA.opacity "0.5"
        ]
        []


renderBarriers : Board -> Tile -> Float -> Float -> List (Svg msg)
renderBarriers board tile tileX tileY =
    let
        size =
            tileSizeFloat board

        w =
            1.4 * lineWidth board

        color =
            "#5c3566"

        boardSz =
            board.size

        -- On board edges, keep barrier fully inside; between tiles, straddle the boundary
        upY =
            if tile.y == 0 then
                tileY

            else
                tileY - w / 2

        downY =
            if tile.y == boardSz - 1 then
                tileY + size - w

            else
                tileY + size - w / 2

        leftX =
            if tile.x == 0 then
                tileX

            else
                tileX - w / 2

        rightX =
            if tile.x == boardSz - 1 then
                tileX + size - w

            else
                tileX + size - w / 2

        up =
            Svg.rect
                [ SA.x (String.fromFloat tileX)
                , SA.y (String.fromFloat upY)
                , SA.width (String.fromFloat size)
                , SA.height (String.fromFloat w)
                , SA.fill color
                ]
                []

        right =
            Svg.rect
                [ SA.x (String.fromFloat rightX)
                , SA.y (String.fromFloat tileY)
                , SA.width (String.fromFloat w)
                , SA.height (String.fromFloat size)
                , SA.fill color
                ]
                []

        down =
            Svg.rect
                [ SA.x (String.fromFloat tileX)
                , SA.y (String.fromFloat downY)
                , SA.width (String.fromFloat size)
                , SA.height (String.fromFloat w)
                , SA.fill color
                ]
                []

        left =
            Svg.rect
                [ SA.x (String.fromFloat leftX)
                , SA.y (String.fromFloat tileY)
                , SA.width (String.fromFloat w)
                , SA.height (String.fromFloat size)
                , SA.fill color
                ]
                []

        allBarriers =
            [ up, right, down, left ]
    in
    List.map2 Tuple.pair tile.barriers allBarriers
        |> List.filter Tuple.first
        |> List.map Tuple.second


renderHoverOverlay : Board -> HoverSide -> Float -> Float -> List (Svg msg)
renderHoverOverlay board side tileX tileY =
    let
        size =
            tileSizeFloat board

        halfSize =
            size / 2

        ( highlightX, arrowX, arrowChar ) =
            case side of
                LeftSide ->
                    ( tileX, tileX + size * 0.15, "\u{21BB}" )

                RightSide ->
                    ( tileX + halfSize, tileX + size * 0.85, "\u{21BA}" )

        arrowY =
            tileY + size * 0.15

        fontSize =
            String.fromFloat (size * 0.22)
    in
    [ Svg.rect
        [ SA.x (String.fromFloat highlightX)
        , SA.y (String.fromFloat tileY)
        , SA.width (String.fromFloat halfSize)
        , SA.height (String.fromFloat size)
        , SA.fill "rgba(255,255,255,0.15)"
        ]
        []
    , Svg.text_
        [ SA.x (String.fromFloat arrowX)
        , SA.y (String.fromFloat arrowY)
        , SA.fill "white"
        , SA.fontSize (fontSize ++ "px")
        , SA.textAnchor "middle"
        , SA.dominantBaseline "central"
        ]
        [ Svg.text arrowChar ]
    ]


renderTile : Maybe ( TilePos, HoverSide ) -> Board -> ( TilePos, Tile ) -> Svg msg
renderTile hoverInfo board ( ( tx, ty ), tile ) =
    let
        size =
            tileSizeFloat board

        tileX =
            toFloat tx * size

        tileY =
            toFloat ty * size

        cx =
            tileX + size / 2

        cy =
            tileY + size / 2

        background =
            renderBackground board tile tileX tileY

        connections =
            renderConnections board tile cx cy

        barriers =
            renderBarriers board tile tileX tileY

        foreground =
            renderForeground board tile cx cy

        lockOverlay =
            if tile.locked then
                [ renderLock board tileX tileY ]

            else
                []

        hoverOverlay =
            case hoverInfo of
                Just ( pos, side ) ->
                    if pos == ( tx, ty ) && not tile.locked then
                        renderHoverOverlay board side tileX tileY

                    else
                        []

                Nothing ->
                    []
    in
    Svg.g []
        (background ++ connections ++ foreground ++ barriers ++ lockOverlay ++ hoverOverlay)


renderBoard : Maybe ( TilePos, HoverSide ) -> Board -> Svg msg
renderBoard hoverInfo board =
    let
        tiles =
            Dict.toList board.tiles

        sizeStr =
            String.fromInt board.renderSize
    in
    Svg.svg
        [ SA.width sizeStr
        , SA.height sizeStr
        , SA.viewBox ("0 0 " ++ sizeStr ++ " " ++ sizeStr)
        ]
        (List.map (renderTile hoverInfo board) tiles)
