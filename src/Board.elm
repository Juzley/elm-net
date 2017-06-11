module Board
    exposing
        ( Board
        , BoardState(..)
        , Tile
        , TileType(..)
        , emptyBoard
        , generateBoard
        , rotateTile
        , lockTile
        , setRenderSize
        , renderBoard
        )

import List
import Dict
import Set
import Color
import Collage
import Random
import Mouse
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



-- MISC


{-| Get the tile at the given board coordinates.
-}
getTile : Board -> TilePos -> Maybe Tile
getTile board pos =
    Dict.get pos board.tiles



-- INPUT HANDLING


{-| Determine which tile a click has hit.
-}
clickInfo : Mouse.Position -> Board -> Maybe ( TilePos, Rotation )
clickInfo mousePos board =
    let
        size =
            tileSizeInt board

        tilePos =
            ( mousePos.x // size, mousePos.y // size )

        info = 
            case (mousePos.x // (size // 2)) % 2 of
                0 ->
                    ( tilePos, RotateCW )

                _ ->
                    ( tilePos, RotateCCW )

        inBounds =
            mousePos.x > 0 && mousePos.y > 0 &&
            mousePos.x < board.renderSize &&
            mousePos.y < board.renderSize
    in
        if inBounds then
            Just info
        else
            Nothing


{-| Lock/Unlock the tile at the given board coordinates.
-}
lockTile : Mouse.Position -> Board -> Board
lockTile mousePos board =
    let
        click = 
            clickInfo mousePos board

        lockFn =
            (\tile ->
                case tile of
                    Just t ->
                        Just { t | locked = not t.locked }

                    Nothing ->
                        Nothing
            )
    in
        case click of
            Just (pos, _) ->
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


{-| Rotate the tlie at the given board coordinates in the given direction
-}
rotateTile : Mouse.Position -> Board -> Board
rotateTile mousePos board =
    let
        click =
            clickInfo mousePos board

        ( tile, pos, dir ) =
            maybeCall click ( Nothing, (0, 0), RotateCW )
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
            (\( a, b ) -> ( a % board.size, b % board.size ))
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


{-| Determine whether a tile has a valid connection in a given diretion.
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
    (tileHasConnection src dir) && (tileHasConnection dst (oppositeDir dir))


{-| Get a list of tiles which are connected to the given tile.
-}
tileConnectedNeighbours : Maybe Tile -> Board -> List (Maybe Tile)
tileConnectedNeighbours tile board =
    tileNeighbours tile board
        |> zipList [ Up, Right, Down, Left ]
        |> List.filter (\( dir, neighbour ) -> tilesConnected tile dir neighbour)
        |> List.map (\( d, t ) -> t)


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
            (\b t -> List.map2 (||) b t.barriers)
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

        updateFn dir pos tile =
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
    4 - (connectionCount board pos)


{-| Get the number of edges of a tile that have connections.
-}
connectionCount : Board -> TilePos -> Int
connectionCount board pos =
    let
        tile =
            getTile board pos

        countFn =
            (\t ->
                t.connections
                    |> List.map
                        (\c ->
                            if c then
                                1
                            else
                                0
                        )
                    |> List.sum
            )
    in
        maybeCall tile 0 countFn


{-| Get a list of surrounding tiles which haven't yet been visisted (i.e.
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
            (\( d, b, t ) ->
                (not b) && remainingConnections board ( t.x, t.y ) == 4
            )
    in
        tileNeighbours (getTile board pos) board
            |> List.map (\tile -> Maybe.withDefault errorTile tile)
            |> List.map3 (,,) [ Up, Right, Down, Left ] barriers
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
            (\dt ->
                case dt of
                    Just t ->
                        Just { t | connections = newConnections }

                    Nothing ->
                        Nothing
            )
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

        ( selected, newSeed ) =
            Random.step (Random.int 1 (pathCount)) seed

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
                in
                    generateBoardHelper (( dst.x, dst.y ) :: pos :: queue) ( newSeed, newBoard )

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
            (\_ -> Just { tile | barriers = barriers })
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
            (Random.map ((>) barrierChance) (Random.float 0 1))

        comboGen =
            Random.map2 (,) gen gen

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


{-| Calculate the minimum number of moves to complete the board. Note this
assumes that the board has a unique solution, which isn't necessarily true,
so there may be cases where this minimum isn't the true minimum.
-}
minMoves : TileDict -> TileDict -> Int
minMoves original shuffled =
    let
        toConnections =
            (\dictEntry -> .connections <| Tuple.second dictEntry)

        origConnections =
            List.map toConnections <| Dict.toList original

        shuffledConnections =
            List.map toConnections <| Dict.toList shuffled

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


collageTop : Board -> Int
collageTop board =
    board.renderSize // 2


collageLeft : Board -> Int
collageLeft board =
    -board.renderSize // 2


tileCoordsToCollagePosition : TilePos -> Board -> ( Float, Float )
tileCoordsToCollagePosition ( tileX, tileY ) board =
    let
        size =
            tileSizeInt board

        top =
            (collageTop board) - tileY * size

        left =
            (collageLeft board) + tileX * size

        centerY =
            top - size // 2

        centerX =
            left + size // 2
    in
        ( toFloat (centerX), toFloat (centerY) )


connectionColor : Tile -> Color.Color
connectionColor tile =
    if tile.connected then
        Color.green
    else
        Color.white


renderConnection : Board -> Tile -> ( Float, Float ) -> Float -> Collage.Form
renderConnection board tile translation rotation =
    let
        size =
            tileSizeFloat board / 2

        color =
            connectionColor tile

        width =
            lineWidth board

        form =
            Collage.filled color (Collage.rect width size)
    in
        form
            |> Collage.move translation
            |> Collage.rotate (degrees rotation)


renderUpConnection : Board -> Tile -> Collage.Form
renderUpConnection board tile =
    let
        translate =
            ( 0, tileSizeFloat board / 4 )
    in
        renderConnection board tile translate 0


renderRightConnection : Board -> Tile -> Collage.Form
renderRightConnection board tile =
    let
        translate =
            ( tileSizeFloat board / 4, 0 )
    in
        renderConnection board tile translate 90


renderDownConnection : Board -> Tile -> Collage.Form
renderDownConnection board tile =
    let
        translate =
            ( 0, -(tileSizeFloat board) / 4 )
    in
        renderConnection board tile translate 0


renderLeftConnection : Board -> Tile -> Collage.Form
renderLeftConnection board tile =
    let
        translate =
            ( -(tileSizeFloat board) / 4, 0 )
    in
        renderConnection board tile translate 90


renderConnections : Board -> Tile -> List Collage.Form
renderConnections board tile =
    let
        renderFns =
            [ renderUpConnection board
            , renderRightConnection board
            , renderDownConnection board
            , renderLeftConnection board
            ]
    in
        zipList renderFns tile.connections
            |> List.filter (\( r, c ) -> c)
            |> List.map (\( r, c ) -> r tile)


renderForeground : Board -> Tile -> List Collage.Form
renderForeground board tile =
    let
        size =
            tileSizeFloat board
    in
        case tile.tileType of
            Source ->
                [ Collage.filled Color.grey (Collage.square (size / 1.5))
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
                        [ Collage.filled (connectionColor tile)
                            (Collage.circle (size / 4))
                        ]
                    else
                        []


renderBackground : Board -> Tile -> List Collage.Form
renderBackground board tile =
    let
        size =
            tileSizeFloat board
    in
        [ Collage.filled Color.black (Collage.square size)
        , Collage.filled Color.blue (Collage.square (size * 0.99))
        ]


renderLock : Board -> Collage.Form
renderLock board =
    Collage.square (tileSizeFloat board)
        |> Collage.filled Color.purple
        |> Collage.alpha 0.5



-- TODO: Can probably tidy the group/list handling up


setRenderSize : Board -> Int -> Board
setRenderSize board size =
    { board | renderSize = size }


renderTile : Board -> ( TilePos, Tile ) -> Collage.Form
renderTile board ( pos, tile ) =
    let
        background =
            renderBackground board tile

        connections =
            renderConnections board tile

        barriers =
            renderBarriers board ( pos, tile )

        foreground =
            renderForeground board tile

        group =
            if tile.locked then
                Collage.group
                    (background
                        ++ connections
                        ++ foreground
                        ++ [ renderLock board ]
                    )
            else
                Collage.group (background ++ connections ++ foreground)

        move =
            Collage.move (tileCoordsToCollagePosition pos board)
    in
        move group


barrierRenderFunctions : Board -> TilePos -> List Collage.Form
barrierRenderFunctions board pos =
    let
        size =
            tileSizeFloat board

        width =
            1.4 * lineWidth board

        line =
            Collage.filled Color.darkPurple (Collage.rect width size)

        edgeTranslate =
            size / 2 - width / 2 + 1

        upFn =
            if Tuple.second pos == 0 then
                Collage.move ( 0, edgeTranslate)
                    (Collage.rotate (degrees 90) line)
            else
                Collage.move ( 0, size / 2)
                    (Collage.rotate (degrees 90) line)

        rightFn =
            if Tuple.first pos == board.size - 1 then
                Collage.move ( edgeTranslate, 0 ) line
            else
                Collage.move ( size / 2, 0 ) line

        downFn =
            if Tuple.second pos == board.size - 1 then
                Collage.move ( 0, -edgeTranslate )
                    (Collage.rotate (degrees 90) line)     
            else
                Collage.move ( 0, -size / 2 )
                    (Collage.rotate (degrees 90) line)     

        leftFn =
            if Tuple.first pos == 0 then
                Collage.move ( -edgeTranslate, 0 ) line
            else
                Collage.move ( -size / 2, 0 ) line
    in
        [ upFn, rightFn, downFn, leftFn ] 


renderBarriers : Board -> ( TilePos, Tile ) -> Collage.Form
renderBarriers board ( pos, tile ) =
    let
        zipped =
            List.map2 (,) tile.barriers (barrierRenderFunctions board pos)

        filtered =
            List.map Tuple.second (List.filter Tuple.first zipped)

        move =
            Collage.move (tileCoordsToCollagePosition pos board)

        group =
            Collage.group filtered
    in
        move group


renderBoard : Board -> List Collage.Form
renderBoard board =
    let
        tiles =
            Dict.toList board.tiles

        tileRenders =
            List.map (renderTile board) tiles

        barrierRenders =
            List.map (renderBarriers board) tiles
    in
        tileRenders ++ barrierRenders
