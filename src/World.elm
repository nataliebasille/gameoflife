module World exposing (CellState(..), GameBoard, create, nextGeneration, toggleCell)

import Array


type CellState
    = Alive
    | Dead


type alias GameBoard =
    { cellSize : Int
    , rows : Int
    , columns : Int
    , board : List CellState
    }


flipCellState : CellState -> CellState
flipCellState cellState =
    case cellState of
        Alive ->
            Dead

        Dead ->
            Alive


create : Int -> Int -> GameBoard
create rows columns =
    { cellSize = 40
    , rows = rows
    , columns = columns
    , board = List.repeat (rows * columns) Dead
    }


toggleCell : Int -> GameBoard -> GameBoard
toggleCell index gameboard =
    { gameboard
        | board =
            List.indexedMap
                (\cellIndex cell ->
                    if index == cellIndex then
                        cell |> flipCellState

                    else
                        cell
                )
                gameboard.board
    }


nextGeneration : GameBoard -> GameBoard
nextGeneration gameboard =
    { gameboard
        | board = calculateNextGeneration gameboard.rows gameboard.columns gameboard.board
    }


indexToCoordinates : Int -> Int -> Int -> ( Int, Int )
indexToCoordinates rows columns index =
    ( index // columns, modBy rows index )


coordinatesToIndex : Int -> ( Int, Int ) -> Int
coordinatesToIndex rows ( row, column ) =
    rows * row + column


calculcateNextCellState : ( Int, CellState ) -> CellState
calculcateNextCellState ( aliveNeighborCount, state ) =
    case state of
        Dead ->
            if aliveNeighborCount == 3 then
                Alive

            else
                Dead

        Alive ->
            if aliveNeighborCount < 2 then
                Dead

            else if aliveNeighborCount <= 3 then
                Alive

            else
                Dead


calculateNextGeneration : Int -> Int -> List CellState -> List CellState
calculateNextGeneration rows columns world =
    let
        updateAliveNeighborsHash : Int -> Array.Array ( Int, CellState ) -> Array.Array ( Int, CellState )
        updateAliveNeighborsHash index hash =
            let
                ( row, column ) =
                    indexToCoordinates rows columns index

                viableNeighbors =
                    [ ( row - 1, column - 1 ), ( row - 1, column ), ( row - 1, column + 1 ), ( row, column - 1 ), ( row, column + 1 ), ( row + 1, column - 1 ), ( row + 1, column ), ( row + 1, column + 1 ) ]
                        |> List.filter (\( r, c ) -> r >= 0 && c >= 0)
                        |> List.map (coordinatesToIndex rows)
            in
            viableNeighbors
                |> List.foldl
                    (\neighbor array ->
                        case Array.get neighbor array of
                            Maybe.Just ( value, state ) ->
                                Array.set neighbor ( value + 1, state ) array

                            Maybe.Nothing ->
                                array
                    )
                    hash

        calculateNumberOfAliveNeighborsForEveryCell : List CellState -> List ( Int, CellState )
        calculateNumberOfAliveNeighborsForEveryCell listOfStates =
            listOfStates
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, state ) -> state == Alive)
                |> List.map (\( index, _ ) -> index)
                |> List.foldl updateAliveNeighborsHash
                    (listOfStates |> List.map (\state -> ( 0, state )) |> Array.fromList)
                |> Array.toList
    in
    world
        |> calculateNumberOfAliveNeighborsForEveryCell
        |> List.map calculcateNextCellState
