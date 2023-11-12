open System

let getDim (n: int) =
    Math.Sqrt n |> Math.Ceiling |> int |> (+) 1

let getCenter (n: int) = (n / 2), (n / 2)

let createArray n : int option array2d =
    let dim = getDim n
    Array2D.create dim dim None

type Direction =
    | Up
    | Down
    | Left
    | Right

module Position =
    let private up = 0, -1
    let private right = 1, 0
    let private left = -1, 0
    let private down = 0, 1
    let private apply a pos =
        (fst a) + (fst pos), (snd a) + (snd pos)
    let move a dir =
        match dir with
        | Right -> apply a right
        | Up -> apply a up
        | Left -> apply a left
        | Down -> apply a down

let getNextDirection =
    function
    | Right -> Up
    | Up -> Left
    | Left -> Down
    | Down -> Right

let getNext (arr: int option array2d) pos dir =
    let nextDir = getNextDirection dir
    let x1, y1 = Position.move pos nextDir
    if arr[x1, y1] |> Option.isNone then
        (x1, y1), nextDir
    else
        (Position.move pos dir), dir

let createFilledArray n =
    let array = createArray n
    let xStart, yStart = getCenter (getDim n)

    let rec inner value (x, y) dir =
        array[x, y] <- Some value

        if value = n then
            (xStart, yStart), (x, y)
        else
            let nextPos, nextDir = getNext array (x, y) dir
            inner (value + 1) nextPos nextDir

    inner 1 (xStart, yStart) Down

let getNeighboursSum (array: int option array2d) (xpos, ypos) =
    let neighbours =
        [ for x in -1 .. 1 do
              for y in -1 .. 1 do
                  yield array[xpos + x, ypos + y] ]

    neighbours |> Seq.choose id |> Seq.sum

let solveDay2 n =
    let array = createArray n
    let xStart, yStart = getCenter (getDim n)
    array[xStart, yStart] <- Some 1

    let rec inner (x, y) dir =
        let value = getNeighboursSum array (x, y)
        array[x, y] <- Some value

        if value > n then
            value
        else
            let nextPos, nextDir = getNext array (x, y) dir
            inner nextPos nextDir

    inner (xStart, yStart) Down

// let start, stop = createFilledArray 23


let getDistance (x1: int, y1: int) (x2, y2) = Math.Abs(x2 - x1) + Math.Abs(y2 - y1)

let solveDay1 n = createFilledArray n ||> getDistance

solveDay1 289326
solveDay2 289326
