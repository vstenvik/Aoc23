open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""



type NumberWithPos = int * (int * int)
let getNumbers (str: string) =
    let matches = Regex("(\d+)").Matches(str)
    let matchesList = [for matc in matches-> matc] |> List.map ((fun m -> int m.Value , (int m.Index, int m.Length)))
    matchesList

test <@ getNumbers "467..114.." = [467, (0, 3); 114, (5,3)] @>

let positionsAround = [
    (-1, -1)
    (-1, 0)
    (-1, 1)
    (1, -1)
    (1, 0)
    (1, 1)
    (0, -1)
    (0, 1)
]

let isSymbol (c: char) = c = '*'

let tryGetAdjacent (grid: 'a array array) (x, y) =
    if x >= 0 && y >= 0 && grid.Length > x && grid[x].Length > y then
        Some grid.[x].[y]
    else
        None

let hasSymbolAdjacent (num: NumberWithPos) row (grid : char array array) =
    let index = snd num
    let start = fst index
    let length = (snd index) - 1
    let endIndex = start + length
    [for i = start to endIndex do
        for (offsetX, offsetY) in positionsAround do
            let i1 = (row + offsetX)
            let i2 = (i + offsetY)
            let adj = tryGetAdjacent grid (i1, i2)
            //Console.WriteLine (sprintf "%A %A :: %A %A" num adj offsetX offsetY)
            if adj.IsSome && isSymbol adj.Value then
                let gearId = (String [|adj.Value|]) + (i1.ToString()) + ":" + i2.ToString()
                yield (fst num, gearId)]
let getGrid (str: string) =
    str.Split("\n")
    |> Array.map Array.ofSeq
let getRows (str: string) =
    str.Split("\n")
let solve input =
    let grid = getGrid input
    let rows = input |> getRows |> Array.indexed

    [for row in rows do
        let rowNum = fst row
        for num in getNumbers (snd row) do
            hasSymbolAdjacent num rowNum grid]
    |> List.collect id
    |> List.groupBy snd
    |> List.map (fun (key,list) ->list |> List.map fst |> Set.ofList)
    |> List.filter (fun s -> s.Count = 2)
    |> List.map (Set.toList >> List.reduce (*))
    |> List.sum

let input = File.ReadAllText "./Day3/input.txt"
solve input
