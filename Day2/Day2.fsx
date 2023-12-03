open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".Split("\n") |> List.ofArray

type GameSets = (string * int) seq
type Game = int * GameSets
let pattern = Regex(@"Game (\d+):(?: (\d+ [a-zA-Z]+)(?:[,:] (\d+ [a-zA-Z]+))*)*")
(pattern.Match example[0]).Groups
|> Seq.skip 1
|> Seq.map _.Value.Trim()
|> Seq.filter (fun v -> not (String.IsNullOrEmpty v))
|> Seq.toList
let parseRow (str: string): Game =
    let gameId = str.Split(":")[0] |> Seq.skip 5 |> Array.ofSeq |> String |> int
    let onlySets = str.Split(":", StringSplitOptions.TrimEntries)[1]
    let options = StringSplitOptions.TrimEntries
    let gameSets = onlySets.Split(",", options)
                    |> Seq.collect _.Split(";", options)
                    |> Seq.map (_.Split(" ", options) >> (fun arr -> Array.last arr, (Array.head arr |> int)))
    gameId, gameSets

let configuration = Map ["red", 12; "green", 13; "blue", 14]

let tryPossible (configuration: Map<string, int>) (gameSets: Game) =
    let impossible = (snd gameSets) |> Seq.tryFind (fun (name,amount) -> configuration[name] < amount)
    if impossible.IsSome then
        None
    else
        Some (fst gameSets)

let solvePart1 =
    let input = File.ReadAllLines "./Day2/input.txt"

    input
    |> Seq.map parseRow
    |> Seq.choose (tryPossible configuration)
    |> Seq.sum

let getLeastCubesPerType (game : Game) =
    let sets = snd game
    sets
    |> Seq.groupBy fst
    |> Seq.map (fun group -> Seq.maxBy snd (snd group))
    |> List.ofSeq


<@ (getLeastCubesPerType << parseRow) "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" = [("blue", 6); ("red", 4); ("green", 2)] @>
<@ (getLeastCubesPerType << parseRow) "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" = [("green", 3); ("red", 14); ("blue", 15)] @>

let getPower (gameSets: GameSets) =
    gameSets
    |> Seq.map snd
    |> Seq.reduce (*) 

let input = File.ReadAllLines "./Day2/input.txt"
input
|> Seq.map (parseRow >> getLeastCubesPerType >> getPower)
|> Seq.sum
