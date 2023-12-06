
open System
open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """Time:      71530
Distance:  940200"""

let input = """Time:        50748685
Distance:   242101716911252"""

let parse (str : string) =
    let splitOptions = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    let arrays = str.Split("\n")
                |> Array.map (_.Split(" ", splitOptions) >> Array.tail >> Array.map int64)
    arrays[1]
    |> Array.zip arrays[0]
    
let getWinning (time, distance) =
    [for holdTime = 1L to time do
        (time - holdTime) * holdTime]
    |> List.filter ((<) distance)
    |> List.length

parse input
|> Array.map getWinning
|> Array.reduce (*)