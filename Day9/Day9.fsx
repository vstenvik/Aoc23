
open System
open System.IO
let splitopts = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

let example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

let parse (str:string) =
    str.Split("\n", splitopts)
    |> Seq.map (_.Split(" ", splitopts) >> List.ofArray >> (List.map int))
    |> List.ofSeq

let diff (a:int,b:int) =
    let diff = Math.Abs(a-b)
    if b < a then diff * -1
    else diff
    
let diffList list =
    list
    |> List.pairwise
    |> List.map diff

let extrapolateNextValue (list : int list) =
    let rec findNext acc l =
        if l |> List.forall ((=) 0) then
            acc
        else
            let lastNumber = List.last l
            let diffedList = diffList l
            (findNext (acc + lastNumber) diffedList)
    findNext 0 list

let input = File.ReadAllText("./Day9/input.txt")
let solve str =
    str
    |> List.map extrapolateNextValue
    |> List.sum
    
// Part 1
input
|> parse
|> solve

 // Part 2
input
|> parse
|> List.map List.rev
|> solve