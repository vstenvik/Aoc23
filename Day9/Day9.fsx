
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
    
let diffB (a, b) = diff (b, a)
    
let diffList list =
    list
    |> List.pairwise
    |> List.map diff
    
let diffListB list =
    list
    |> List.pairwise
    |> List.map diff

let expandListForward (list : int list) =
    let rec findNext l =
        if l |> List.forall ((=) 0) then
            0
        else
            let lastNumber = List.last l
            let diffedList = diffList l
            lastNumber + (findNext diffedList)
    findNext list
    
let expandListBackward (list : int list) =
    let rec findNext l =
        if l |> List.forall ((=) 0) then
            0
        else
            let lastNumber = List.head l
            let diffedList = diffListB l
            lastNumber - (findNext diffedList)
    findNext list

let input = File.ReadAllText("./Day9/input.txt")

input
|> parse
|> List.map expandListBackward
|> List.sum