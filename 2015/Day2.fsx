open System
open System.IO

let example =
    """5 1 9 5
7 5 3
2 4 6 8"""


let example2 =
    """5 9 2 8
9 4 7 3
3 8 6 5"""

let input = File.ReadAllText "./2015/Day2.txt"

let parseInput (str: string) =
    str.Split("\n")
    |> Seq.map (fun s ->
        s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.TrimEntries)
        |> Seq.map int)

let getChecksum (row: int seq) =
    let max = Seq.max row
    let min = Seq.min row
    max - min


let isDivisibleBy a b = a % b = 0 || b % a = 0

let tryFindMatch (value: int) (rest: int seq) =
    match Seq.tryFind (isDivisibleBy value) rest with
    | Some v -> Some(value, v)
    | None -> None

let divide (a: int) (b: int) =
    let max = Math.Max(a, b)
    let min = Math.Min(a, b)
    max / min

let getDivisiblePairs (row: int seq) =
    let rec inner (row2: int list) =
        match row2 with
        | l when Seq.length l < 2 -> None
        | head :: tail ->
            match tryFindMatch head tail with
            | Some v -> Some v
            | None -> inner tail

    inner (List.ofSeq row)

parseInput input
|> Seq.choose getDivisiblePairs
|> Seq.map (fun (a, b) -> divide a b)
|> Seq.sum

