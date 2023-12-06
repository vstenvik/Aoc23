open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".Split("\n") |> List.ofArray

type Card = Set<int> * int list
let parseCard (str : string): Card =
    let split = (str.Split(":", StringSplitOptions.TrimEntries)[1])
                    .Split("|", StringSplitOptions.TrimEntries)
    let options = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    let winning = Array.head split |> _.Split(" ", options) |> Array.map int |> Set.ofArray
    let having = Array.last split |> _.Split(" ", options) |> Array.map int  |> List.ofArray
    winning, having

let getWinningNumbers ((winning, having): Card) =
    having
    |> List.filter (fun v -> Set.contains v winning)

let getScore (nums : int list) =
    match nums with
    | [v] -> 1
    | v when v.Length > 1 -> pown 2 (v.Length - 1)
    | _ -> 0

let solve1 str =
    str
    |> Seq.map (parseCard >> getWinningNumbers >> getScore)
    |> Seq.sum

solve1 example

let input = File.ReadAllLines "./Day4/input.txt"

let getRewardCards cardList cardId wins =
    cardList
    |> Seq.skip (cardId+1)
    |> Seq.take wins

let solve2 str =
    let cards = str |> Seq.mapi (fun i v -> i, parseCard v)
    let rec inner acc icards =
        [ for i, v in icards do
              let length = (getWinningNumbers v).Length
              if length = 0 then
                  yield 1
              else
                let reward = getRewardCards cards i length
                yield 1 + (inner 0 reward)]
        |> Seq.sum
    inner (0) cards
    
solve2 input