#r "nuget: Unquote"
open System
open Swensen.Unquote

type MemoryBanks = int list
let example = [0; 2; 7; 0]
let getRedistBank (lst : MemoryBanks) =
    let max = List.max lst
    let index = List.findIndex ((=) max) lst
    let redistList = List.updateAt index 0 lst
    index, max, redistList

let step index bag (list : MemoryBanks) =
    let safeIndex = index % list.Length
    let value = list[safeIndex]
    let newList = List.updateAt safeIndex (value + 1) list
    let nextIndex = ((safeIndex + 1) % list.Length)
    let remaining = bag - 1
    newList, nextIndex, remaining

let redistribute index bag (list : MemoryBanks) =
    let rec inner iindex ibag ilist =
        if ibag = 0 then
            ilist
        else
            let nlist, nindex, nremain = step iindex ibag ilist
            inner nindex nremain nlist
    inner index bag list

test <@ getRedistBank [0;2;7;0] = (2, 7, [0;2;0;0]) @>
test <@ step 3 7 [0;2;0;0] = ([0;2;0;1], 0, 6)  @>
test <@ redistribute 3 7 [0;2;0;0] = [2;4;1;2]  @>

let solvePart1 input =
    let rec inner countRedists prevLists currentList =
        let index, value, state = getRedistBank currentList
        let newCurrentList = redistribute (index + 1) value state
        if Set.contains newCurrentList prevLists then
            countRedists
        else
            let newCountRedists = (countRedists + 1)
            let newPrevLists = (Set.add newCurrentList prevLists)
            inner newCountRedists newPrevLists newCurrentList
    inner 1 Set.empty input

test <@ solvePart1 example = 5 @>
solvePart1 example

let solvePart2 input =
    let endsLoop loopStart current =
        match loopStart with
        | Some value -> value = current
        | None -> false
    let rec inner loopStart countRedists prevLists currentList =
        let index, value, state = getRedistBank currentList
        let newCurrentList = redistribute (index + 1) value state
        if endsLoop loopStart newCurrentList then
            countRedists
        else if Option.isNone loopStart && Set.contains newCurrentList prevLists then
            let newPrevLists = (Set.add newCurrentList prevLists)
            inner (Some newCurrentList) 1 newPrevLists newCurrentList
        else
            let newCountRedists = (countRedists + 1)
            let newPrevLists = (Set.add newCurrentList prevLists)
            inner loopStart newCountRedists newPrevLists newCurrentList
    inner None 1 Set.empty input
let options = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
let input = "4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3".Split(" ", options)
            |> Seq.map int |> List.ofSeq
solvePart1 input
solvePart2 input