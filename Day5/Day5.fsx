open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""


type State = {
    seeds : int64 array
    maps : SeedMap list
}
and SeedMap = {
    name : string
    mappings : Mapping list
}
and Mapping = (int64*int64*int64)

let parseSeeds (str : string) =
    str
    |> Seq.skip "seeds: ".Length
    |> Seq.toArray |> String
    |> _.Split(" ")
    |> Array.map int64

let parseMap (str : string) =
    let split = str.Split("\n")
    let name = Array.head split |> _.Split(" ") |> Array.head
    let parseNums (str:string) = str.Split(" ", StringSplitOptions.TrimEntries) |> Array.map int64
    let nums = Array.tail split
               |> Array.map parseNums
    let mappings = [ for num in nums do
                      let dest = num[0]
                      let source = num[1]
                      let length = num[2]
                      source, dest, length]
    {
        name = name
        mappings = mappings |> List.sortBy (fun (k,_,_) -> k)
    }
    
    
// example.Split("\n\n")[1] |> parseMap 

let parse (str : string) =
    let split = str.Split("\n\n")
    let seeds = Array.head split
    let rest = Array.tail split |> List.ofArray
    {
        seeds = parseSeeds seeds
        maps = rest |> List.map parseMap
    }

let isInRange num ((s,d,l): Mapping)=
    num >= s && num < s + l
let getMappingValue num ((s,d,l) : Mapping)=
    let offset = num - s
    d + offset
let getMapping (mappings : Mapping list) num =
    match List.tryFind (isInRange num) mappings with
    | Some v -> v
    | None -> (num, num, 1)
let getLocation (seed : int64) (seedMaps : SeedMap list) =
    let rec inner (iSeedMaps : SeedMap list) (prev : int64) =
        match iSeedMaps with
        | [] -> prev
        | [v] -> getMapping v.mappings prev |> getMappingValue prev
        | head :: rest ->
            let mapping = getMapping head.mappings prev |> getMappingValue prev
            inner rest mapping  
    inner seedMaps seed

let input = File.ReadAllText "./Day5/input.txt"
let state = parse input

[ for seed in state.seeds do
      printf "%A" seed
      getLocation seed state.maps ]
|> List.min