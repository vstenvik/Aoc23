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
    //seeds : uint64 array
    maps : SeedMap list
}
and SeedMap = {
    name : string
    mappings : Mapping list
}
and Mapping = (uint64*uint64*uint64)

let parseMap (str : string) =
    let split = str.Split("\n")
    let name = Array.head split |> _.Split(" ") |> Array.head
    let parseNums (str:string) = str.Split(" ", StringSplitOptions.TrimEntries) |> Array.map uint64
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
    //printf "Parsing seeds"
    //let seeds = parseSeeds seeds
    //printf "done"
    printf "parsing maps"
    let seedMaps = rest |> List.map parseMap
    printf "done"
    {
        //seeds = seeds
        maps = seedMaps
    }

let isInRange num ((s,d,l): Mapping)=
    num >= s && num < s + l
    
let getMappingValue num ((s,d,l) : Mapping)=
    let offset = num - s
    d + offset
    
let getMapping (mappings : Mapping list) num =
    match List.tryFind (isInRange num) mappings with
    | Some v -> v
    | None -> (num, num, 1UL)
    
let getLocation (seed : uint64) (seedMaps : SeedMap list) =
    let rec inner (iSeedMaps : SeedMap list) (prev : uint64) =
        match iSeedMaps with
        | [] -> prev
        | [v] -> getMapping v.mappings prev |> getMappingValue prev
        | head :: rest ->
            let mapping = getMapping head.mappings prev |> getMappingValue prev
            inner rest mapping  
    inner seedMaps seed

let input = File.ReadAllText "./Day5/input.txt"
let state = parse input
let seeds = input.Split("\n", StringSplitOptions.TrimEntries)[0]

let parseSeeds (state: State) (str : string) =
    str
    |> Seq.skip "seeds: ".Length
    |> Seq.toArray |> String
    |> _.Split(" ")
    |> Array.map uint64
    |> Array.chunkBySize 2
    |> Array.map (fun v ->
                        let start = Array.head v
                        let stop = start + Array.last v - 1UL
                        printfn "Seed: %A"  start
                        let locations = [|for i = start to stop do
                                          let location = getLocation i state.maps
                                          location |]
                        Array.min locations)
    |> Array.min

parseSeeds state seeds