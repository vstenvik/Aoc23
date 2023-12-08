
open System
open System.IO
let splitopts = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

let example = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

let example2 = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

type Parsed = {
    rl: char list
    mappings: Map<string, Mapping>
    start: string
    stop: string
}
and Mapping = string * (string * string)

let parse (str:string) =
    let split = str.Split("\n\n")
    let rl = split.[0]
    let mappingsUnparsed = split.[1]
    let mappings = [ for row in mappingsUnparsed.Split("\n") do
                          let rowSplit = row.Split(" = ")
                          let id = rowSplit.[0]
                          let mapping = rowSplit.[1].Replace("(", "").Replace(")", "").Split(", ", splitopts) |> (fun v -> id, (v.[0], v.[1]))
                          (id,mapping)
                    ]
    {
        rl = rl |> List.ofSeq
        mappings = mappings |> Map.ofList
        start = "AAA"
        stop = "ZZZ"
    }

let solve (input : Parsed) =
    let rec inner (mapping : Mapping) (instructions:char list) (step:UInt64) =
        if fst mapping = input.stop then
            step
        else
            match instructions with
            | current::rest ->
               let next = match current with
                           | 'R' -> mapping |> snd |> snd
                           | 'L' -> mapping |> snd |> fst
                           | _ -> failwithf "Invalid direction %A" current
               inner input.mappings[next] rest (step + 1UL)
            | [] -> inner mapping input.rl step
    inner input.mappings[input.start] input.rl 0UL
    
parse example |> solve
parse example2 |> solve

File.ReadAllText "./Day8/input.txt"
|> parse
|> solve