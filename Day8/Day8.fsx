
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

let example3 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""


type Parsed = {
    rl: char list
    mappings: Map<string, Mapping>
    start: string list
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
        start = mappings |> List.map fst |> List.filter _.EndsWith("A")
    }
    
let solve (input : Parsed) =
    let rec inner (mapping : Mapping) (instructions:char list) (step:UInt64) =
        if mapping |> (fst >> _.EndsWith("Z")) then
            step
        else
            match instructions with
            // List is not empty, deconstruct to first element and rest
            | current::rest ->
               // get a function that takes the first or second (left/right) mapping
               let leftOrRight = match current with
                                 | 'R' -> snd >> snd
                                 | 'L' -> snd >> fst
               // Apply the leftOrRight-function to the current mapping, and use the result to get the next mapping
               let nextMapping = input.mappings[leftOrRight mapping]
               // Recurse with next mapping and rest of instructions, increasing the step count
               inner nextMapping rest (step + 1UL)
            // List is empty, rerun the same mapping with a fresh copy of the instruction list
            | [] -> inner mapping input.rl step
            
    input.start
    // Get initial mappings
    |> List.map (fun s -> input.mappings[s])
    // Find lowest number of steps for each start-mapping
    |> List.map (fun start -> inner input.mappings[fst start] input.rl 0UL) 
    

let rec gcd a b =
    if a % b = 0UL then b
    else gcd b (a % b)

let lcm a b =
    a / (gcd a b) * b
    
File.ReadAllText "./Day8/input.txt"
|> parse
|> solve
|> List.reduce lcm