open System.Text.RegularExpressions
open System

let example = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""


let optionOfString (str : string) =
    if String.IsNullOrEmpty str then
        None
    else
        Some str
let parseMain str =
    let m = Regex.Match(str, "(\S+) \((\d+)\)( -> .*)?$")
    let name = m.Groups[1].Value
    let weight = m.Groups[2].Value |> int
    let rest = optionOfString m.Groups[3].Value
    (name, weight, rest)
    
let parseRest (str : string) =
    str.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    

parseMain "tknk (41) -> ugml, padx, fwft"