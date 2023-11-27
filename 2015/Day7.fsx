open System.Text.RegularExpressions

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


let parseMain str =
    let m = Regex.Match(str, "()")
let parseRow (str : string) =
    match str.Split(" -> ") with
    | [|main; children|] -> Some main, Some children
    | [|main|] -> Some main, None
    | _ -> failwith "Invalid row"

parseRow "tknk (41) -> ugml, padx, fwft"
