open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote

let example = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""


let isNumber (c : char) =
    (int c) >= (int '0') && (int c) <= (int '9') 
        

let findDigitsInRow (str: string) =
    str
    |> Seq.filter isNumber
    |> List.ofSeq

let parse (str: string) =
    str.Split("\n")
    
let getFirstAndLast (a: char list) =
    let first = a[0]
    let last = List.last a
    String [|first; last|]

let solve (str : string) =
    str
    |> parse
    |> Seq.map (findDigitsInRow >> getFirstAndLast)
    |> Seq.map int
    |> Seq.sum
    

solve example

let input = File.ReadAllText "./Day1/input.txt"

solve input

let example2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
2
4nineeightseven2
zoneight234
7pqrstsixteen"""

let findNumbersInRow (str : string) =
    let matches = Regex.Matches(str, "(?<=(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9))")
    [for m in matches do
         yield (Seq.last m.Groups).Value]

let numberToNumber = function
    | "one" | "1" -> 1
    | "two" | "2" -> 2
    | "three" | "3" -> 3
    | "four" | "4" -> 4
    | "five" | "5" -> 5
    | "six" | "6" -> 6
    | "seven" | "7" -> 7
    | "eight" | "8" -> 8
    | "nine" | "9" -> 9
    | "" -> 0
    | _ -> failwith "Invalid value"

let getFirstAndLast2 (a : string list) =
    a[0], List.last a

let join (a: string, b: string) =
    let f = numberToNumber a
    let s = numberToNumber b
    sprintf "%d%d" f s
    
input.Split("\n")
|> Seq.map findNumbersInRow
|> Seq.map (getFirstAndLast2 >> join >> int)
|> List.ofSeq
|> List.sum



test <@ (findNumbersInRow >> (List.map numberToNumber)) "(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9" = [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9] @>
test <@ (findNumbersInRow >> (List.map numberToNumber)) "eighthree" = [8; 3] @>
test <@ (findNumbersInRow >> (List.map numberToNumber)) "sevenine" = [7; 9] @>
test <@ (findNumbersInRow >> (List.map numberToNumber)) "seven3nine" = [7;3;9] @>
test <@ (findNumbersInRow >> getFirstAndLast2) "2onetwone" = ("2", "one1") @>
test <@ (findNumbersInRow >> getFirstAndLast2) "2" = ("2", "2") @>
