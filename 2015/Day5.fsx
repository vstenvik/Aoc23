#r "nuget: Unquote"
open System
open System.IO
open Swensen.Unquote

let isOutside i arr =
    i < 0 || i >= Array.length arr

test <@ Array.create 3 0 |> isOutside 4 |> (=) true @>
test <@ Array.create 3 0 |> isOutside -1 |> (=) true @>
test <@ Array.create 3 0 |> isOutside 3 |> (=) true @>
test <@ Array.create 3 0 |> isOutside 2 |> (=) false @>

let jump i (arr : int array) =
    let value = arr.[i]
    arr[i] <- (value + 1)
    arr, i + value

let jump2 i (arr : int array) =
    let value = arr.[i]
    if value >= 3 then
        arr[i] <- (value - 1)
    else
        arr[i] <- (value + 1)
    arr, i + value
    
test <@ [|1; 2; 3|] |> jump 0 |> (=) ([|2;2;3|], 1) @>
test <@ [|2; 2; 3|] |> jump 1 |> (=) ([|2;3;3|], 3) @>
test <@ [|0; 2; 3|] |> jump 0 |> (=) ([|1;2;3|], 0) @>
test <@ [|0; 2; -2|] |> jump 2 |> (=) ([|0;2;-1|], 0) @>
test <@ [|1; 2; 3|] |> jump2 0 |> (=) ([|2;2;3|], 1) @>
test <@ [|2; 2; 3|] |> jump2 1 |> (=) ([|2;3;3|], 3) @>
test <@ [|0; 2; 3|] |> jump2 0 |> (=) ([|1;2;3|], 0) @>
test <@ [|0; 2; -2|] |> jump2 2 |> (=) ([|0;2;-1|], 0) @>
test <@ [|3; 2; -2|] |> jump2 0 |> (=) ([|2;2;-2|], 3) @>
test <@ [|3; 2; -3|] |> jump2 2 |> (=) ([|3;2;-2|], -1) @>

test <@ [|0; 3; 0; 1; -3|] |> jump2 0 |> (=) ([|1; 3; 0; 1; -3|], 0) @>

let parseVertical (str: string) =
    let flags = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    str.Split(" ", flags)
    |> Array.map int

let example = parseVertical "0 3  0  1  -3"
let solvePart1 (input : int array) =
    let array = Array.copy input
    let mutable index = 0
    let mutable steps = 0
    while isOutside index array |> not do
        steps <- steps + 1
        let _, i = jump index array
        index <- i
    steps

test <@ solvePart1 example = 5 @>

let solvePart2 (input : int array) =
    let array = Array.copy input
    let mutable index = 0
    let mutable steps = 0
    while isOutside index array |> not do
        steps <- steps + 1
        let _, i = jump2 index array
        index <- i
    steps, array

test <@ solvePart2 example = (10, [|2; 3; 2; 3; -1|]) @>
  
let input = File.ReadAllLines "./2015/Day5.txt" |> Array.map int
           
solvePart1 input
solvePart2 input
