#r "nuget: Unquote"
open System
open System.IO
open Swensen.Unquote

let validatePass (str: string) =
    let words =
        str.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

    let uniqueWords = Set.ofArray words
    (Seq.length words) = (Seq.length uniqueWords)
    
let sortWord (str:string) = str |> Array.ofSeq |> Array.sort |> String
let validatePass2 (str: string) =
    let words =
        str.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    let sortedWords = Seq.map sortWord words
    let uniqueSortedWords = Set.ofSeq sortedWords
    (Seq.length sortedWords) = (Seq.length uniqueSortedWords)
        
test <@"aa bb cc dd ee" |> validatePass = true @>
test <@"aa bb cc dd aa" |> validatePass = false @>
test <@"aa bb cc dd aaa" |> validatePass = true @>

test <@"abcde fghij" |> validatePass2 = true @>
test <@"a ab abc abd abf abj" |> validatePass2 = true @>
test <@"abcde xyz ecdab" |> validatePass2 = false @>

let input = File.ReadAllLines "./2015/Day4.txt"

input |> Seq.filter validatePass |> Seq.length
input |> Seq.filter validatePass2 |> Seq.length
