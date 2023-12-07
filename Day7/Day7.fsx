open System
open System
open System.IO
open System.Text.RegularExpressions

#r "nuget: FsToolkit.ErrorHandling, 4.11.1"
#r "nuget: Unquote"
open Swensen.Unquote
open FsToolkit.ErrorHandling


type Card =
    | A
    | K
    | Q
    | J
    | T
    | NumericCard of int

let getNumericValue = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> 1
    | T -> 10
    | NumericCard v -> v

type Hand = Card list
type HandType =
    | FiveOfAKind of Hand
    | FourOfAKind of Hand
    | FullHouse of Hand
    | ThreeOfAKind of Hand
    | TwoPair of Hand
    | OnePair of Hand
    | HighCard of Hand

let upgrade (hand : Hand): Hand =
    [ for card in hand do
          if card = J then
             let withoutJ = hand |> Seq.filter ((<>) J)
             if Seq.length withoutJ > 0 then
                withoutJ |> Seq.countBy id |> Seq.maxBy snd |> fst
             else
                hand |> Seq.countBy id |> Seq.maxBy snd |> fst
          else
              card]

upgrade [J;J;J;T;T]

let getHandType (hand : Hand) =
    let counts = (upgrade hand)
                |> List.countBy id
                |> List.map (snd)
                |> List.sortDescending
    match counts with
    | [5] -> FiveOfAKind hand
    | [4; 1] -> FourOfAKind hand
    | [3; 2] -> FullHouse hand
    | [3; 1; 1] -> ThreeOfAKind hand
    | [2; 2; 1] -> TwoPair hand
    | [2; 1; 1; 1] -> OnePair hand
    | [1; 1; 1; 1; 1] -> HighCard hand
    | _ -> failwithf "Impossible hand %A" hand
    
let getType hand =
    match getHandType hand with
    | FiveOfAKind h -> "five of a kind"
    | FourOfAKind h -> "four of a kind"
    | FullHouse h -> "full house"
    | ThreeOfAKind h -> "three of a kind"
    | TwoPair h -> "two pair"
    | OnePair h -> "one pair"
    | HighCard h -> "high card"
    
let getNumericPart hand =
    hand |> List.mapi (fun i v -> (i * 15) + (getNumericValue v)) |> List.sum
let getScore hand =
    match getHandType hand with
    | FiveOfAKind h -> (getNumericValue(h[0]) * (pown 10 8)) + (getNumericPart h)
    | FourOfAKind h -> (pown 10 8) + (getNumericPart h)
    | FullHouse h -> (pown 10 8) + (getNumericPart h)
    | ThreeOfAKind h -> (pown 10 8) + (getNumericPart h)
    | TwoPair h -> (pown 10 8) + (getNumericPart h)
    | OnePair h -> (pown 10 8) + (getNumericPart h)
    | HighCard h -> getNumericPart h


let getTypeScore = function
    | FiveOfAKind _ -> 100
    | FourOfAKind _ -> 90
    | FullHouse _ -> 80
    | ThreeOfAKind _ -> 70
    | TwoPair _ -> 60
    | OnePair _ -> 50
    | HighCard _ -> 0

let compareHands (handA : Hand * int) (handB : Hand * int) =
    match (getHandType (fst handA), getHandType (fst handB)) with
    // Same type
    | a, b when (getTypeScore a) = (getTypeScore b) ->
        compare (List.map getNumericValue (fst handA)) (List.map getNumericValue (fst handB))
    // Diff types
    | a, b ->
        compare (getTypeScore a) (getTypeScore b)
let sort (hands : (Hand * int) list ) =
    hands
    |> List.sortWith (compareHands)


test <@ getType [A;A;A;A;A] = "five of a kind" @>
test <@ getType [K;A;A;A;A] = "four of a kind" @>
test <@ getType [A;A;A;K;K] = "full house" @>
test <@ getType [K;Q;A;A;A] = "three of a kind" @>
test <@ getType [A;A;T;K;K] = "two pair" @>
test <@ getType [A;A;T;K;Q] = "one pair" @>
test <@ getType [A;NumericCard 2;T;K;Q] = "high card" @>

let test1 = [
    [A;A;A;A;A]
    [K;A;A;A;A]
    [A;A;A;K;K]
    [NumericCard 3;Q;A;A;A]
    [NumericCard 2;Q;A;A;A]
    [A;A;T;K;K]
    [A;A;T;K;Q]
]
let test2 = [
    [5;4;2;3;1] |> List.map NumericCard
    [4;5;3;2;1] |> List.map NumericCard
    [5;4;3;2;1] |> List.map NumericCard
]
let test2sorted = [
    [5;4;3;2;1] |> List.map NumericCard
    [5;4;2;3;1] |> List.map NumericCard
    [4;5;3;2;1] |> List.map NumericCard
]
//test <@ test1 |> sort = test1 @>
//test <@ test2 |> sort = test2sorted @>


let example = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

let parseHand (str:string) =
    str
    |> Seq.map (function
                | 'A' -> A
                | 'K' -> K
                | 'Q' -> Q
                | 'J' -> J
                | 'T' -> T
                | c when Char.IsNumber c -> NumericCard (String [|c|] |> int)
                | v -> failwithf "%c is not a valid input" v)
    |> List.ofSeq
    
let parse (str:string) =
    str.Split("\n")
    |> Array.map (_.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) >> (fun v -> parseHand v.[0], int v.[1]))
    |> List.ofArray
    
let input = File.ReadAllText "./Day7/input.txt"

parse input
|> sort
|> Seq.mapi (fun i (_,b) -> (i + 1) * b)
|> Seq.sum