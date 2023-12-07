open System
open System.IO

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
             // Try to see if there is some way to replace J with anything else
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
    
let getNumericPart hand =
    hand |> List.mapi (fun i v -> (i * 15) + (getNumericValue v)) |> List.sum

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