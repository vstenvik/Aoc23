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

let getNumericValue card = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> 11
    | T -> 10
    | NumericCard v -> v

type Hand = Card list

let (|FiveOfAKind|_|) (hand: Hand) =
    match hand with
    | first::rest when rest.Length = 4-> if List.forall ((=) first) rest
                                            then Some(hand)
                                            else None
    | _ -> None

let (|FourOfAKind|_|) (hand: Hand) =
    option {
        let! fourOfAKindHand = hand
                                |> List.groupBy id
                                |> List.tryFind (snd >> _.Length >> ((=) 4))
        return snd fourOfAKindHand
    }
    
let (|FullHouse|_|) (hand: Hand) =
    option {
        let! threeOfAKind = hand
                            |> List.groupBy id
                            |> List.tryFind (snd >> _.Length >> ((=) 3))
        let! twoOfAKind = hand
                            |> List.groupBy id
                            |> List.tryFind (snd >> _.Length >> ((=) 3))
        return (snd threeOfAKind, snd twoOfAKind)
    }
    
let (|TwoPair|_|) (hand: Hand) =
        let counts = hand
                       |> List.groupBy id
                       |> List.map (snd >> _.Length)
                       |> List.sortDescending
                       
        printf "%A" counts
        match counts with
        | [2;2;1] -> Some hand
        | _ -> None
        
let (|TwoPair|_|) (hand: Hand) =
        let counts = hand
                       |> List.groupBy id
                       |> List.map (snd >> _.Length)
                       |> List.sortDescending
                       
        printf "%A" counts
        match counts with
        | [2;2;1] -> Some hand
        | _ -> None

let getType hand =
    match hand with
    | FiveOfAKind h -> "five of a kind"
    | FourOfAKind h -> "four of a kind"
    | FullHouse h -> "full house"
    | TwoPair h -> "two pair"
    | OnePair h -> "two pair"
    | _ -> "nothing"

test <@ getType [A;A;A;A;A] = "five of a kind" @>
test <@ getType [K;A;A;A;A] = "four of a kind" @>
test <@ getType [A;A;A;K;K] = "full house" @>
test <@ getType [A;A;T;K;K] = "two pair" @>
