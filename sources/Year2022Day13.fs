module Year2022Day13

open System.IO
open System

let newline = System.Environment.NewLine

type Packet =
    | Number of int
    | List of Packet list

let rec toNumber (number: char list) (chars: char list) = 
    match chars with 
    | c::rest when "0123456789".Contains(c) -> toNumber (c::number) rest
    | rest -> 
        let stringNumber = new String(number |> List.rev |> Array.ofList) 
        Number (stringNumber |> int), rest

let parseLine (line: string) =
    let rec parseStr (acc: Packet list) str =
        match str with
        | '[' :: rest ->
            let (list, r) = parseStr [] rest
            parseStr (List list :: acc) r
        | ',' :: rest -> parseStr acc rest
        | ']' :: rest -> (acc |> List.rev), rest
        | i :: rest -> 
            let number, newRest = toNumber [i] rest
            parseStr (number::acc) newRest
        | _ -> acc, []

    parseStr [] (line |> Seq.toList)
    |> fst
    |> Seq.exactlyOne

let parsedInputs =
    File
        .ReadAllText("inputs/day13.txt")
        .Split(newline + newline)
    |> List.ofArray
    |> List.map (fun pair ->
        let split = pair.Split(newline)
        parseLine split[0], parseLine split[1])

let rec hasRightOrder left right =
    match left, right with
    | Number l, Number r -> if l = r then None else Some(l < r)
    | List (l1 :: ls), List (r1 :: rs) ->
        match hasRightOrder l1 r1 with
        | Some isRightOrder -> Some isRightOrder
        | None -> hasRightOrder (List ls) (List rs)
    | Number l, List r -> hasRightOrder (List [ Number l ]) (List r)
    | List l, Number r -> hasRightOrder (List l) (List [ Number r ])
    | List [], List [] -> None
    | List [], r -> Some true
    | l, List [] -> Some false

let part1 =
    parsedInputs
    |> List.mapi (fun i (left, right) ->
        match hasRightOrder left right with
        | Some x -> if x then i + 1 else 0
        | None -> 0)
    |> List.sum


let part2 = 0
