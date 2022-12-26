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

let rec comparer left right =
    match left, right with
    | Number l, Number r -> 
        if l = r then 0 
        elif l < r then -1
        else 1
    | List (l1 :: ls), List (r1 :: rs) ->
        match comparer l1 r1 with
        | 0 -> comparer (List ls) (List rs)
        | isRightOrder -> isRightOrder
    | Number l, List r -> comparer (List [ Number l ]) (List r)
    | List l, Number r -> comparer (List l) (List [ Number r ])
    | List [], List [] -> 0
    | List [], r -> -1
    | l, List [] -> 1

let part1 =
    parsedInputs
    |> List.mapi (fun i (left, right) ->
        match comparer left right with
        | -1 -> i + 1
        | _ -> 0)
    |> List.sum

let divider x =
    List [List [Number x]]

let part2 =
    let d2 = divider 2
    let d6 = divider 6

    let orderedPackets = 
        parsedInputs
        |> List.collect (fun (left,right) -> [left; right])
        |> List.append [d2; d6]
        |> List.sortWith comparer

    let indexD2 = orderedPackets |> List.findIndex (fun packet -> packet = d2)
    let indexD6 = orderedPackets |> List.findIndex (fun packet -> packet = d6)

    (indexD2 + 1) * (indexD6 + 1)