module Year2022Day13

open System.IO

let newline = System.Environment.NewLine

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0

    if System.Int32.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

let parseLine (line:string) =
    
    let rec parseStr (acc: list) (str : string) =
        match str with
        | '['::rest -> 
            let list, rest = parseStr [] rest 
            parseStr  (list @ acc) rest
        | ']'::rest -> (acc |> List.rev) rest
        | ','::rest -> parseStr acc rest
        | i::rest when Integer i -> parseStr (i::acc) rest 
        | _ -> acc, []

    parseStr (line |> Seq.toList) []

let inputs =
    File
        .ReadAllText("inputs/day13.txt")
        .Split(newline + newline)
    |> List.ofArray
    |> List.map (fun pair ->
        pair.Split(newline)
        |> List.ofArray
        |> List.map parseLine)


let part1 = inputs
let part2 = 0
