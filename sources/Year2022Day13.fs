module Year2022Day13

open System.IO
open System.Environment

let newline = System.Environment.NewLine

let parseLine (line: string) = line

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
