module Year2022Day5

open System.IO
open System

let inputs = 
    File.ReadAllLines("inputs/day5.txt")
    |> List.ofSeq
    
let splitUntil lineValue list =    
    let rec split acc list =
        match list with
        | [] -> (acc |> List.rev),[]
        | x::xs -> if x = lineValue then (acc |> List.rev),xs else split (x::acc) xs

    split [] list

let parseStacks (stacks:string list) = 
    stacks 
    |> List.rev 
    |> Seq.map (fun line -> line |> Seq.chunkBySize 4 |> Seq.map(fun x -> x.[1]) |> Seq.toList)
    |> List.transpose 
    |> Array.ofList

let parseMoves moves = moves

let SolveDay5Part1 = 
    let stacks,moves = inputs |> splitUntil ""
    stacks |> parseStacks//,(moves |> parseMoves)
    
let SolveDay5Part2 = 0