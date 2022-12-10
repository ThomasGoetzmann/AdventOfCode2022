module Year2022Day5

open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic

let inputs =
    File.ReadAllLines("inputs/day5.txt") |> List.ofSeq

let splitUntil lineValue list =
    let rec split acc list =
        match list with
        | [] -> (acc |> List.rev), []
        | x :: xs ->
            if x = lineValue then
                (acc |> List.rev), xs
            else
                split (x :: acc) xs

    split [] list

let parseStacks (stacks: string list) =
    stacks
    |> List.rev
    |> List.map
        (fun line ->
            line
            |> Seq.chunkBySize 4
            |> Seq.map (fun x -> x.[1])
            |> List.ofSeq)
    |> List.transpose
    |> List.map (
        List.choose
            (fun z ->
                match z with
                | ' ' -> None
                | y -> Some(y))
        >> List.tail
    )

type Move = {
    Amount : int
    From : int
    To : int
}

let parseMoves moves = 
    let parseLine line =
        let m = Regex.Match(line, "move (\d+) from (\d+) to (\d+)")
        { Amount = (int)m.Groups[1].Value; From = (int)m.Groups[2].Value; To = (int)m.Groups[3].Value }

    moves |> List.map parseLine

let rec apply crane moves stacks = 
    let applyMove move (s:char list list) = 
        let fromIndex = move.From - 1
        let toIndex = move.To - 1
        let moving, staying = s[fromIndex] |> List.rev |> List.splitAt move.Amount

        s 
        |> List.updateAt fromIndex (staying |> List.rev)
        |> List.updateAt toIndex (s[toIndex] @ (moving |> crane))

    match moves with
    | move::remainingMoves -> apply crane remainingMoves (applyMove move stacks)
    | [] -> stacks

let crane9000 stacks = stacks
let crane9001 stacks = stacks |> List.rev
let cratesOnTop stacks = stacks |> List.map (List.rev >> List.head)

let rawStacks, rawMoves = inputs |> splitUntil ""
let stacks = rawStacks |> parseStacks
let moves = rawMoves |> parseMoves

let SolveDay5Part1 =
    stacks
    |> apply crane9000 moves
    |> cratesOnTop

let SolveDay5Part2 = 
    stacks
    |> apply crane9001 moves
    |> cratesOnTop
