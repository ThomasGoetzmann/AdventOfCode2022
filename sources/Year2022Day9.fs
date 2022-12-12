module Year2022Day9

open System.IO
open System.Text.RegularExpressions

let inputs = File.ReadAllLines("inputs/day9.txt") |> List.ofSeq

type Direction =
    | Up
    | Down
    | Left
    | Right

let parse line =
    let m =
        Regex("(?<direction>.) (?<number>\d+)")
            .Match(line)

    let number = (m.Groups["number"].Value |> int)

    match m.Groups["direction"].Value with
    | "U" -> Up |> List.replicate number
    | "L" -> Left |> List.replicate number
    | "R" -> Right |> List.replicate number
    | "D" -> Down |> List.replicate number
    | _ -> failwith "Invalid input"

let moveHead move position =
    let x, y = position
    match move with
    | Up -> x , y + 1
    | Down -> x , y - 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let moveTail pos1 pos2 =
    let x1, y1 = pos1
    let x2, y2 = pos2
    let xdiff, ydiff = x1 - x2 , y1 - y2
    
    match xdiff, ydiff with
    | 2 , _ -> x2 + 1, y1
    | -2 , _ -> x2 - 1, y1
    | _ , 2 -> x1, y2 + 1
    | _ , -2 -> x1, y2 - 1
    | _ -> pos2

let applyMoves moves = 
    let rec applyMove acc head tail moves =
        match moves with
        | m :: ms -> 
            let newHead = head |> moveHead m
            let newTail = moveTail newHead tail
            applyMove (newTail::acc) newHead newTail ms
        | [] -> acc
    
    applyMove [0,0] (0,0) (0,0) moves

let part1 =
    inputs 
    |> List.collect parse
    |> applyMoves
    |> List.distinct
    |> List.length

let part2 = 
    inputs 
    |> List.collect parse
    |> applyMoves
