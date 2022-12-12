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

let apply move position =
    let x, y = position
    match move with
    | Up -> x , y + 1
    | Down -> x , y - 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let follow pos1 pos2 =
    let x1, y1 = pos1
    let x2, y2 = pos2
    let xdiff, ydiff = x1 - x2 , y1 - y2
    
    match xdiff, ydiff with
    | 2 , 2 ->  x2 + 1, y2 + 1
    | 2 , -2 -> x2 + 1, y2 - 1
    | -2 , 2 -> x2 - 1, y2 + 1
    | -2 , -2 -> x2 - 1, y2 - 1
    | 2 , _ -> x2 + 1, y1
    | -2 , _ -> x2 - 1, y1
    | _ , 2 -> x1, y2 + 1
    | _ , -2 -> x1, y2 - 1
    | _ -> pos2

let rec applyMoves a rope moves =
    
    let applyMove rope move  = 
    
        let rec followRope acc knots =
            match knots with
            | k::ks -> 
                let movedKnot = k |> follow (acc |> List.head)
                followRope (movedKnot::acc) ks
            | [] -> acc |> List.rev
        
        let newHead = (rope |> List.head) |> apply move
        followRope [newHead] rope.Tail

    match moves with 
    | m::ms -> 
        let newRope = applyMove rope m
        applyMoves (newRope::a) newRope ms
    | [] -> a |> List.rev


let part1 =
    let rope = (0,0) |> List.replicate 2
    
    inputs 
    |> List.collect parse
    |> applyMoves [rope] rope
    |> List.map (fun rope -> rope |> List.last)
    |> List.distinct
    |> List.length

let part2 = 
    let rope = (0,0) |> List.replicate 10
    
    inputs 
    |> List.collect parse
    |> applyMoves [rope] rope
    |> List.map (fun rope -> rope |> List.last)
    |> List.distinct
    |> List.length
