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
    | Up -> x, y + 1
    | Down -> x, y - 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let follow head tail =
    let xh, yh = head
    let xt, yt = tail
    let xdiff, ydiff = xh - xt, yh - yt

    match xdiff, ydiff with
    | 2, 2 -> xt + 1, yt + 1
    | 2, -2 -> xt + 1, yt - 1
    | -2, 2 -> xt - 1, yt + 1
    | -2, -2 -> xt - 1, yt - 1

    | 2, _ -> xt + 1, yh
    | -2, _ -> xt - 1, yh

    | _, 2 -> xh, yt + 1
    | _, -2 -> xh, yt - 1

    | _ -> tail

let rec applyMoves acc rope moves =
    let applyMove rope move =
        let rec followRope acc knots =
            match knots with
            | knot :: remainingKnots ->
                let movedKnot = knot |> follow (acc |> List.head)
                followRope (movedKnot :: acc) remainingKnots
            | [] -> acc |> List.rev

        let newHead = (rope |> List.head) |> apply move
        followRope [ newHead ] rope.Tail

    match moves with
    | move :: remainingMove ->
        let newRope = applyMove rope move
        applyMoves (newRope :: acc) newRope remainingMove
    | [] -> acc |> List.rev

let solve ropeSize =
    let knot = (0, 0) 
    let rope = knot |> List.replicate ropeSize

    inputs
    |> List.collect parse
    |> applyMoves [ rope ] rope
    |> List.map List.last
    |> List.distinct
    |> List.length

let part1 = solve 2

let part2 = solve 10
