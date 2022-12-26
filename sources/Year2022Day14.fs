module Year2022Day14

open System.IO

let inputs = File.ReadAllLines("inputs/day14.txt")

let parse (line: string) =
    let parsePoint (pointString: string) =
        let split = pointString.Split(',')
        split[0] |> int, split[1] |> int

    line.Split(" -> ")
    |> List.ofArray
    |> List.map parsePoint

let createlist i j = if i < j then [ i..j ] else [ j..i ]

let drawPoints (a, b) =
    let xs = createlist (fst a) (fst b)
    let ys = createlist (snd a) (snd b)
    ys |> List.allPairs xs

let draw path =
    path
    |> List.pairwise
    |> List.map drawPoints
    |> List.collect id

let drizzleSandOver rocks =
    let initialPosition = 500, 0
    let bottom = rocks |> List.maxBy snd |> snd

    let rec drizzle (x, y) occupied =
        if y > bottom then
            None
        elif occupied |> List.contains (x, y + 1) |> not then
            drizzle (x, y + 1) occupied
        elif occupied |> List.contains (x - 1, y + 1) |> not then
            drizzle (x - 1, y + 1) occupied
        elif occupied |> List.contains (x + 1, y + 1) |> not then
            drizzle (x + 1, y + 1) occupied
        else
            Some(x, y)

    let rec makeItRainOver rocksAndSands =
        match rocksAndSands |> drizzle initialPosition with
        | Some sand -> makeItRainOver (sand :: rocksAndSands)
        | None -> rocksAndSands

    makeItRainOver rocks

let part1 =
    let rocks =
        inputs
        |> List.ofArray
        |> List.map parse
        |> List.map draw
        |> List.collect id
        |> List.distinct

    let rocksAndSands = drizzleSandOver rocks

    (rocksAndSands |> List.length)
    - (rocks |> List.length)

let part2 = 0
