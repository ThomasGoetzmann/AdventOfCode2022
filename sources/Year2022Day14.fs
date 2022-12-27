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

let drizzleSandOver rocks fallsEndlessly hitBottom =
    let initialPosition = 500, 0

    let rec drizzle occupied sandPath = 
        match sandPath with
        | p::_ when fallsEndlessly p ->  None, []
        | p::rest when hitBottom p -> Some p, rest
        | (x, y) ::_ when not (occupied |> Seq.contains (x, y+1) ) -> drizzle occupied ((x, y + 1)::sandPath)
        | (x, y) ::_ when not (occupied |> Seq.contains (x-1, y+1) ) -> drizzle occupied ((x - 1, y + 1) ::sandPath)
        | (x, y) ::_ when not (occupied |> Seq.contains (x+1, y+1) ) -> drizzle occupied ((x + 1, y + 1) ::sandPath)
        | (x, y) ::rest  -> Some (x,y), rest
        | [] -> None, []

    let rec makeItRainOver rocksAndSands sandPath =
        match drizzle rocksAndSands sandPath with
        | Some sand, p -> makeItRainOver (sand :: rocksAndSands) p
        | _ -> rocksAndSands

    makeItRainOver rocks [initialPosition]

let part1 =
    let rocks =
        inputs
        |> List.ofArray
        |> List.map parse
        |> List.map draw
        |> List.collect id
        |> List.distinct

    let hitBottom (x,y) = false
    let fallsEndlessly (x,y) = 
        let bottom = rocks |> List.maxBy snd |> snd
        y > bottom
    
    let rocksAndSands = drizzleSandOver rocks fallsEndlessly hitBottom

    (rocksAndSands |> Seq.length)
    - (rocks |> Seq.length)

let part2 =
    let rocks =
        inputs
        |> List.ofArray
        |> List.map parse
        |> List.map draw
        |> List.collect id
        |> List.distinct

    let fallsEndlessly (x,y) = false
    let hitBottom (x,y) = 
        let bottom = (rocks |> Seq.maxBy snd |> snd) + 2
        y + 1 = bottom

    let rocksAndSands = drizzleSandOver rocks fallsEndlessly hitBottom

    (rocksAndSands |> Seq.length)
    - (rocks |> Seq.length)
