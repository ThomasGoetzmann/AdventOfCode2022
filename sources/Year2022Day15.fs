module Year2022Day15

open System.IO
open System.Text.RegularExpressions

let inputs =
    File.ReadAllLines("inputs/day15.txt")
    |> List.ofArray

type Sensor =
    { Position: int64 * int64
      Beacon: int64 * int64 }
    
    member this.ManhattanDistance() = 
        ((this.Position |> fst) - (this.Beacon |> fst) |> abs)
        + ((this.Position |> snd) - (this.Beacon |> snd) |> abs)

    member this.DistanceFromRow(y) =
        ((this.Position |> snd) - y )|> abs

let parse line =
    let regex =
        Regex("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)", RegexOptions.Compiled)

    let values =
        (regex.Match line).Groups.Values
        |> List.ofSeq
        |> List.tail
        |> List.map (fun g -> g.Value |> int64)

    { Position = values[0], values[1]
      Beacon = values[2], values[3] }

let impossibleBeaconsAtRow y (sensor:Sensor) =
    let spread = sensor.ManhattanDistance() - sensor.DistanceFromRow(y)

    if spread <= 0L 
        then []
    else
        let x = sensor.Position |> fst
        [x- spread..x + spread]

let part1 = 
    let parsedInputs = 
        inputs 
        |> List.map parse

    let beaconsAt row = 
        parsedInputs
        |> List.map (fun b -> b.Beacon)
        |> List.filter (fun (_,y) -> y = row)
        |> List.distinctBy fst
        |> List.length
    
    let row = 2000000

    parsedInputs
    |> List.map (impossibleBeaconsAtRow row)
    |> List.collect id
    |> List.distinct
    |> List.sortBy id
    |> List.length
    |> (+) -(beaconsAt row)

let part2 = 0
