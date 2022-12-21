module Year2022Day10

open System.IO
open System.Text.RegularExpressions

let inputs = File.ReadAllLines("inputs/day10.txt")

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

type Instruction =
    | Noop
    | Add of int

type State =
    { Tick: int
      X: int }

    member this.Sprite() = [ this.X - 1; this.X; this.X + 1 ]

let parse input =
    match input with
    | "noop" -> Noop
    | ParseRegex "addx (?<number>-?\d+)" [ number ] -> Add(number |> int)
    | s -> failwith (sprintf "Invalid input %s" s)

let execute states instruction =
    let last = states |> List.head
    let tick s = { s with Tick = s.Tick + 1 }

    match instruction with
    | Noop -> tick last :: states
    | Add x ->
        let noop = tick last
        let op = { tick noop with X = noop.X + x }
        op :: noop :: states

let states =
    let initialState = [ { Tick = 1; X = 1 } ]
    let instructions = inputs |> Seq.map parse
    (initialState, instructions) ||> Seq.fold execute

let part1 =
    states
    |> Seq.filter (fun x ->
        [ 20; 60; 100; 140; 180; 220 ]
        |> List.contains x.Tick)
    |> Seq.sumBy (fun x -> x.Tick * x.X)

let display crt =
    crt
    |> List.map (List.toArray >> System.String)
    |> String.concat System.Environment.NewLine

let part2 =
    let emptyCRT = ('.' |> List.replicate 40) |> List.replicate 6

    emptyCRT
    |> List.mapi (fun rowNumber line ->
        line
        |> List.mapi (fun i _ ->
            let currentTick = i + 1 + rowNumber * 40

            let currentState =
                states
                |> List.find (fun s -> s.Tick = currentTick)

            let pixel =
                if currentState.Sprite() |> List.contains i then
                    '#'
                else
                    '.'

            pixel))
    |> display
