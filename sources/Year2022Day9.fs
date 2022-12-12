module Year2022Day9

open System.IO
open System.Text.RegularExpressions

let inputs = File.ReadAllLines("inputs/day9.txt") |> List.ofSeq

type Position = { X: int; Y: int }

type Rope = 
    { Head: Position; 
      Tail: Position; }
      member this.Vector() = this.Head.X - this.Tail.X, this.Head.Y - this.Tail.Y

type Direction =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let parse line =
    let m =
        Regex("(?<direction>.) (?<number>\d+)")
            .Match(line)

    let number = (m.Groups["number"].Value |> int)

    match m.Groups["direction"].Value with
    | "U" -> Up number
    | "L" -> Left number
    | "R" -> Right number
    | "D" -> Down number
    | _ -> failwith "Invalid input"

let apply move position =
    match move with
    | Up (nb) -> { position with Y = position.Y + nb }
    | Down (nb) -> { position with Y = position.Y - nb }
    | Left (nb) -> { position with X = position.X - nb }
    | Right (nb) -> { position with X = position.X + nb }

let applyTailfollowing rope = 
    let rec applyMove1 acc (r:Rope) =
        match r.Vector() with
        | x , y when x < -1 -> 
            let n = { r.Head with X = r.Tail.X - 1 } 
            applyMove1 (n::acc) {Head = r.Head; Tail = n}
        | x , y when x > 1 -> 
            let n = { r.Head with X = r.Tail.X + 1 } 
            applyMove1 (n::acc) {Head = r.Head; Tail = n}
        | x , y when y < -1 -> 
            let n = { r.Head with Y = r.Tail.Y - 1 } 
            applyMove1 (n::acc) {Head = r.Head; Tail = n}
        | x , y when y > 1 -> 
            let n = { r.Head with Y = r.Tail.Y + 1 } 
            applyMove1 (n::acc) {Head = r.Head; Tail = n}
        | _ ->  acc
    
    applyMove1 [] rope

let part1 =
    let Rope = {  Head = { X = 0; Y = 0 } ; Tail = { X = 0; Y = 0 } }

    ((Rope, [{X =0; Y = 0}]), inputs |> List.map parse)
    ||> Seq.fold (fun (rope, positions) move ->
        let newHead = rope.Head |> apply move
        let newTails = { rope with Head = newHead } |> applyTailfollowing

        if newTails = [] 
        then { Head = newHead; Tail = rope.Tail }, positions
        else { Head = newHead; Tail = newTails.Head }, positions |> List.append newTails
        
    )
    |> snd
    |> Seq.distinct
    |> Seq.length
//probably some fold or reduce or mapFold ...

let part2 = 0
