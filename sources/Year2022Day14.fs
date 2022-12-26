module Year2022Day14

open System.IO
open System.Text.RegularExpressions

let inputs = File.ReadAllLines("inputs/day14test.txt")

let parse (line:string) =
    let parsePoint (pointString:string) =
        let split = pointString.Split(',')
        split[0] |> int , split[1] |> int
    
    line.Split(" -> ")
    |> List.ofArray
    |> List.map parsePoint

let createlist i j =
    if i < j then [i..j] else [j..i]

let drawPoints (a,b) =
    let xs = createlist (fst a) (fst b)
    let ys = createlist (snd a) (snd b)
    xs |> List.allPairs ys
  
let draw path =
    path 
    |> List.pairwise
    |> List.map drawPoints
    |> List.collect id

let part1 = 
    inputs
    |> List.ofArray
    |> List.map parse
    |> List.map draw
    |> List.collect id
    |> List.distinct

let part2 = 0