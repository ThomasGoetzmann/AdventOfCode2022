module Year2022Day3

open System.IO
open System

let inputs = 
    File.ReadAllLines("inputs/day3.txt")
    |> Array.ofSeq
    |> Array.map (fun line -> line.ToCharArray())

let findSameItemTypeInCompartments rucksack=
    let firstCompartment, secondCompartment = 
        rucksack 
        |> Array.splitAt (rucksack.Length / 2)
    
    firstCompartment 
    |> Seq.find (fun x -> secondCompartment |> Seq.exists(fun y -> x = y))

let priority c =
    match c with 
    | c when Char.IsLower(c) -> (c |> int) - 96 //'a' is 1 and 'z' is 26
    | c -> (c |> int) - 65 + 27 // 'A' is 27 and 'Z' is 52

let findBadge (elfRucksacks:char[][])=
    elfRucksacks 
    |> Set.ofArray
    |> Set.map Set.ofArray
    |> Set.intersectMany
    |> Seq.exactlyOne

let SolveDay3Part1 = 
    inputs
    |> Array.map findSameItemTypeInCompartments
    |> Array.sumBy priority

let SolveDay3Part2 = 
    inputs
    |> Array.splitInto (inputs.Length / 3) 
    |> Array.map findBadge
    |> Array.sumBy priority
    