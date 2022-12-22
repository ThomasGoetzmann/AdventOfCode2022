module Year2022Day12

open System.IO

let inputs =
    File.ReadAllLines("inputs/day12.txt") 
    |> List.ofSeq 
    |> List.map (fun x -> x |> List.ofSeq)

let part1 = 
    inputs 
    


let part2 = 0