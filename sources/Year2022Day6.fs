module Year2022Day6
    open System.IO
    
    let inputs =
        File.ReadAllText("inputs/day6.txt") 
        |> List.ofSeq
    
    let add x y = x + y

    let analyze size input  =
        input 
        |> List.windowed size
        |> List.findIndex (fun x -> (x |> List.distinct |> List.length) = size)
        |> add size

    let packet = 4
    let message = 14

    let part1 = inputs |> analyze packet
    let part2 = inputs |> analyze message