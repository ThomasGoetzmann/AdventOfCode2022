module Year2022Day10
    open System.IO
    open System.Text.RegularExpressions

    let inputs = File.ReadAllLines("inputs/day10.txt")

    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    type Instruction = 
        | Noop
        | Add of int
    
    type State = 
        { Tick: int; 
        Value: int}

    let parse input = 
        match input with 
        | "noop" -> Noop
        | ParseRegex "addx (?<number>-?\d+)" [number] -> Add (number |> int)
        | s -> failwith (sprintf "Invalid input %s" s)

    let execute states instruction = 
        let last = states |> List.head
        let tick s = {s with Tick= s.Tick + 1} 

        match instruction with
        | Noop -> tick last::states
        | Add x -> 
            let one = tick last
            let two = {tick one with Value = one.Value + x}
            two::one::states

    let instructions = 
        inputs 
        |> Seq.map parse

    let initialState = [{Tick=1; Value=1}]

    let part1 = 
        (initialState, instructions)
        ||> Seq.fold execute
        |> Seq.rev
        |> Seq.filter (fun x -> [20;60;100;140;180;220]  |> List.contains x.Tick )
        |> Seq.sumBy (fun x -> x.Tick * x.Value)
    
    let part2 = 0