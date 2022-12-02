module Year2022Day2

open System.IO

let inputs = 
    File.ReadAllLines("inputs/day2.txt")

type Shape = 
    | Rock = 1
    | Paper = 2
    | Scissors = 3

type Round = {
    Opponent : Shape
    You : Shape
}

type Outcome =
    | Win = 6
    | Draw = 3
    | Loss = 0

let parseRound (input:string) = 
    {
        Opponent =  
            match input[0] with
            | 'A' -> Shape.Rock
            | 'B' -> Shape.Paper
            | 'C' -> Shape.Scissors
            | _ -> failwith "Invalid Shape for opponent"
        You = 
            match input[2] with
            | 'X' -> Shape.Rock
            | 'Y' -> Shape.Paper
            | 'Z' -> Shape.Scissors
            | _ -> failwith "Invalid Shape for yourself"
    }

let getScore round =
    let outcome=
        match round with
        | {Opponent = o: Shape; You = y} when 
            (o = Shape.Rock && y = Shape.Paper)
            || (o = Shape.Paper && y = Shape.Scissors )
            || (o = Shape.Scissors && y = Shape.Rock) -> Outcome.Win
        | r when r.Opponent = r.You -> Outcome.Draw
        | _ -> Outcome.Loss

    (int round.You) + (int outcome)


let SolveDay2Part1 = 
    inputs
    |> Seq.sumBy (fun line -> line |> parseRound |> getScore)
