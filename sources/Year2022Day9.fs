module Year2022Day9
    open System.IO
    open System.Text.RegularExpressions

    let inputs = 
        File.ReadAllLines("inputs/day9.txt")
        |> List.ofSeq

    type Direction =
        | Up of int
        | Down of int
        | Left of int
        | Right of int

    let parse line =
        let m = Regex("(?<direction>.) (?<number>\d+)").Match(line)
        let number = (m.Groups["number"].Value |> int) 

        match m.Groups["direction"].Value with
        | "U" -> Up number
        | "L" -> Left number
        | "R" -> Right number
        | "D" -> Down number
        | _ -> failwith "Invalid input"

    let move (x,y) move =
        match move with 
        | Up(nb) -> x, y + nb
        | Down(nb) -> x,y - nb
        | Left(nb) -> x - nb,y
        | Right(nb) -> x + nb,y

    let following (tx,ty) (hx,hy) =
        (tx,ty) //TODO


    let part1 = 
        let head = (0,0)
        let tail = (0,0)

        inputs
        |> List.map parse
        //probably some fold or reduce or mapFold ...

    let part2 = 0