module Year2022Day1
    open System.IO

    let inputs = 
        File.ReadAllText("inputs/day1.txt").Split(System.Environment.NewLine + System.Environment.NewLine)
        |> Seq.map(fun str -> str.Split(System.Environment.NewLine)|> Seq.map int)

    let caloriesByElf inputs = 
        inputs 
        |> Seq.map Seq.sum

    let part1 = 
        inputs 
        |> caloriesByElf
        |> Seq.max

    let part2 = 
        inputs
        |> caloriesByElf
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum