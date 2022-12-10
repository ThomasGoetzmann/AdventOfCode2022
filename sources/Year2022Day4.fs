module Year2022Day4
    open System.IO

    let inputs = File.ReadAllLines("inputs/day4.txt")

    let parse (line: string) =
        let splits = line.Split(',')

        let firstElf = splits.[0].Split('-')
        let secondElf = splits.[1].Split('-')

        let firstSections =
            [ (firstElf.[0] |> int) .. (firstElf.[1] |> int) ]

        let secondSections =
            [ (secondElf.[0] |> int) .. (secondElf.[1] |> int) ]

        (firstSections, secondSections)

    let fullyContains (range1, range2) =
        let set1 = range1 |> Set.ofList
        let set2 = range2 |> Set.ofList

        (set1 |> Set.isSubset set2)
        || (set1 |> Set.isSuperset set2)

    let overlap (range1, range2) =
        let set1 = range1 |> Set.ofList
        let set2 = range2 |> Set.ofList

        set1 |> Set.intersect set2 |> Set.isEmpty |> not


    let part1 =
        inputs
        |> Seq.map parse
        |> Seq.filter fullyContains
        |> Seq.length

    let part2 =
        inputs
        |> Seq.map parse
        |> Seq.filter overlap
        |> Seq.length
