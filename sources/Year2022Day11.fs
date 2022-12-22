module Year2022Day11

open System.IO
open System.Text.RegularExpressions

let newline = System.Environment.NewLine

let inputs =
    File
        .ReadAllText("inputs/day11.txt")
        .Split(newline + newline)
    |> List.ofArray
    |> List.map (fun monkeyInfo -> monkeyInfo.Split(newline) |> List.ofArray)

type OperationValue =
    | Old
    | Value of int

type Operator =
    | Add
    | Multiply

type Operation =
    { Operator: Operator
      Value1: OperationValue
      Value2: OperationValue }

type Test =
    { DivisibleBy: int64
      TrueTarget: int
      FalseTarget: int }

type Monkey =
    { Number: int
      Items: int64 List
      Operation: Operation
      Test: Test
      Inspected: int64 }

let parseNumber line =
    Regex.Match(line, "Monkey (?<number>\d):").Groups["number"]
        .Value
    |> int

let removeSubString (substring: string) (s: string) =
    let index = s.IndexOf(substring)
    s.Remove(index, substring.Length)

let parseItems (line: string) =
    let items = line |> removeSubString "Starting items: "

    items.Split(", ")
    |> Array.map int64
    |> List.ofArray

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0

    if System.Int32.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

let parseOperation (line: string) =
    let m =
        Regex.Match(line, "Operation: new = (?<value1>\S*) (?<operator>\S*) (?<value2>\S*)")

    let operator =
        match m.Groups["operator"].Value with
        | "+" -> Add
        | "*" -> Multiply
        | _ -> failwith "invalid input for operator"

    let matchValue v =
        match v with
        | "old" -> Old
        | Integer x -> Value x
        | _ -> failwith "invalid input for operation value 1"

    { Operator = operator
      Value1 = m.Groups["value1"].Value |> matchValue
      Value2 = m.Groups["value2"].Value |> matchValue }

let parseTest (lines: string list) =
    { DivisibleBy =
        lines
        |> List.head
        |> removeSubString "Test: divisible by "
        |> int64
      TrueTarget =
        lines[1]
        |> removeSubString "If true: throw to monkey "
        |> int
      FalseTarget =
        lines[2]
        |> removeSubString "If false: throw to monkey "
        |> int }

let parse monkeyInfo =
    { Number = monkeyInfo |> List.head |> parseNumber
      Items = parseItems monkeyInfo[1]
      Operation = parseOperation monkeyInfo[2]
      Test = parseTest monkeyInfo[3..]
      Inspected = 0 }

let getValue old operationValue =
    match operationValue with
    | Old -> old
    | Value v -> v |> int64

let inspect m =
    let v1 =
        m.Operation.Value1
        |> getValue (m.Items |> List.head)

    let v2 =
        m.Operation.Value2
        |> getValue (m.Items |> List.head)

    match m.Operation.Operator with
    | Add -> v1 + v2
    | Multiply -> v1 * v2

let monkeyToThrowAt test (worryLevel: int64) =
    if worryLevel % (test.DivisibleBy |> int64) = 0 then
        test.TrueTarget
    else
        test.FalseTarget

let rec applyTurn reduceWorries monkeys index =
    let monkey = monkeys |> List.item index

    match monkey.Items with
    | _ :: items ->
        let newItem = monkey |> inspect |> reduceWorries
        let targetMonkey = newItem |> monkeyToThrowAt monkey.Test

        let updatedMonkeys =
            monkeys
            |> List.mapi (fun i m ->
                match i with
                | x when x = index ->
                    { m with
                        Inspected = m.Inspected + 1L
                        Items = items }
                | x when x = targetMonkey -> { m with Items = m.Items @ [ newItem ] }
                | _ -> m)

        applyTurn reduceWorries updatedMonkeys index
    | [] -> monkeys

let rec applyRounds reduceWorries x monkeys =
    if x > 0 then
        let monkeysAfterRound =
            (monkeys, [ 0..7 ])
            ||> List.fold (fun acc x -> applyTurn reduceWorries acc x)

        applyRounds reduceWorries (x - 1) monkeysAfterRound
    else
        monkeys

let part1 =
    let reduceWorries x = x / 3L

    inputs
    |> List.map parse
    |> applyRounds reduceWorries 20
    |> List.sortByDescending (fun m -> m.Inspected)
    |> List.take 2
    |> List.map (fun m -> m.Inspected)
    |> List.reduce (*)

let part2 =
    let parsedInputs = inputs |> List.map parse

    let reduceWorries x =
        let gcd =
            parsedInputs
            |> List.map (fun m -> m.Test.DivisibleBy)
            |> List.reduce (*)

        x % gcd

    parsedInputs
    |> applyRounds reduceWorries 10_000
    |> List.sortByDescending (fun m -> m.Inspected)
    |> List.take 2
    |> List.map (fun m -> m.Inspected)
    |> List.reduce (*)
