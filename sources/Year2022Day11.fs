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
    { DivisibleBy: int
      TrueTarget: int
      FalseTarget: int }

type Monkey =
    { Number: int
      Items: int List
      Operation: Operation
      Test: Test }

let parseNumber line =
    Regex.Match(line, "Monkey (?<number>\d):")
        .Groups["number"]
        .Value
    |> int

let removeSubString (substring:string) (s:string) =
    let index = s.IndexOf(substring)
    s.Remove(index, substring.Length)

let parseItems (line: string) =
    let items = line |> removeSubString "Starting items: "

    items.Split(", ") |> Array.map int |> List.ofArray

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0

    if System.Int32.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

let parseOperation (line: string) =
    let m = Regex.Match(line, "Operation: new = (?<value1>\S*) (?<operator>\S*) (?<value2>\S*)")

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
    { DivisibleBy = lines |> List.head |> removeSubString "Test: divisible by " |> int; 
      TrueTarget = lines[1] |> removeSubString "If true: throw to monkey " |> int; 
      FalseTarget = lines[2] |> removeSubString "If false: throw to monkey " |> int}

let parse monkeyInfo =
    { Number = monkeyInfo |> List.head |> parseNumber;
      Items = parseItems monkeyInfo[1];
      Operation = parseOperation monkeyInfo[2];
      Test = parseTest monkeyInfo[3..]}

let part1 = inputs |> List.map parse

let part2 = 0
