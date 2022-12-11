module Year2022Day8
    open System.IO
    
    type Tree = {
        Size : int 
        Visible : bool
    }

    let inputs = 
        File.ReadAllLines("inputs/day8.txt")
        |> List.ofSeq
        |> List.map (fun x -> x |> List.ofSeq |> List.map (fun c -> {Size =(c |> int) - 48; Visible=false}))

    let makeFirstAndLastVisible list = list |> List.mapi (fun index line -> if index = 0 || index = 98 then (line |> List.map (fun t -> {Size = t.Size; Visible = true})) else line)
    
    let evaluateLine (trees:Tree list) = 
        let rec loop (acc:Tree list) (tree:Tree) (remainingTrees:Tree list) =
            match tree, remainingTrees with
            | t, [] -> (t::acc) |> List.rev
            | t, t1::ts -> 
                let analysedTree = if (t1.Size > t.Size) && t.Visible then {Size = t1.Size; Visible = true} else t1
                loop (t::acc) analysedTree ts

        loop [] trees.Head trees.Tail

    let part1 = 
        inputs
        |> makeFirstAndLastVisible
        |> List.transpose
        |> makeFirstAndLastVisible
        |> List.transpose
        |> List.map evaluateLine
        |> List.map (List.rev >> evaluateLine)
        // |> List.map evaluateLine
        // |> List.map (List.rev >> evaluateLine)
        |> List.map (fun line -> line |> List.filter(fun t -> t.Visible) |> List.length)


    let part2 = 0