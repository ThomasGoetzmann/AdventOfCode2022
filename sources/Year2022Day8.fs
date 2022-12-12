module Year2022Day8
    open System.IO
    
    type Tree = {
        Size : int 
        Visible : bool
        ScenicScore : int
    }

    let inputs = 
        File.ReadAllLines("inputs/day8.txt")
        |> List.ofSeq
        |> List.map (fun x -> x |> List.ofSeq |> List.map (fun c -> {Size = c |> int |> (+) -48; Visible=false; ScenicScore = 0}))
    
    let isOnEdge index = index = 0 || index = 98

    let isVisibleFromAny treeLists tree =
        treeLists 
        |> List.exists (List.forall (fun t -> t.Size < tree.Size))

    let checkVisibility array =
        array
        |> List.mapi (fun i treeRow -> 
            if i |> isOnEdge
            then treeRow |> List.map (fun tree -> {tree with Visible = true})
            else
                treeRow |> List.mapi (fun j tree ->
                    if j |> isOnEdge 
                    then {tree with Visible = true}
                    else 
                        let left, right = treeRow |> List.splitAt j                        
                        let top, bottom = array |> List.map (fun treeRow -> treeRow[j]) |> List.splitAt i
                        
                        if tree |> isVisibleFromAny [left; right.Tail; top; bottom.Tail]
                        then {tree with Visible = true}
                        else tree
                )
        )

    let scenicScore treeLists tree = 
        let rec score acc trees =
            match trees with
            | t::ts when t.Size < tree.Size -> score (acc + 1) ts
            | [] -> acc
            | _ -> acc + 1
        
        treeLists
        |> List.map (fun trees -> score 0 trees)
        |> List.reduce (*)

    let scenicScores array =
        array 
        |> List.mapi (fun i treeRow ->
            if i |> isOnEdge
            then treeRow |> List.map (fun tree -> {tree with ScenicScore = 0})
            else
                treeRow |> List.mapi (fun j tree -> 
                    if j |> isOnEdge
                    then {tree with ScenicScore = 0 }
                    else
                        let left, right = treeRow |> List.splitAt j
                        let top, bottom = array |> List.map (fun treeRow -> treeRow[j]) |> List.splitAt i

                        let directions = [left |> List.rev; right.Tail; top |> List.rev; bottom.Tail]
                        {tree with ScenicScore = tree |> scenicScore directions}
                )
        )

    let part1 = 
        inputs
        |> checkVisibility
        |> Seq.reduce List.append
        |> List.filter (fun tree -> tree.Visible)
        |> List.length

    let part2 = 
        (inputs
        |> scenicScores
        |> Seq.reduce List.append
        |> List.maxBy (fun tree -> tree.ScenicScore)
        ).ScenicScore