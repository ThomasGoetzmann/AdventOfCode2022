module Year2022Day7
    open System.IO
    open System.Text.RegularExpressions

    let inputs = File.ReadAllLines("inputs/day7.txt")

    type Command =
    | CD_Root
    | CD_Up
    | CD_Dir of dir: string
    | LS

    type Output =
    | Directory of name:string
    | File of name: string * size:int64

    type Line = 
    | Command of Command
    | Output of Output
    
    let CdDirRegex = "\$ cd (?<name>.+)"
    let DirRegex = "dir (?<name>.*)"
    let FileRegex = "(?<size>\d+) (?<name>.*)"
    
    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    let (|CommandLine|_|) line =
        match line with
        | "$ cd /" -> Some CD_Root
        | "$ cd .." -> Some CD_Up
        | ParseRegex CdDirRegex [dir]-> Some (CD_Dir dir)
        | "$ ls" -> Some LS
        | _ -> None

    let (|OutputLine|_|) line = 
        match line with 
        | ParseRegex FileRegex [size; fileName] -> Some (File (fileName, (int64)size))
        | ParseRegex DirRegex [name] -> Some (Directory name)
        | _ -> None

    let parse inputs =
        inputs 
        |> Seq.map (function
            | CommandLine c -> Command c
            | OutputLine o -> Output o
            | _ -> failwith "unrecognised input")
        |> List.ofSeq

    let run lines =
        let applyCommand command path =
            match command with
            | CD_Root -> []
            | CD_Dir dirName -> dirName :: path
            | CD_Up ->
                match path with
                | _ :: parentPath -> parentPath
                | [] -> failwith "Already at root"
            | LS -> path

        let applyOutput output path contents =
            let existingDir, existingSize = 
                contents 
                |> Map.tryFind path
                |> Option.defaultValue([], 0L)

            let newContents = 
                match output with
                | File (_, size) -> existingDir, size + existingSize
                | Directory dirName -> (dirName :: path) :: existingDir, existingSize
            
            contents |> Map.add path newContents

        (([], Map.empty), lines)
        ||> Seq.fold (fun (path,contents) commands ->
            match commands with
            | Command c -> 
                path |> applyCommand c,
                contents
            | Output o -> 
                path, 
                contents |> applyOutput o path
        )
        |> snd

    let directorySizes =
        let directoryContents = inputs |> parse |> run
        
        let rec getDirectoryTotalSize paths size  =
            match paths with
            | [] -> size
            | dirName :: parentDirs ->
                let directoriesHere, sizeHere = directoryContents |> Map.find dirName
                getDirectoryTotalSize (directoriesHere @ parentDirs) (sizeHere + size)
        
        directoryContents |> Map.map (fun paths _ -> getDirectoryTotalSize [paths] 0L)

    let part1 = 
        directorySizes
        |> Map.values
        |> Seq.filter (fun v -> v <= 100_000L)
        |> Seq.sum

    let part2 = 
        let availableSpace = 70_000_000L
        let requiredSpace  = 30_000_000L
        let usedSpace = directorySizes |> Map.find []
        let freeSpace = availableSpace - usedSpace
        let minRequiredSpace = requiredSpace - freeSpace
        let toDelete = 
            directorySizes
            |> Map.filter (fun _ size -> size >= minRequiredSpace)
            |> Map.toSeq
            |> Seq.minBy snd
        let deletePath = toDelete |> fst |> List.rev |> String.concat "/"
        let deleteSize = toDelete |> snd

        sprintf "Required %i ; FreeSpace %i ; MinRequiredSpace %i, can be done if you delete folder %A of size %i" requiredSpace freeSpace minRequiredSpace deletePath deleteSize