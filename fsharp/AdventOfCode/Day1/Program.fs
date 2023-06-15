open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let splitSeq list = 
    let rec splitSeqRec acc list = 
        match list with
        | x::y::rest when y = "" -> (x::acc), rest
        | x::rest -> splitSeqRec (x::acc) rest
        | [] -> acc, []
    splitSeqRec [] list

let repeatUntilEmpty f list = 
    let rec repeatUntilEmptyRec acc list = 
        match f list with 
        | l, [] -> (l::acc)
        | l, rest -> repeatUntilEmptyRec (l::acc) rest
    repeatUntilEmptyRec [] list

let solvePart1 list =
    list
    |> List.map (List.sumBy int)
    |> List.max

let solvePart2 list =
    list
    |> List.map (List.sumBy int)
    |> List.sortDescending
    |> List.take 3
    |> List.sum

File.ReadLines("ElfCalories.txt") |> Seq.toList |>  repeatUntilEmpty splitSeq |> solvePart1 |> printfn "%A"
//File.ReadLines("ElfCalories.txt") |> Seq.toList |>  repeatUntilEmpty splitSeq |> solvePart1 |> printfn "%A"