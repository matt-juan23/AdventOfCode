module Day1
open System.IO

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

//File.ReadLines("ElfCalories.txt") |> Seq.toList |>  repeatUntilEmpty splitSeq |> solvePart1 |> printfn "%A"

let calorieAcc state input = 
    match input, state with
    | "", s -> 0 :: s
    | x, [] -> [ int x ]
    | x, y::ys -> (int x+y) :: ys

let solvePart1Better = List.max

let (solvePart2Better:int list -> int) = List.sortDescending >> List.take 3 >> List.sum

//File.ReadLines("ElfCalories.txt") |> Seq.toList |> List.fold calorieAcc [] |> solvePart1Better |> printfn "%A"

let solve filename = File.ReadLines(filename) |> Seq.toList |> List.fold calorieAcc [] |> solvePart1Better |> printfn "%A"