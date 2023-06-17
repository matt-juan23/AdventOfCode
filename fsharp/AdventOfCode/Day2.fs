module Day2
open System.IO

type ShapeScore = 
    | Rock = 1
    | Paper = 2
    | Scissors = 3

type ResultScore = 
    | Lose = 0 
    | Draw = 3
    | Win = 6

let getScore (line:string) = 
    match Seq.toList line with
    | 'A'::' '::'X'::_ -> int ShapeScore.Rock + int ResultScore.Draw
    | 'B'::' '::'X'::_ -> int ShapeScore.Rock + int ResultScore.Lose
    | 'C'::' '::'X'::_ -> int ShapeScore.Rock + int ResultScore.Win
    | 'A'::' '::'Y'::_ -> int ShapeScore.Paper + int ResultScore.Win
    | 'B'::' '::'Y'::_ -> int ShapeScore.Paper + int ResultScore.Draw
    | 'C'::' '::'Y'::_ -> int ShapeScore.Paper + int ResultScore.Lose
    | 'A'::' '::'Z'::_ -> int ShapeScore.Scissors + int ResultScore.Lose
    | 'B'::' '::'Z'::_ -> int ShapeScore.Scissors + int ResultScore.Win
    | 'C'::' '::'Z'::_ -> int ShapeScore.Scissors + int ResultScore.Draw
    | _ -> 0

let getScore2 (line:string) = 
    match Seq.toList line with
    | 'A'::' '::'X'::_ -> int ShapeScore.Scissors + int ResultScore.Lose
    | 'B'::' '::'X'::_ -> int ShapeScore.Rock + int ResultScore.Lose
    | 'C'::' '::'X'::_ -> int ShapeScore.Paper + int ResultScore.Lose
    | 'A'::' '::'Y'::_ -> int ShapeScore.Rock + int ResultScore.Draw
    | 'B'::' '::'Y'::_ -> int ShapeScore.Paper + int ResultScore.Draw
    | 'C'::' '::'Y'::_ -> int ShapeScore.Scissors + int ResultScore.Draw
    | 'A'::' '::'Z'::_ -> int ShapeScore.Paper + int ResultScore.Win
    | 'B'::' '::'Z'::_ -> int ShapeScore.Scissors + int ResultScore.Win
    | 'C'::' '::'Z'::_ -> int ShapeScore.Rock + int ResultScore.Win
    | _ -> 0

let solve filename = File.ReadLines(filename) |> Seq.toList |> List.map getScore2 |> List.sum |> printfn "%A"
