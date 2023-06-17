module Day2
open System.IO

module Part1 = 
    type Shape = 
        | Rock
        | Paper
        | Scissors

        member this.Score = 
            match this with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        static member MapCode = function
            | 'A' 
            | 'X' -> Rock
            | 'B' 
            | 'Y' -> Paper
            | 'C' 
            | 'Z' -> Scissors
            | _ -> Rock

    type Round = 
        { OpponentHand : Shape
          UserHand     : Shape }

        member this.Result = 
            match this.OpponentHand, this.UserHand with 
            | x, y when x = y -> 3
            | Rock, Paper
            | Paper, Scissors
            | Scissors, Rock -> 6
            | _ -> 0

        member this.Score = this.UserHand.Score + this.Result

        static member Parse (line:string) = { OpponentHand = Shape.MapCode line[0]; UserHand = Shape.MapCode line[2] }

    let solve filename = File.ReadLines(filename) |> Seq.map (Round.Parse >> fun x -> x.Score) |> Seq.sum |> printfn "%A"

module Part2 = 
    type Shape = 
        | Rock
        | Paper
        | Scissors

        member this.Score = 
            match this with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        static member MapCode = function
            | 'A' -> Rock
            | 'B' -> Paper
            | 'C' -> Scissors
            | _ -> Rock

    type Result = 
        | Lose
        | Draw
        | Win
        
        static member MapCode = function
            | 'X' -> Lose
            | 'Y' -> Draw
            | 'Z' -> Win
            | _ -> Lose

        member this.Score = 
            match this with 
            | Lose -> 0
            | Draw -> 3
            | Win -> 6

        member this.UserHand = fun (oppHand:Shape) ->
            match this, oppHand with
            | Lose, Rock 
            | Draw, Scissors 
            | Win, Paper -> Scissors.Score
            | Lose, Scissors
            | Draw, Paper
            | Win, Rock -> Paper.Score
            | Lose, Paper
            | Draw, Rock
            | Win, Scissors -> Rock.Score

    type Round = 
        { OpponentHand : Shape
          UserResult   : Result }

        member this.Score = (this.UserResult.UserHand this.OpponentHand) + this.UserResult.Score

        static member Parse (line:string) = { OpponentHand = Shape.MapCode line[0]; UserResult = Result.MapCode line[2] }

    let solve filename = File.ReadLines(filename) |> Seq.map (Round.Parse >> fun x -> x.Score) |> Seq.sum |> printfn "%A"
