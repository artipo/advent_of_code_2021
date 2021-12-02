module App.Solutions.Day02

open App.Helpers

// domain
type Distance = int

type Direction =
    | Forward
    | Down
    | Up

type Command = Direction * Distance

module Puzzle_1 =
    let move pos cmd =
        let hor, depth = pos

        match cmd with
        | Forward, d -> hor + d, depth
        | Down, d -> hor, depth + d
        | Up, d -> hor, depth - d
    
    let overallDistance pos =
        let h, d = pos
        h * d

module Puzzle_2 =
    let move pos cmd =
        let hor, depth, aim = pos

        match cmd with
        | Forward, d -> hor + d, depth + aim * d, aim
        | Down, d -> hor, depth, aim + d
        | Up, d -> hor, depth, aim - d

    let overallDistance pos =
        let h, d, _ = pos
        h * d

// helpers
let parseDirection dir =
    match dir with
    | "forward" -> Forward
    | "down" -> Down
    | "up" -> Up
    | _ -> failwith "invalid command"

let parseCommands (commands : string seq) =
    commands
    |> Seq.map
        (fun c ->
            let values = c.Split " "
            let dir = values.[0] |> parseDirection
            let dist = values.[1] |> int
            dir, dist)

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day02.txt"
    |> parseCommands
    |> Seq.fold Puzzle_1.move (0, 0)
    |> Puzzle_1.overallDistance
    |> printfn "Day02, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day02.txt"
    |> parseCommands
    |> Seq.fold Puzzle_2.move (0, 0, 0)
    |> Puzzle_2.overallDistance
    |> printfn "Day02, puzzle 2 -> result = %A"