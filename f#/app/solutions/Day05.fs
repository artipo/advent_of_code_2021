module App.Solutions.Day05

open App
open System

// domain
type Pos = int * int
type VentLine = Pos * Pos

let unfoldVentLine ventLine =
    let (sX, sY), (eX, eY) = ventLine
    let length = max (abs (eX - sX)) (abs (eY - sY))
    let signX, signY = (sign (eX - sX)), (sign (eY - sY))

    [ for i in 0 .. length do
          sX + i * signX, sY + i * signY ]

let parseVentLine (str: string) =
    let pos =
        str
            .Replace(" -> ", ",")
            .Split(
                ",",
                StringSplitOptions.TrimEntries
                ||| StringSplitOptions.RemoveEmptyEntries
            )
        |> Array.map int

    (pos.[0], pos.[1]), (pos.[2], pos.[3])

let isDiagonalVentLine ventLine =
    let (sX, sY), (eX, eY) = ventLine
    let signX, signY = (sign (eX - sX)), (sign (eY - sY))
    signX <> 0 && signY <> 0

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day05.txt"
    |> List.ofArray
    |> List.map parseVentLine
    |> List.filter (not << isDiagonalVentLine)
    |> List.collect unfoldVentLine
    |> List.countBy id
    |> List.map snd
    |> List.filter (fun n -> n >= 2)
    |> List.length
    |> printfn "Day05, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day05.txt"
    |> List.ofArray
    |> List.map parseVentLine
    |> List.collect unfoldVentLine
    |> List.countBy id
    |> List.map snd
    |> List.filter (fun n -> n >= 2)
    |> List.length
    |> printfn "Day05, puzzle 2 -> result = %A"
