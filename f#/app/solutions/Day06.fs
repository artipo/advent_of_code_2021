module App.Solutions.Day06

open System
open App

// domain
type Fish = int

let resetFish = 6
let newFish = 8

let rec simulateFish remainingDays fish =
    if remainingDays >= fish then
        match fish with
        | 0 ->
            (simulateFish (remainingDays - 1 - resetFish) 0) + (simulateFish (remainingDays - 1 - newFish) 0)
        | _ ->
            (simulateFish (remainingDays - 1 - fish) 0)
    else
        1L

let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day06.txt"
    |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int
    |> Array.Parallel.map (simulateFish 80)
    |> Array.sum
    |> printfn "Day06, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day06.txt"
    |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int
    |> Array.Parallel.map (simulateFish 256)
    |> Array.sum
    |> printfn "Day06, puzzle 2 -> result = %A"