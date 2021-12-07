module App.Solutions.Day06

open App
open System
open System.Collections.Generic

// domain
type Fish = int

let resetFish = 6
let newFish = 8

let cache = Dictionary<int, int64>()

let rec getValue key =
    match sign key with
    | -1 -> 1L
    | _ ->
        match cache |> Dictionary.tryFind key with
        | None ->
            let res = simulateFish key 0
            cache.Add(key, res)
            res
        | Some v ->
            v

and simulateFish remainingDays fish =
    if remainingDays >= fish then
        match fish with
        | 0 ->
            let parent = getValue (remainingDays - 1 - resetFish)
            let child = getValue (remainingDays - 1 - newFish)
            parent + child
        | _ ->
            getValue (remainingDays - 1 - fish)
    else
        1L

let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day06.txt"
    |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int
    |> Array.map (simulateFish 80)
    |> Array.sum
    |> printfn "Day06, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day06.txt"
    |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int
    |> Array.map (simulateFish 256)
    |> Array.sum
    |> printfn "Day06, puzzle 2 -> result = %A"