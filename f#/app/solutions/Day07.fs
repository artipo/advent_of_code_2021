module App.Solutions.Day07

open App
open System

// domain
type Fuel = int
type Crab = int

let searchBestPos posToFuel crabs =
    let min = crabs |> List.min
    let max = crabs |> List.max
    
    [ min .. max ]
    |> List.map (fun p -> p, posToFuel p)
    |> List.minBy snd

module Puzzle_1 =
    let moveCrabsToPos crabs pos =
        crabs
        |> List.sumBy (fun c -> abs (pos - c))
        
module Puzzle_2 =
    let moveCrabsToPos crabs pos =
        
        let sumFactorial n =
            [ 1 .. n ]
            |> List.sum
        
        crabs
        |> List.sumBy (fun c ->
            (pos - c)
            |> abs
            |> sumFactorial)

let solve_puzzle_1 () =
    let crabs =
        File.readAllLines @"./inputs/day07.txt"
        |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
        |> Array.map int
        |> List.ofArray
    
    crabs
    |> searchBestPos (Puzzle_1.moveCrabsToPos crabs)
    |> snd
    |> printfn "Day07, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    let crabs =
        File.readAllLines @"./inputs/day07.txt"
        |> Array.collect (fun l -> l.Split(",", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
        |> Array.map int
        |> List.ofArray
    
    crabs
    |> searchBestPos (Puzzle_2.moveCrabsToPos crabs)
    |> snd
    |> printfn "Day07, puzzle 2 -> result = %A"