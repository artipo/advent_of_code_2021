module App.Solutions.Day04

open App
open System

// domain
type Cell =
    | Marked of int
    | Unmarked of int

type Board = Cell list list

let markBoard number board =
    board
    |> List.map
        (fun l ->
            l
            |> List.map
                (fun c' ->
                    match c' with
                    | Unmarked c when c = number -> Marked c
                    | _ -> c'))

let initBoard boardNumbers =
    boardNumbers
    |> List.map (fun l -> l |> List.map Unmarked)

let private containWinningLine length board =
    board
    |> List.map
        (fun l ->
            l
            |> List.filter
                (fun c' ->
                    match c' with
                    | Marked _ -> true
                    | Unmarked _ -> false)
            |> List.length)
    |> List.exists (fun n -> n = length)

let isWinnerBoard board =
    let length = List.length board

    let hasWinningRow = board |> containWinningLine length

    match hasWinningRow with
    | true -> true
    | false ->
        board
        |> List.transpose
        |> containWinningLine length

let simulateBingo numbers boards =
    let rec loop numbers boards res =
        match numbers with
        | [] -> res
        | x :: xs ->
            let winners, notWinners' =
                boards
                |> List.map (markBoard x)
                |> List.groupBy isWinnerBoard
                |> Map.ofList
                |> (fun bs ->
                    let winners =
                        bs
                        |> Map.tryFind true
                        |> Option.defaultValue []
                    
                    let notWinners =
                        bs
                        |> Map.tryFind false
                        |> Option.defaultValue []
                    
                    winners, notWinners)
            
            let newRes =
                winners
                |> List.map (fun b -> b, x)
                |> List.append res
            
            
            match notWinners' with
            | [] ->
                newRes
            | notWinners -> 
                loop xs notWinners newRes
    
    loop numbers boards []

let calculateScore board lastNumber =
    board
    |> List.collect id
    |> List.choose (fun c' ->
        match c' with
        | Marked _ -> None
        | Unmarked c -> Some c)
    |> List.sum
    |> (*) lastNumber

// wrappers
let parseInput fileName =
    let lines =
        File.readAllLines fileName
        |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace(l)))
    
    let numbers =
        lines.[0].Split(",")
        |> Array.map int
        |> List.ofArray
    
    let boards =
        lines
        |> Array.skip 1
        |> Array.chunkBySize 5
        |> Array.map (fun b ->
            b
            |> Array.map (fun l ->
                l.Split(" ", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                |> Array.map int
                |> List.ofArray)
            |> List.ofArray)
        |> List.ofArray
        |> List.map initBoard
    
    numbers, boards

let solve_puzzle_1 () =
    let numbers, boards = parseInput @"./inputs/day04.txt"
    simulateBingo numbers boards
    |> List.head
    ||> calculateScore
    |> printfn "Day04, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    let numbers, boards = parseInput @"./inputs/day04.txt"
    simulateBingo numbers boards
    |> List.last
    ||> calculateScore
    |> printfn "Day04, puzzle 2 -> result = %A"