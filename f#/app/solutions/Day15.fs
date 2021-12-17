module App.Solutions.Day15

open System.Collections.Generic
open App
open System

// domain
type Risk = int
type Pos = int * int
type Cave = Map<Pos, Risk>

type Parent =
    | Undefined
    | Defined of Pos

type Distance = int
type Solution = IDictionary<Pos, Distance * Parent>
type Visited = IDictionary<Pos, bool>

let searchShortestPath (cave: Cave) =

    // setup
    let startPos =
        ((cave |> Map.keys |> Seq.map fst |> Seq.min), (cave |> Map.keys |> Seq.map snd |> Seq.min))

    let width =
        (cave |> Map.keys |> Seq.map fst |> Seq.max)

    let height =
        (cave |> Map.keys |> Seq.map snd |> Seq.max)

    let endPos = (width, height)

    let sln = Dictionary<Pos, Distance * Parent>()
    cave.Keys
    |> Seq.iter (fun p -> sln.Add(p, (Int32.MaxValue, Undefined)))
    sln.[startPos] <- (0, Undefined)

    let visited = Dictionary<Pos, bool>()
    cave.Keys
    |> Seq.iter (fun p -> visited.Add(p, false))
    visited.[startPos] <- true

    // result
    let rec unfoldLoop pos (sln: Solution) acc =
        match sln.[pos] with
        | _, Undefined -> acc
        | _, Defined parentPos -> unfoldLoop parentPos sln (parentPos :: acc)

    // Dijkstra
    let rec loop toVisit =
        match toVisit with
        | [] -> failwith "Huston we have a problem!"
        | p :: _ when p = endPos -> (unfoldLoop p sln [ p ]), (sln.[p] |> fst)
        | p :: ps ->
            let x, y = p
            let baseDst, _ = sln.[p]

            // find neighbors
            let neighbors =
                [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
                |> List.map (fun (dx, dy) -> x + dx, y + dy)
                |> List.filter
                    (fun pos ->
                        let posX, posY = pos

                        posX >= 0
                        && posX <= width
                        && posY >= 0
                        && posY <= height)
                |> List.filter (fun pos -> not visited.[pos])

            // edit sln
            neighbors
            |> Seq.iter
                (fun pos ->
                    let newDst = baseDst + cave.[pos]

                    sln.[pos] <-
                        match sln.[pos] with
                        | Int32.MaxValue, Undefined -> newDst, Defined p
                        | prevDst, _ when newDst < prevDst -> newDst, Defined p
                        | v -> v)

            // edit toVisit
            let newToVisit =
                ps
                |> List.append neighbors
                |> List.distinct
                |> List.sortBy (fun pos -> sln.[pos] |> fst)

            visited.[p] <- true
            loop newToVisit
    
    loop [ startPos ]

// utils
let parse (rows: string seq) =
    rows
    |> Seq.indexed
    |> Seq.collect
        (fun (i, r) ->
            r
            |> Seq.indexed
            |> Seq.map (fun (j, r) -> (i, j), r |> string |> int))
    |> Map.ofSeq

let expand n (cave : Cave) =
    let width =
        (cave |> Map.keys |> Seq.map fst |> Seq.max) + 1

    let height =
        (cave |> Map.keys |> Seq.map snd |> Seq.max) + 1
    
    let rec mod9 n = if n <= 9 then n else mod9 (n - 9)
        
    [
        for i in 0..(n-1) do
            for j in 0..(n-1) do
                yield!
                    cave
                    |> Map.toSeq
                    |> Seq.map (fun ((x, y), r) ->
                        ((width * i + x),(height * j + y)), (r + i + j) |> mod9)
    ]
    |> Map.ofList

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day15.txt"
    |> parse
    |> searchShortestPath
    |> snd
    |> printfn "Day15, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day15.txt"
    |> parse
    |> expand 5
    |> searchShortestPath
    |> snd
    |> printfn "Day15, puzzle 2 -> result = %A"
