module App.Solutions.Day09

open App

// domain
type Height = int

let getLowPoints (map: Map<int * int, Height>) =
    map
    |> Map.toSeq
    |> Seq.filter
        (fun ((x, y), h) ->
            seq {
                for dx, dy in [ (-1, 0); (0, -1); (1, 0); (0, 1) ] do
                    if map.ContainsKey((x + dx, y + dy)) then
                        yield map.[(x + dx, y + dy)]
            }
            |> Seq.forall ((<) h))

let getBasin (map: Map<int * int, Height>) lowPoint =
    let rec loop pointsToExplore pointsVisited acc =
        match pointsToExplore with
        | [] -> acc
        | x::xs ->
            match x with
            | _, 9 -> loop xs (x::pointsVisited) acc
            | (px, py), _ ->
                let newPoints =
                    [
                        for dx, dy in [ (-1, 0); (0, -1); (1, 0); (0, 1) ] do
                            if map.ContainsKey((px + dx, py + dy)) then
                                yield (px + dx, py + dy), map.[(px + dx, py + dy)]
                    ]
                    |> List.except pointsVisited
                loop (xs@newPoints) (pointsVisited@newPoints) (x::acc)
    
    loop [ lowPoint ] [ lowPoint ] []

let toRiskLevel point =
    let _, h = point
    h + 1

// utils
let parse (rows: string seq) =
    rows
    |> Seq.indexed
    |> Seq.collect
        (fun (i, r) ->
            r
            |> Seq.indexed
            |> Seq.map (fun (j, h) -> (i, j), h |> string |> int))
    |> Map.ofSeq

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day09.txt"
    |> parse
    |> getLowPoints
    |> Seq.map toRiskLevel
    |> Seq.sum
    |> printfn "Day09, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    let map =
        File.readAllLines @"./inputs/day09.txt"
        |> parse
        
    getLowPoints map
    |> Seq.map (getBasin map)
    |> Seq.map Seq.length
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (fun s1 s2 -> s1 * s2)
    |> printfn "Day09, puzzle 2 -> result = %A"
