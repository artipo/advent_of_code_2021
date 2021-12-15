module App.Solutions.Day12

open App
open System

// domain
type Node =
    | Small of string
    | Big of string

    member this.Name =
        match this with
        | Small n -> n
        | Big n -> n

type Navigation = Map<Node, Node list>

let getNode (nav : Navigation) name =
    nav
    |> Map.keys
    |> Seq.filter (fun n -> n.Name = name)
    |> Seq.head

let listPaths withDetour (nav : Navigation) =
    let startNode = getNode nav "start"
    let endNode = getNode nav "end"

    let rec loop path smallVisited acc withDetour =
        match path with
        | [] -> failwith "Huston we have a problem!"
        | node::_ when node = endNode -> path::acc
        | node::_ ->
            nav.[node]
            |> List.choose (fun next ->
                match next with
                | Small _ ->
                    if next = startNode then
                        None
                    else
                        match withDetour, List.contains next smallVisited with
                        | true, true -> Some <| loop (next::path) smallVisited acc false
                        | false, true -> None
                        | _, _ -> Some <| loop (next::path) (next::smallVisited) acc withDetour
                | Big _ ->
                    Some <| loop (next::path) smallVisited acc withDetour)
            |> List.concat
            |> List.distinct

    loop [ startNode ] [ startNode ] [] withDetour

// utils
let parseNode n =
    if n |> Seq.forall Char.IsUpper then
        Big n
    else
        Small n

let parse (rows : string list) =
    rows
    |> List.collect (fun l ->
        let nodes = l.Split("-", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        let a = nodes.[0] |> parseNode
        let b = nodes.[1] |> parseNode
        [ a, b
          b, a ])
    |> List.groupBy fst
    |> Map.ofList
    |> Map.map (fun _ value -> value |> List.map snd)

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day12.txt"
    |> List.ofArray
    |> parse
    |> listPaths false
    |> List.length
    |> printfn "Day12, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day12.txt"
    |> List.ofArray
    |> parse
    |> listPaths true
    |> List.length
    |> printfn "Day12, puzzle 2 -> result = %A"
