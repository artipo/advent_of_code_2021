module App.Solutions.Day14

open App
open System

// domain
type Polymer = (string * int64) seq
type Rules = Map<string, char>

let firstChar (pair : string) =
    let cs = pair.ToCharArray()
    cs.[0]

let lastChar (pair : string) =
    let cs = pair.ToCharArray()
    cs.[cs.Length - 1]

let splitPair (pair : string) =
    let cs = pair.ToCharArray()
    cs.[0], cs.[1]

let formPair a b =
    $"{a}{b}"

let applyRules (polymer : Polymer) (rules: Rules) =
    polymer
    |> Seq.collect
        (fun (pair, count) ->
            match rules.TryFind(pair) with
            | Some c ->
                let a, b = splitPair pair
                seq {
                    yield (formPair a c, count)
                    yield (formPair c b, count)
                }
            | None ->
                (pair, count)
                |> Seq.singleton)
    |> Seq.groupBy fst
    |> Seq.map (fun (pair, polys) ->
        pair, polys |> Seq.map snd |> Seq.sum)

let simulate steps (template : string) (rules: Rules) =
    let rec loop steps polymer =
        match steps with
        | 0 -> polymer
        | _ -> loop (steps - 1) (applyRules polymer rules)
    
    template
    |> Seq.windowed 2
    |> Seq.countBy id
    |> Seq.map (fun (pair, count) ->
        pair
        |> Seq.map string
        |> String.concat ""
        , count |> int64)
    |>
    loop steps, template

let toResult (polymer : Polymer) (template : string) =
    polymer
    |> Seq.map (fun (pair, count) ->
        lastChar pair, count)
    |> Seq.append ((firstChar template, 1L) |> Seq.singleton)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, polys) ->
        polys |> Seq.map snd |> Seq.sum)
    |> Seq.sort
    |> (fun s ->
        (s |> Seq.last) - (s |> Seq.head))

// OLD _ TOO SLOW T.T
//let applyRules (polymer : Polymer) (rules: Rules) =
//    polymer
//    |> Seq.windowed 2
//    |> Seq.map
//        (fun w ->
//            match rules.TryFind(w) with
//            | Some c -> w |> Seq.insertAt 1 c
//            | None -> w |> Seq.ofArray)
//    |> Seq.map Seq.tail
//    |> Seq.concat
//    |> Seq.append
//        (polymer
//        |> Seq.take 1)
//
//let simulate steps polymer (rules: Rules) =
//    let rec loop steps polymer =
//        match steps with
//        | 0 -> polymer
//        | _ -> loop (steps - 1) (applyRules polymer rules)
//    
//    loop steps polymer

// utils
let parse (lines : string seq) =
    let polymer = lines |> Seq.head
    
    let rules =
        lines
        |> Seq.tail
        |> Seq.map (fun s -> s.Substring(0, 2), lastChar s)
        |> Map.ofSeq
    polymer, rules

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day14.txt"
    |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace(l)))
    |> parse
    ||> simulate 10
    ||> toResult
    |> printfn "Day14, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day14.txt"
    |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace(l)))
    |> parse
    ||> simulate 40
    ||> toResult
    |> printfn "Day14, puzzle 2 -> result = %A"