module App.Solutions.Day10

open App

// domain
type Validation =
    | Valid
    | Invalid of char
    | Incomplete of char list

let (|Opens|Closes|) (c : char) =
    match c with
    | '(' | '[' | '{' | '<' -> Opens
    | ')' | ']' | '}' | '>' -> Closes
    | _ -> failwith "Huston we have a problem!"

let isOpens c =
    match c with
    | Opens -> true
    | Closes -> false

let getCloses o =
    match o with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "Huston we have a problem!"

let validate (row : string) =
    let rec loop chunks opened =
        match chunks with
        | [] ->
            match opened with
            | [] -> Valid
            | _ ->
                opened
                |> List.map getCloses
                |> Incomplete
        | x::xs ->
            match x with
            | Opens ->
                loop xs (x::opened)
            | Closes ->
                match opened with
                | [] -> Invalid x
                | y::ys ->
                    if getCloses y = x then
                        loop xs ys
                    else
                        Invalid x
    
    loop (row |> Seq.toList) []

let getInvalidPoints c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "Huston we have a problem!"

let getIncompletePoints c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> failwith "Huston we have a problem!"

let calculateIncompletePoints cs =
    (List.fold (fun s c ->
        s
        |> (*) 5L
        |> (+) (getIncompletePoints c)) 0L) cs

let selectIncompleteWinner cs =
    let winner = (List.length cs) / 2
    
    cs
    |> List.sort
    |> List.item winner

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day10.txt"
    |> List.ofArray
    |> List.map validate
    |> List.choose (fun r ->
        match r with
        | Incomplete _ | Valid -> None
        | Invalid c -> Some c)
    |> List.countBy id
    |> List.map (fun (c, count) ->
        getInvalidPoints c
        |> (*) count)
    |> List.sum
    |> printfn "Day10, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day10.txt"
    |> List.ofArray
    |> List.map validate
    |> List.choose (fun r ->
        match r with
        | Invalid _ | Valid -> None
        | Incomplete cs -> Some cs)
    |> List.map calculateIncompletePoints
    |> selectIncompleteWinner
    |> printfn "Day10, puzzle 2 -> result = %A"