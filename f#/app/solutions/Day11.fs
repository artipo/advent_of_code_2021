module App.Solutions.Day11

open App

// domain
type Energy = int
type Pos = int * int
type Opctos = Map<Pos, Energy>

let increaseByOne (octos: Opctos) =
    octos |> Map.map (fun _ energy -> energy + 1)

let resetFlashed (octos: Opctos) =
    octos
    |> Map.map (fun _ energy -> if energy > 9 then 0 else energy)

let getFlashing (octos: Opctos) =
    octos
    |> Map.toList
    |> List.filter (fun (_, energy) -> energy > 9)
    |> List.map fst

let performFlash (pos: Pos) (octos: Opctos) =
    let x, y = pos

    [ -1; 0; 1 ]
    |> List.allPairs [ -1; 0; 1 ]
    |> List.map (fun (dx, dy) -> x + dx, y + dy)
    |> List.filter octos.ContainsKey
    |> List.fold (fun octos p -> octos |> Map.change p (Option.map ((+) 1))) octos

let handleFlashing (octos: Opctos) =
    let rec loop (octos: Opctos) flashing flashed =
        match flashing with
        | [] ->
            octos, flashed |> List.length
        | x :: _ ->
            let newOctos = octos |> performFlash x

            let newFlashed = (x :: flashed)

            let newFlashing =
                newOctos |> getFlashing |> List.except newFlashed

            loop newOctos newFlashing newFlashed
    
    loop octos (octos |> getFlashing) []

let step (octos: Opctos, flashes) =
    octos
    |> increaseByOne
    |> handleFlashing
    |> (fun (octos, newFlahses) ->
        resetFlashed octos,
        flashes + newFlahses,
        octos.Count = newFlahses)

let simulate steps (octos: Opctos) =
    let rec loop n (octos: Opctos, flashes : int, synchFlashes : int list)  =
        match n with
        | 0 -> flashes, synchFlashes
        | _ ->
            (octos, flashes)
            |> step
            |> (fun (octos, flashes, hadSynchFlash) ->
                
                if hadSynchFlash then
                    loop (n - 1) (octos, flashes, (n - 1)::synchFlashes)
                else
                    loop (n - 1) (octos, flashes, synchFlashes))
        
    loop steps (octos, 0, [])

// utils
let parse (rows: string seq) =
    rows
    |> Seq.indexed
    |> Seq.collect
        (fun (i, r) ->
            r
            |> Seq.indexed
            |> Seq.map (fun (j, e) -> (i, j), e |> string |> int))
    |> Map.ofSeq

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day11.txt"
    |> parse
    |> simulate 100
    |> fst
    |> printfn "Day11, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    let steps = 400
    
    File.readAllLines @"./inputs/day11.txt"
    |> parse
    |> simulate steps
    |> snd
    |> List.map (fun n -> steps - n)
    |> List.sort
    |> List.head
    |> printfn "Day11, puzzle 2 -> result = %A"