module App.Solutions.Day08

open System
open App

// domain
type Entries = string list
type OutputValues = string list

type Segment =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

type Number =
    | N0
    | N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9

type Guess =
    | Sure of Segment
    | Unsure of Set<Segment>
    | NotInit

type Decryption = Map<Segment, Guess>
type Solution = Map<Segment, Segment>

// segments utils
let init () =
    [ A, NotInit
      B, NotInit
      C, NotInit
      D, NotInit
      E, NotInit
      F, NotInit
      G, NotInit ]
    |> Map.ofList

let allSegments = [ A; B; C; D; E; F; G ] |> Set.ofList

let segmentsToNumber segments =
    match segments |> Set.toList with
    | [ A; B; C; E; F; G ] -> N0
    | [ C; F ] -> N1
    | [ A; C; D; E; G ] -> N2
    | [ A; C; D; F; G ] -> N3
    | [ B; C; D; F ] -> N4
    | [ A; B; D; F; G ] -> N5
    | [ A; B; D; E; F; G ] -> N6
    | [ A; C; F ] -> N7
    | [ A; B; C; D; E; F; G ] -> N8
    | [ A; B; C; D; F; G ] -> N9
    | _ -> failwith "Huston we have a problem!"

let solveSegment (solution : Solution) segment' =
    match solution.TryFind(segment') with
    | Some segment -> segment
    | None -> failwith "Huston we have a problem!"

let private charToSegment =
    function
    | 'a' -> A
    | 'b' -> B
    | 'c' -> C
    | 'd' -> D
    | 'e' -> E
    | 'f' -> F
    | 'g' -> G
    | _ -> failwith "Huston we have a problem!"

let private numToSegmentCount =
    function
    | N0 -> 6
    | N1 -> 2
    | N2 -> 5
    | N3 -> 5
    | N4 -> 4
    | N5 -> 5
    | N6 -> 6
    | N7 -> 3
    | N8 -> 7
    | N9 -> 6

let private numberToInt =
    function
    | N0 -> 0
    | N1 -> 1
    | N2 -> 2
    | N3 -> 3
    | N4 -> 4
    | N5 -> 5
    | N6 -> 6
    | N7 -> 7
    | N8 -> 8
    | N9 -> 9

// decryption utils
let getSure segment' (decryption: Decryption) =
    match decryption.TryFind(segment') with
    | Some (Sure segment) -> segment
    | _ -> failwith "Huston we have a problem!"

let getUnsure segment' (decryption: Decryption) =
    match decryption.TryFind(segment') with
    | Some (Unsure segments) -> segments
    | _ -> failwith "Huston we have a problem!"

let saveSure segment value (decryption: Decryption) = decryption.Add(segment, Sure value)

let saveUnsure segment values (decryption: Decryption) = decryption.Add(segment, Unsure values)

let getNumber number (entries: Entries) =
    let length = numToSegmentCount number

    entries
    |> List.filter (fun s -> s.Length = length)
    |> List.exactlyOne
    |> Seq.map charToSegment
    |> Set.ofSeq

let private getOne (entries: Entries) = getNumber N1 entries

let private getFour (entries: Entries) = getNumber N4 entries

let private getSeven (entries: Entries) = getNumber N7 entries

let private solveOne (one: Set<Segment>) (decryption: Decryption) =
    decryption.Add(C, Unsure one).Add(F, Unsure one)

let private solveSeven (seven: Set<Segment>) (decryption: Decryption) =
    let one = getUnsure C decryption

    let a =
        Set.difference seven one
        |> Set.exactlyOne

    saveSure A a decryption

let private solveFour (four: Set<Segment>) (decryption: Decryption) =
    let one = getUnsure C decryption
    let maybeBD = Set.difference four one

    decryption
    |> saveUnsure B maybeBD
    |> saveUnsure D maybeBD

let private solveB (decryption: Decryption) =

    let maybeB = getUnsure B decryption
    let d = getSure D decryption

    let b =
        [ d ]
        |> Set.ofList
        |> Set.difference maybeB
        |> Set.exactlyOne

    saveSure B b decryption

let private solveC (decryption: Decryption) =

    let f = getSure F decryption

    let maybeC = getUnsure C decryption

    let c =
        [ f ]
        |> Set.ofList
        |> Set.difference maybeC
        |> Set.exactlyOne

    saveSure C c decryption

let private solveD (entries: Entries) (decryption: Decryption) =

    let maybeD = getUnsure B decryption

    let d =
        entries
        |> List.filter (fun s -> s.Length = 6)
        |> List.map (Seq.map charToSegment >> Set.ofSeq)
        |> Set.intersectMany
        |> Set.difference maybeD
        |> Set.exactlyOne

    saveSure D d decryption

let private solveE (decryption: Decryption) =

    let one = getUnsure C decryption
    let a = getSure A decryption
    let b = getSure B decryption
    let d = getSure D decryption
    let g = getSure G decryption

    let e =
        let maybeE =
            [ a; b; d; g ]
            |> Set.ofList
            |> Set.difference allSegments

        Set.difference maybeE one |> Set.exactlyOne

    saveSure E e decryption

let private solveF (entries: Entries) (decryption: Decryption) =

    let a = getSure A decryption
    let b = getSure B decryption
    let d = getSure D decryption
    let e = getSure E decryption
    let g = getSure G decryption

    let known = [ a; b; d; e; g ] |> Set.ofList

    let maybeF =
        entries
        |> List.filter (fun s -> s.Length = 6)
        |> List.map (Seq.map charToSegment >> Set.ofSeq)
        |> List.filter (fun s -> Set.difference s known |> Set.toSeq |> Seq.length = 1)
        |> List.exactlyOne

    let f =
        Set.difference maybeF known |> Set.exactlyOne

    saveSure F f decryption

let private solveG (entries: Entries) (decryption: Decryption) =
    let a = getSure A decryption
    
    let maybeG =
        entries
        |> List.map (Seq.map charToSegment >> Set.ofSeq)
        |> List.filter (fun s -> s |> Set.toSeq |> Seq.length > 4)
        |> Set.intersectMany
    
    let g =
        [ a ]
        |> Set.ofList
        |> Set.difference maybeG
        |> Set.exactlyOne

    decryption.Add(G, Sure g)

let private compile (decryption: Decryption) =
    let a = getSure A decryption
    let b = getSure B decryption
    let c = getSure C decryption
    let d = getSure D decryption
    let e = getSure E decryption
    let f = getSure F decryption
    let g = getSure G decryption
    
    [
        a, A
        b, B
        c, C
        d, D
        e, E
        f, F
        g, G
    ] |> Map.ofList

let solve (entries: Entries, outputValues: OutputValues) =

    let decryption = init ()

    let one = getOne entries
    let four = getFour entries
    let seven = getSeven entries

    let solution =
        decryption
        |> solveOne one // solves A
        |> solveSeven seven
        |> solveFour four
        |> solveG entries
        |> solveD entries
        |> solveB
        |> solveE
        |> solveF entries
        |> solveC
        |> compile

    outputValues
    |> List.map (Seq.map charToSegment >> Set.ofSeq)
    |> List.map (Set.map (solveSegment solution))
    |> List.map segmentsToNumber
    |> List.map numberToInt
    |> List.map string
    |> String.concat ""
    |> int

// part 1
let countEasyDigitInOutputValues (outputValues: OutputValues) =
    outputValues
    |> List.choose
        (fun s ->
            match s.Length with
            | 2
            | 3
            | 4
            | 7 -> Some 1
            | _ -> None)
    |> List.sum

// wrappers
let parse (text: string) =
    let sections =
        text.Split(
            "|",
            StringSplitOptions.TrimEntries
            ||| StringSplitOptions.RemoveEmptyEntries
        )

    let entries =
        sections.[0]
            .Split(
                " ",
                StringSplitOptions.TrimEntries
                ||| StringSplitOptions.RemoveEmptyEntries
            )
        |> List.ofArray

    let outputValues =
        sections.[1]
            .Split(
                " ",
                StringSplitOptions.TrimEntries
                ||| StringSplitOptions.RemoveEmptyEntries
            )
        |> List.ofArray

    entries, outputValues

let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day08.txt"
    |> List.ofArray
    |> List.map parse
    |> List.map snd
    |> List.sumBy countEasyDigitInOutputValues
    |> printfn "Day08, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day08.txt"
    |> List.ofArray
    |> List.map parse
    |> List.map solve
    |> List.sum
    |> printfn "Day08, puzzle 2 -> result = %A"