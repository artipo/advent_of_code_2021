module App.Program

open System
open App.Solutions

type Day =
    | Day_1
    | Day_2
    | Day_3
    | Day_4
    | Day_5
    | Day_6
    | Day_7
    | Day_8
    | Day_9
    | Day_10
    | Day_11
    | Day_12
    | Day_13
    | Day_14
    | Day_15
    | Day_16
    | Day_17
    | Day_18
    | Day_19
    | Day_20
    | Day_21
    | Day_22
    | Day_23
    | Day_24
    | Day_25

let parseDay day =
    match day with
    | "1" -> Some Day_1
    | "2" -> Some Day_2
    | "3" -> Some Day_3
    | "4" -> Some Day_4
    | "5" -> Some Day_5
    | "6" -> Some Day_6
    | "7" -> Some Day_7
    | "8" -> Some Day_8
    | "9" -> Some Day_9
    | "10" -> Some Day_10
    | "11" -> Some Day_11
    | "12" -> Some Day_12
    | "13" -> Some Day_13
    | "14" -> Some Day_14
    | "15" -> Some Day_15
    | "16" -> Some Day_16
    | "17" -> Some Day_17
    | "18" -> Some Day_18
    | "19" -> Some Day_19
    | "20" -> Some Day_20
    | "21" -> Some Day_21
    | "22" -> Some Day_22
    | "23" -> Some Day_23
    | "24" -> Some Day_24
    | "25" -> Some Day_25
    | _ -> None

let solve day =
    match day with
    | None ->
        Day01.solve_puzzle_1 ()
        Day01.solve_puzzle_2 ()
        
        Day02.solve_puzzle_1 ()
        Day02.solve_puzzle_2 ()
    
    | Some Day_1 ->
        Day01.solve_puzzle_1 ()
        Day01.solve_puzzle_2 ()
    | Some Day_2 ->
        Day02.solve_puzzle_1 ()
        Day02.solve_puzzle_2 ()
    | Some Day_3 ->
        Day03.solve_puzzle_1 ()
        Day03.solve_puzzle_2 ()
    | Some Day_4 ->
        Day04.solve_puzzle_1 ()
        Day04.solve_puzzle_2 ()
    | Some Day_5 ->
        Day05.solve_puzzle_1 ()
        Day05.solve_puzzle_2 ()
    | Some Day_6 ->
        Day06.solve_puzzle_1 ()
        Day06.solve_puzzle_2 ()
    | Some Day_7 ->
        Day07.solve_puzzle_1 ()
        Day07.solve_puzzle_2 ()
    | Some Day_8 ->
        Day08.solve_puzzle_1 ()
        Day08.solve_puzzle_2 ()
        
    | Some _ -> raise (NotImplementedException("NOT IMPLEMENTED -> doing day by day!"))

[<EntryPoint>]
let main argv =
    argv
    |> Array.tryHead
    |> Option.bind parseDay
    |> solve
    
    0
