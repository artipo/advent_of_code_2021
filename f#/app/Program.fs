module App.Program

module Day01 =
    open App.Solutions.Day01

    let solve_day01_1 () =
        File.readAllLines @"C:/Users/Fedo/Documents/Codes/repos/advent_of_code_2021/f#/inputs/day01.txt"
        |> List.ofArray
        |> List.map rowToMeasurement
        |> mapMeasurementsToChanges
        |> countChanges MeasurementChange.Larger
        |> printfn "Day01, puzzle 1 -> result = %A"
    
    let solve_day01_2 () =
        File.readAllLines @"C:/Users/Fedo/Documents/Codes/repos/advent_of_code_2021/f#/inputs/day01.txt"
        |> List.ofArray
        |> List.map rowToMeasurement
        |> removeNoiseFromMeasurements
        |> mapMeasurementsToChanges
        |> countChanges MeasurementChange.Larger
        |> printfn "Day01, puzzle 2 -> result = %A"

[<EntryPoint>]
let main _ =
    Day01.solve_day01_1 ()
    Day01.solve_day01_2 ()
    0
