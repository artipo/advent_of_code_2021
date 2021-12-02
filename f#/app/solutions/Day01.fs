module App.Solutions.Day01

open App.Helpers

// domain
type Measurement = int

type MeasurementChange =
    | Unknown
    | Smaller
    | Equal
    | Larger

let changeInMeasurements m_prev m_next =
    match m_prev, m_next with
    | p, n when p > n -> Smaller
    | p, n when p = n -> Equal
    | p, n when p < n -> Larger
    | _, _ -> Unknown

let mapMeasurementsToChanges measurements =
    measurements
    |> Seq.windowed 2
    |> Seq.mapAsCouples changeInMeasurements

let countChanges change changes =
    changes
    |> Seq.filter (fun c -> c = change)
    |> Seq.length

let removeNoiseFromMeasurements (measurements : Measurement list) =
    measurements
    |> Seq.windowed 3
    |> Seq.map Seq.sum

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day01.txt"
    |> List.ofArray
    |> List.map int
    |> mapMeasurementsToChanges
    |> countChanges MeasurementChange.Larger
    |> printfn "Day01, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day01.txt"
    |> List.ofArray
    |> List.map int
    |> removeNoiseFromMeasurements
    |> mapMeasurementsToChanges
    |> countChanges MeasurementChange.Larger
    |> printfn "Day01, puzzle 2 -> result = %A"