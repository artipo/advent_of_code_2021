module App.Solutions.Day01

open App.Helpers

// utils
let countChanges change changes =
    changes
    |> Seq.filter (fun c -> c = change)
    |> Seq.length

let rowToMeasurement (r: string) = r |> int

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
    | _ -> Unknown

let mapMeasurementsToChanges measurements =
    measurements
    |> Seq.windowed 2
    |> Seq.mapAsCouples changeInMeasurements

let removeNoiseFromMeasurements (measurements : Measurement list) =
    measurements
    |> Seq.windowed 3
    |> Seq.map Seq.sum
