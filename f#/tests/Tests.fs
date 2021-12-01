module Tests

open Xunit

module Day01 =

    open App.Solutions.Day01
    
    [<Fact>]
    let ``day 01, puzzle 1`` () =
        let measurements =
            [ 199
              200
              208
              210
              200
              207
              240
              269
              260
              263 ]

        let result =
            measurements
            |> mapMeasurementsToChanges
            |> countChanges MeasurementChange.Larger

        Assert.Equal(7, result)
    
    [<Fact>]
    let ``day 01, puzzle 2`` () =
        let measurements =
            [ 199
              200
              208
              210
              200
              207
              240
              269
              260
              263 ]

        let result =
            measurements
            |> removeNoiseFromMeasurements
            |> mapMeasurementsToChanges
            |> countChanges MeasurementChange.Larger

        Assert.Equal(5, result)
