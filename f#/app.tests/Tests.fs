module Tests

open FsUnit.Xunit
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

        measurements
        |> mapMeasurementsToChanges
        |> countChanges MeasurementChange.Larger
        |> should equal 7

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

        measurements
        |> removeNoiseFromMeasurements
        |> mapMeasurementsToChanges
        |> countChanges MeasurementChange.Larger
        |> should equal 5

module Day02 =

    open App.Solutions.Day02

    [<Fact>]
    let ``day 02, puzzle 1`` () =
        let commands =
            [ "forward 5"
              "down 5"
              "forward 8"
              "up 3"
              "down 8"
              "forward 2" ]

        commands
        |> parseCommands
        |> Seq.fold Puzzle_1.move (0, 0)
        |> Puzzle_1.overallDistance
        |> should equal 150

    [<Fact>]
    let ``day 02, puzzle 2`` () =
        let commands =
            [ "forward 5"
              "down 5"
              "forward 8"
              "up 3"
              "down 8"
              "forward 2" ]

        commands
        |> parseCommands
        |> Seq.fold Puzzle_2.move (0, 0, 0)
        |> Puzzle_2.overallDistance
        |> should equal 900
