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

module Day03 =

    open App.Solutions.Day03

    [<Fact>]
    let ``day 03, puzzle 1`` () =
        let words =
            [ "00100"
              "11110"
              "10110"
              "10111"
              "10101"
              "01111"
              "00111"
              "11100"
              "10000"
              "11001"
              "00010"
              "01010" ]
        
        words
        |> List.map parseWord
        |> Puzzle_1.extractRate
        |> Puzzle_1.rateToPowerConsumption
        |> should equal 198
    
    [<Fact>]
    let ``day 03, puzzle 2`` () =
        let words =
            [ "00100"
              "11110"
              "10110"
              "10111"
              "10101"
              "01111"
              "00111"
              "11100"
              "10000"
              "11001"
              "00010"
              "01010" ]
            
        words
        |> List.map parseWord
        |> Puzzle_2.extractRates
        |> Puzzle_2.ratesToLifeSupportRating
        |> should equal 230