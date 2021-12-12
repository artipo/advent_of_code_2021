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

module Day04 =

    open App.Solutions.Day04

    [<Fact>]
    let ``day 04, puzzle 1`` () =
        let numbers =
            [ 7
              4
              9
              5
              11
              17
              23
              2
              0
              14
              21
              24
              10
              16
              13
              6
              15
              25
              12
              22
              18
              20
              8
              19
              3
              26
              1 ]

        let boards =
            [ [ [ 22; 13; 17; 11; 0 ]
                [ 8; 2; 23; 4; 24 ]
                [ 21; 9; 14; 16; 7 ]
                [ 6; 10; 3; 18; 5 ]
                [ 1; 12; 20; 15; 19 ] ]
              [ [ 3; 15; 0; 2; 22 ]
                [ 9; 18; 13; 17; 5 ]
                [ 19; 8; 7; 25; 23 ]
                [ 20; 11; 10; 24; 4 ]
                [ 14; 21; 16; 12; 6 ] ]
              [ [ 14; 21; 17; 24; 4 ]
                [ 10; 16; 15; 9; 19 ]
                [ 18; 8; 23; 26; 20 ]
                [ 22; 11; 13; 6; 5 ]
                [ 2; 0; 12; 3; 7 ] ] ]
            |> List.map initBoard

        simulateBingo numbers boards
        |> List.head
        ||> calculateScore
        |> should equal 4512

    [<Fact>]
    let ``day 04, puzzle 2`` () =
        let numbers =
            [ 7
              4
              9
              5
              11
              17
              23
              2
              0
              14
              21
              24
              10
              16
              13
              6
              15
              25
              12
              22
              18
              20
              8
              19
              3
              26
              1 ]

        let boards =
            [ [ [ 22; 13; 17; 11; 0 ]
                [ 8; 2; 23; 4; 24 ]
                [ 21; 9; 14; 16; 7 ]
                [ 6; 10; 3; 18; 5 ]
                [ 1; 12; 20; 15; 19 ] ]
              [ [ 3; 15; 0; 2; 22 ]
                [ 9; 18; 13; 17; 5 ]
                [ 19; 8; 7; 25; 23 ]
                [ 20; 11; 10; 24; 4 ]
                [ 14; 21; 16; 12; 6 ] ]
              [ [ 14; 21; 17; 24; 4 ]
                [ 10; 16; 15; 9; 19 ]
                [ 18; 8; 23; 26; 20 ]
                [ 22; 11; 13; 6; 5 ]
                [ 2; 0; 12; 3; 7 ] ] ]
            |> List.map initBoard

        simulateBingo numbers boards
        |> List.last
        ||> calculateScore
        |> should equal 1924

module Day05 =
    
    open App.Solutions.Day05
    
    [<Fact>]
    let ``day 05, puzzle 1`` () =
        let vents =
            [ "0,9 -> 5,9"
              "8,0 -> 0,8"
              "9,4 -> 3,4"
              "2,2 -> 2,1"
              "7,0 -> 7,4"
              "6,4 -> 2,0"
              "0,9 -> 2,9"
              "3,4 -> 1,4"
              "0,0 -> 8,8"
              "5,5 -> 8,2" ]
        
        vents
        |> List.map parseVentLine
        |> List.filter (not << isDiagonalVentLine)
        |> List.collect unfoldVentLine
        |> List.countBy id
        |> List.map snd
        |> List.filter (fun n -> n >= 2)
        |> List.length
        |> should equal 5
    
    [<Fact>]
    let ``day 05, puzzle 2`` () =
        let vents =
            [ "0,9 -> 5,9"
              "8,0 -> 0,8"
              "9,4 -> 3,4"
              "2,2 -> 2,1"
              "7,0 -> 7,4"
              "6,4 -> 2,0"
              "0,9 -> 2,9"
              "3,4 -> 1,4"
              "0,0 -> 8,8"
              "5,5 -> 8,2" ]
        
        vents
        |> List.map parseVentLine
        |> List.collect unfoldVentLine
        |> List.countBy id
        |> List.map snd
        |> List.filter (fun n -> n >= 2)
        |> List.length
        |> should equal 12

module Day06 =
    
    open App.Solutions.Day06
    
    [<Fact>]
    let ``day 06, puzzle 1`` () =
        let fishes =
            [| 3; 4; 3; 1; 2 |]
        
        fishes
        |> Array.map (simulateFish 80)
        |> Array.sum
        |> should equal 5934L
    
    [<Fact>]
    let ``day 06, puzzle 2`` () =
        let fishes =
            [| 3; 4; 3; 1; 2 |]
        
        fishes
        |> Array.map (simulateFish 256)
        |> Array.sum
        |> should equal 26984457539L

module Day07 =
    
    open App.Solutions.Day07
    
    [<Fact>]
    let ``day 07, puzzle 1`` () =
        let crabs =
            [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
        
        crabs
        |> searchBestPos (Puzzle_1.moveCrabsToPos crabs)
        |> snd
        |> should equal 37
    
    [<Fact>]
    let ``day 07, puzzle 2`` () =
        let crabs =
            [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
        
        crabs
        |> searchBestPos (Puzzle_2.moveCrabsToPos crabs)
        |> snd
        |> should equal 168

module Day08 =
    
    open App.Solutions.Day08
    
    [<Fact>]
    let ``day 08, puzzle 1`` () =
        let input =
            [
                "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
            ]
        
        input
        |> List.map parse
        |> List.map snd
        |> List.sumBy countEasyDigitInOutputValues
        |> should equal 26
    
    [<Fact>]
    let ``day 08, puzzle 2`` () =
        let input =
            [
                "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
            ]
        
        input
        |> List.map parse
        |> List.map solve
        |> List.sum
        |> should equal 61229

module Day09 =
    
    open App.Solutions.Day09
    
    [<Fact>]
    let ``day 09, puzzle 1`` () =
        let heightMap =
            [
                "2199943210"
                "3987894921"
                "9856789892"
                "8767896789"
                "9899965678"
            ]
        
        heightMap
        |> parse
        |> getLowPoints
        |> Seq.map toRiskLevel
        |> Seq.sum
        |> should equal 15
    
    [<Fact>]
    let ``day 09, puzzle 2`` () =
        let heightMap =
            [
                "2199943210"
                "3987894921"
                "9856789892"
                "8767896789"
                "9899965678"
            ]
        
        let map =
            heightMap
            |> parse
        
        getLowPoints map
        |> Seq.map (getBasin map)
        |> Seq.map Seq.length
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (fun s1 s2 -> s1 * s2)
        |> should equal 1134