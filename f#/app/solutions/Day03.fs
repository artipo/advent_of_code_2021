module App.Solutions.Day03

open App.Helpers

// domain
type Bit =
    | Off
    | On

type Word = Bit list

let negBit bit =
    match bit with
    | Off -> On
    | On -> Off

let negWord word =
    word
    |> List.map negBit

let printBit bit =
    match bit with
    | Off -> "0"
    | On -> "1"

let printWord word =
    word
    |> List.map printBit
    |> String.join ""

let parseBit bit =
    match bit with
    | '0' -> Off
    | '1' -> On
    | x -> failwithf "Invalid bit value %A" x

let parseWord (word : string) =
    word
    |> Seq.map parseBit
    |> List.ofSeq

let wordToInt word =
    word
    |> printWord
    |> Int.parseBinary

module Puzzle_1 =
    let extractRate words =
        words
        |> List.transpose
        |> List.map (fun bs ->
            bs
            |> List.countBy id
            |> List.maxBy snd
            |> fst )
    
    let rateToPowerConsumption rate =
        let gamma =
            rate
            |> wordToInt
        let epsilon = 
            rate
            |> negWord
            |> wordToInt

        gamma * epsilon

module Puzzle_2 =
    let extractRate picker defaultBit words =
        
        /// filter words by recursive bit inspection
        let rec loop bit words =
            match words with
            | [ w ] -> w
            | _ ->
                let filteringBit =
                    words
                    |> List.transpose
                    |> List.item bit
                    |> List.countBy id
                    |> function
                        | [] -> failwith "Huston we have a problem"
                        | [bit, _] -> bit
                        | [_, count1; _, count2] when count1 = count2 -> defaultBit
                        | counts -> counts |> picker snd |> fst
                
                let filteredWords =
                    words
                    |> List.filter (fun w -> w.[bit] = filteringBit)
                
                loop (bit + 1) filteredWords
        
        loop 0 words

    let extractRates words =
        let oxygenGenerator =
            words
            |> extractRate List.maxBy On
            |> wordToInt
        
        let CO2scrubber =
            words
            |> extractRate List.minBy Off
            |> wordToInt
        
        oxygenGenerator, CO2scrubber
    
    let ratesToLifeSupportRating rates =
        let oxygenGenerator, CO2scrubber = rates
        oxygenGenerator * CO2scrubber

// wrappers
let solve_puzzle_1 () =
    File.readAllLines @"./inputs/day03.txt"
    |> Array.map parseWord
    |> Puzzle_1.extractRate
    |> Puzzle_1.rateToPowerConsumption
    |> printfn "Day03, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllLines @"./inputs/day03.txt"
    |> Array.map parseWord
    |> List.ofArray
    |> Puzzle_2.extractRates
    |> Puzzle_2.ratesToLifeSupportRating
    |> printfn "Day03, puzzle 2 -> result = %A"