[<AutoOpen>]
module App.Helpers

open System.IO

module File =
    let readAllLines filePath = File.ReadAllLines(filePath)

module Seq =
    let toCouples xss =
        xss
        |> Seq.map
            (fun xs ->
                let a = Seq.item 0 xs
                let b = Seq.item 1 xs
                a, b)

    let toTriplets xss =
        xss
        |> Seq.map
            (fun xs ->
                let a = Seq.item 0 xs
                let b = Seq.item 1 xs
                let c = Seq.item 2 xs
                a, b, c)
    
    let mapAsCouples mapper xss =
        xss
        |> toCouples
        |> Seq.map (fun (a, b) -> mapper a b)
    
    let mapAsTriplets mapper xss =
        xss
        |> toTriplets
        |> Seq.map (fun (a, b, c) -> mapper a b c)

