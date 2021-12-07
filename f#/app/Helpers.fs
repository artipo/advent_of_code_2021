[<AutoOpen>]
module App.Helpers

open System
open System.Collections.Generic
open System.IO

module Dictionary =
    let tryFind key (dict : Dictionary<_, _>) =
        match dict.TryGetValue(key) with
        | false, _ -> None
        | true, v -> Some v

module File =
    let readAllLines filePath = File.ReadAllLines(filePath)

module Int =
    let parseBinary b = Convert.ToInt32(b, 2)

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

module String =
    let join (sep : string) (seq : string list) =
        String.Join(sep, seq)