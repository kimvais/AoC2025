module AoC2025.Day3

open System
open AoC2025
open AoC2025.Prelude

let findPair s =
    let pairs = s |> Seq.pairwise
    let first = pairs |> Seq.maxBy fst |> fst
    let firstIndex = pairs |> Seq.findIndex (fun (a, _) -> a = first)
    let second = pairs |> Seq.skip firstIndex |> Seq.maxBy snd |> snd
    first * 10L + second

let rec findGroup acc n s=
    match Seq.length acc with
    | 12 -> acc |> List.map (fun n -> $"{n}") |> String.Concat |> int64
    | _ ->
        let first = s |> (Seq.windowed n) |> (Seq.maxBy Seq.head) |> Seq.head
        let index = s |> Seq.findIndex ((=) first)
        findGroup (acc @ [first]) (n - 1) (s |> Seq.skip (index + 1))
    
let part1 fn () =
    readInput fn |> Seq.map (Seq.map charToL >> findPair) |> Seq.sum
    
let part2 fn () = 
    readInput fn |> Seq.map (Seq.map charToL >> findGroup [] 12) |> Seq.sum