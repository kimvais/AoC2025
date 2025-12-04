module AoC2025.Day2

open AoC2025.Prelude

let getRanges fn =
    readAsText fn
    |> split ','
    |> Seq.map ((split '-') >> fun [| a; b |] -> int64 a, int64 b)

let isRepeatedTwice i =
    let asString = $"{i}"

    let pair =
        asString |> Seq.splitInto 2 |> Seq.map System.String.Concat |> Array.ofSeq

    match pair with
    | [| a; b |] -> a = b
    | _ -> false

let splitInto s n = Seq.splitInto n s

let groupsAreEqual s =
    let first = Seq.head s
    s |> Seq.forall ((=) first)

let isRepeatedAtLeastTwice i =
    let asString = $"{i}"
    let len = String.length asString

    seq { 2..len }
    |> Seq.filter (fun n -> len % n = 0)
    |> Seq.map (splitInto asString >> Seq.map System.String.Concat)
    |> Seq.exists groupsAreEqual

let part1 fn () =
    let invalidIds =
        getRanges fn
        |> Seq.map (fun (a, b) -> ([ a..b ] |> Seq.filter isRepeatedTwice))
        |> Seq.concat

    invalidIds |> Seq.sum

let part2 fn () =
    let invalidIds =
        getRanges fn
        |> Seq.map (fun (a, b) -> ([ a..b ] |> Seq.filter isRepeatedAtLeastTwice))
        |> Seq.concat

    invalidIds |> Seq.sum
