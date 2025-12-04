module AoC2025.Day2

open AoC2025.Prelude

let getRanges fn =
    readAsText fn |> split ',' |> Seq.map ((split '-') >> fun [|a;b|] -> int64 a, int64 b)
    
let isRepeatedTwice i =
    let asString = $"{i}"
    let pair = asString |> Seq.splitInto 2 |> Seq.map System.String.Concat |> Array.ofSeq
    match pair with
    | [|a; b|] ->  a = b
    | _ -> false
    
    
let part1 fn () =
    let invalidIds = getRanges fn |> Seq.map (fun (a,b) -> ([a..b] |> Seq.filter isRepeatedTwice)) |> Seq.concat
    invalidIds |> Seq.sum
    
let part2 fn () = 0L